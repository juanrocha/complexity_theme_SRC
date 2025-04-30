library(tidyverse)
library(tidytext)
library(fs)
library(pdftools)
library(tictoc)
library(topicmodels)
library(patchwork)


#### Prep dataset ####
fls <- dir_ls("data/SRC virtual library/")

x <- pdf_text(fls[1])

dat <- read_csv("~/Box Sync/Share&Delete/scopus_SRC_230315.csv") |> 
    janitor::clean_names()

dat <- dat |> select(eid, abstract, year) |> 
    filter(abstract != "[No abstract available]")

too_words <- tibble(
    word = c("paper", "study", "aim", "aims", "objective", "purpose", "elsevier", "taylor", "francis")
)

dtm <- dat |>
    unnest_tokens(word, abstract) |>  
    anti_join(stop_words) |> 
    anti_join(too_words) |> 
    filter(!str_detect(word, "[:digit:]")) |> 
    # mutate(word = textstem::lemmatize_words(word)) |> 
    group_by(eid) |> 
    count(word, sort = TRUE) |> 
    cast_dtm(document = eid, term = word, value = n)
dtm

# save(dtm, file = "data/dtm.RData")

SEED <- 2022

## Choosing best algorithm
k <- 10
tic()
tset.TM <- list (
    VEM0 = LDA(dtm, k=k, control = list (seed = SEED)),
    VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
    Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
    CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))
toc() # 207.349 sec elapsed
sapply (tset.TM[1:3], slot, "alpha")
map_dbl(tset.TM[-3], perplexity)
map_dbl (tset.TM, function (x) mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z))))) 
map_dbl(tset.TM, logLik)

## Gibbs beats them all

#Finding number of topics
k <- c(10,20,30,50,100)
tic()
topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
        LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })
toc() # 487.714 sec elapsed

# save(tset.TM, topicNumber.TM, file = "data/models_gibbs.RData")
load("data/models_gibbs.RData")
#### algorithm selection ####
df_stats <- tibble(
    model = names(lapply(tset.TM, logLik)),
    loglik = as.numeric(lapply(tset.TM, logLik)), #maximize loglik
    entropy = lapply (tset.TM, function (x) 
        mean(apply(posterior(x)$topics,
                   1, function (z) - sum(z * log(z))))) %>% as.numeric()#maximize ENTROPY
    
)

df_topic_number <- tibble(
    topic_number = k,
    entropy = map_dbl (topicNumber.TM, function (x)
        mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z)))) # maximize Entropy
    ),
    alpha = map_dbl(topicNumber.TM, slot, "alpha"),
    log_lik = map_dbl(topicNumber.TM, logLik) #,  #maximize loglik
    #perplexity = map_dbl(topicNumber.TM, perplexity) #minimize perplexity
)

perp <-  lapply(tset.TM[c(1,2,4)], perplexity)
perp$Gibbs <- NA
# pretty names:
df_stats$model <- c("VEM alpha", "VEM fixed", "Gibbs", "CTM")

g1 <- df_stats %>%
    add_column(perplexity = as.numeric(perp[c(1,2,4,3)])) %>% #minimize perplexity
    pivot_longer(cols = 2:4, names_to = "measure", values_to = "value") %>%
    ggplot(aes(model, value)) + 
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    facet_wrap(.~measure, scales = "free_y") +
    labs(x = "Algorithm", y = "Value", tag = "A") +
    theme_light(base_size = 8) + 
    theme(axis.text.x = element_text(size = 5))
g1

#### number of topics ####
g2 <- df_topic_number %>%
    # mutate(alpha_log = log10(alpha)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "measure", values_to = "value") %>%
    # filter(measure != "alpha") %>%
    ggplot(aes(as.factor(topic_number), value)) +
    geom_col() + 
    # scale_y_continuous(labels = scales::label_scientific) +
    labs(x = "Number of topics", y = "Value", tag = "B") +
    facet_wrap(.~measure, scales = "free", ncol = 4, nrow = 1) +
    theme_light(base_size = 8)

g1/g2


#### topics ####
df_topics30 <- tidy(topicNumber.TM[[3]], matrix = "beta")
g_30 <- df_topics30 %>%
    group_by(topic) %>%
    top_n(10, beta) %>% 
    ungroup() %>%
    arrange(topic, - beta) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta)) +
    geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
    coord_flip() + 
    scale_x_reordered() +
    labs( y = "Probability of word explaining the topic", x = "Word ranking") +
    facet_wrap(.~ topic, scales = "free_y", ncol = 6) +
    theme_light(base_size = 9) + 
    theme(axis.text.x = element_text(size = 5))


g_30


## oolong 