library(tidyverse)
# library(sparklyr)
library(arrow)
library(tictoc)

library(tidytext)
library(tm)
library (topicmodels)
library(lda)

library(patchwork)
#sc <- spark_connect(master = "local")

# spark_read_parquet(
#     sc, name = "dat", repartition = 5,
#     path = "~/Downloads/works_concept_C66204764_sustainability_max_None.parquet")
# spark_disconnect(sc)

tic()
dat <- read_parquet(file = "~/Downloads/works_concept_C66204764_sustainability_max_None.parquet")
toc() # 265s elapsed: 4.5mins

names(dat)
dat |>  pull(has_fulltext) |> sum( na.rm = TRUE) # topics on full text possible for 104,336 papers

tic()
dat <- dat |> 
    filter(language == "en") |> #480k records
    filter(type_crossref %in% c("journal-article", "proceedings-article")) |> #353k + 20k records
    filter(is_retracted == FALSE) |> # 103 records TRUE
    select(id, doi, title, abstract = extracted_abstract, starts_with("primary_topic"), year = publication_year)
toc() # 30s



## clean data: extra words
too_words <- tibble(
    word = c("paper", "study", "aim", "aims", "objective", "purpose", "elsevier", 
        "john", "wiley", "sons", "springer", "verlag", "b.v", "abstract", 
        "press", "reserved", "rights", "author", "taylor", "francis", "elsevier",
        "i.e.", "e.g.", "publisher", "publishers", "published", "publishing", 
        "ii", "iv", "mdpi", "copyright", "journal", "auhors", "blackwell", "oxford", 
        "cambridge", "publisher", "university", "book", "volume", "gmbh")
)

tic()
dat <- dat |> # starts with 366,584 papers
    select(-starts_with("primary_topic"), -doi) |> # this might be useful later to compare topics
    filter(!is.na(abstract)) |> # removes 60,218
    mutate(l = str_length(abstract)) |> 
    filter(l>60) |> 
    mutate(unsuitable = case_when(
        str_detect(abstract, "\\[This corrects") ~ TRUE,
        str_detect(abstract, "\\[This retracts")  ~ TRUE,
        str_detect(abstract, "Abstract not available") ~ TRUE,
        str_detect(abstract, "The authors would like to make the following correction to the published paper") ~ TRUE,
        .default = FALSE
    )) |>
    filter(unsuitable == FALSE)  # removes 12
    # arrange((l)) |>
    # print(n=100)
    # slice(99) |> pull(abstract)
toc() # 5,2s; 306128 obs

#### document-term matrix ####
tic()
dtm <- dat %>% 
    select(id, abstract) %>%
    unnest_tokens(word, abstract) %>% 
    anti_join(stop_words) %>% 
    anti_join(too_words) %>%
    filter(!str_detect(word, "[:digit:]")) %>%
    group_by(id) %>%
    count(word, sort = TRUE) %>%
    # calculate the term frequency-inverse document frequency
    bind_tf_idf(document = id, term = word, n = n) |> #pull(tf_idf) |> summary()
    filter(tf_idf > 0.04) |>  # a value between median and mean
    cast_dtm(document = id, term = word, value = n) #, weighting = tm::weightTfIdf
toc() #152s

dtm # documents: 306 116, terms: 311 126

save(dtm, file = "data/dtm_sustainability.Rda")

# clean memory
rm(dat, too_words)
gc()

#### Chosing algorithm ####
SEED <- 2024
k <- 30
tic()
tset.TM <- list (
    VEM0 = LDA(dtm, k=k, control = list ( seed = SEED)),
    VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
    Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
    CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))
toc() # 61259.582; 17hrs 
sapply (tset.TM[1:3], slot, "alpha")



#Finding number of topics
k <- c(25,50,100,250, 500)
tic()
topicNumber.TM <- map(
    .x = k,
    .f = function(x) {
        LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
    })
toc() #  235081.722s 65hrs

save(tset.TM, topicNumber.TM, file = "data/sust_models_gibbs.RData")

#### Visualizations #####

df_topic_number <- tibble(
    topic_number = k,
    entropy = map_dbl (topicNumber.TM, function (x)
        mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z)))) # maximize Entropy
    ),
    alpha = map_dbl(topicNumber.TM, slot, "alpha"),
    log_lik = map_dbl(topicNumber.TM, logLik) #,  #maximize loglik
    #perplexity = map_dbl(topicNumber.TM, perplexity) #minimize perplexity
)

df_stats <- tibble(
    model = names(lapply(tset.TM, logLik)),
    loglik = as.numeric(lapply(tset.TM, logLik)), #maximize loglik
    entropy = lapply (tset.TM, function (x) 
        mean(apply(posterior(x)$topics,
                   1, function (z) - sum(z * log(z))))) %>% as.numeric()#maximize ENTROPY
    
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


## test
# run a PCA on the prob matrix
# or a hierarchical clustering on the 250 topics, extract the ranking (ordering)
# and use it for the x-axis
# beta: probability of words belonging to topics
# gamma: probability of topics explaining documents

load(file = "data/sust_models_gibbs.RData")
df_topics250 <- tidy(topicNumber.TM[[4]], matrix = "beta")

### documents:
df_documents <- tidy(topicNumber.TM[[4]], matrix = "gamma")

tic()
dat <- dat |> 
    left_join(
        df_documents |> 
            rename(id = document) |> 
            group_by(id) |> 
            slice_max(gamma) |> 
            slice_min(topic) |> 
            ungroup() 
    )
toc() # 31s

dat |> 
    left_join(df_order) |> 
    group_by(topic, year, rank) |> 
    summarize(gamma_m = median(gamma)) |> 
#    filter(year > 1984) |> 
    ggplot(aes(rank, year)) +
    geom_tile(aes(fill = gamma_m)) +
    scale_fill_viridis_c(option = "D", trans = "log") + 
    scale_y_reverse() +
    theme_light()


dat |> 
    left_join(df_order) |> 
    group_by(topic, year, rank) |> 
    summarize(gamma_m = median(gamma)) |> 
    filter(year > 1984) |> 
    ggplot(aes(rank, gamma_m)) +
    geom_line(aes(group = year, color = year), alpha = 0.24) +
    geom_hline(yintercept = 1/250, linetype = 2, color = "grey50") +
    scale_y_reverse() +
    scale_color_viridis_c(option = "C")



tic()
d <- dist(t(slot(topicNumber.TM[[4]], "gamma")))
toc() #1m

hc <- hclust(d, method = "ward.D2")
plot(hc)

df_order <- tibble(
    topic = hc$order,
    rank = 1:250
)


## PCA
tic()
pca <- prcomp(
    x = slot(topicNumber.TM[[4]], "gamma"),
    scale. = TRUE,
    tol = sqrt(.Machine$double.eps),
    rank. = 25
)
toc() #34s

plot(pca)

pca$sdev |> density() |> plot()
