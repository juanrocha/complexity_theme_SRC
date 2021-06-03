library(tidyverse)

dat <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/13hKDXZsA5Juy0qcKeGkj99z6nIPkmspRNlCl_mqA2ms/edit?usp=sharing",
    sheet = 1, skip = 2, col_types = "c")

codebook <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DCfvdeFEA_U3rU6rSaHjUm23UrtI8ysoDgPXRnl_4io/edit?usp=sharing")

load("data/survey_data.RData")

cbk <- codebook %>% 
    filter(!is.na(StartDate)) %>%  
    pivot_longer(everything(), 
                 names_to = "short_names", values_to = "long_names")


names(dat) %in% names(codebook)

extract_codes <- function(question) {
    df_book <- codebook %>% 
        select(question)
    df_book <- df_book[-1, ]
    df_book <- df_book %>% 
        separate(question, sep = "  ", into = c("res", "id")) %>%
        filter(!is.na(res)) %>% 
        mutate(id = str_remove_all(id, pattern = "[:punct:]"))
    return(df_book)
}


extract_codes("Q19")

## qualitative column
colm <- "Q19"

dat %>% 
    filter(Q5 == "Yes") %>% 
    select(q = colm) %>% 
    mutate(q = str_split(q, ","), id = row_number()) %>% 
    unnest(q) %>% 
    mutate(q= as_factor(q)) %>% 
    ggplot(aes(q)) +
    geom_bar() + coord_flip() +
    labs(x = 'Themes', y = "Number of people") +
    theme_light(base_size = 10)

ggsave(
    filename = "figures/years_SRC.png",
    plot = last_plot(),
    dpi = 500, width = 4, height = 3
)

## quantitative column
colm <- "Q14"

dat %>% 
    filter(Q5 == "Yes") %>% 
    select(q = colm) %>% 
    mutate(q = str_split(q, ","), id = row_number()) %>% 
    unnest(q) %>% 
    # mutate(q= str_remove_all(q, "[:alpha:]|[:blank:]"),
    #        q = str_trim(q, "both"),
    #        q = as.numeric(q), 
    #        q = case_when(q == 2012 ~ 2021-2021, TRUE ~ q)) %>% 
    ggplot(aes(q)) +
    geom_bar() + coord_flip() +
    labs(x = 'Years at SRC or Beijer', y = "Number of people") +
    theme_light(base_size = 10)

## range columns: methods
cbk %>% filter(str_detect(short_names, "Q20")) %>% print(n=51)

df_methods <- dat %>% 
    filter(Q5 == "Yes") %>% 
    select(starts_with("Q20")) %>% 
    pivot_longer(everything(), names_to = "short_names", values_to = "responses") %>% 
    left_join(
        cbk %>% filter(str_detect(short_names, "Q20"))
    ) %>% 
    filter(!is.na(responses)) %>%
    mutate(ses_feature = str_remove_all(long_names, pattern = "method (A|B|C) and "),
           ses_feature = str_remove_all(ses_feature, pattern = "[:punct:]|[:digit:]"),
           ses_feature = str_trim(ses_feature, "both")) %>% 
    filter(responses != "A", responses !="B", responses !="C", 
           responses != "yes", responses != "d") %>% 
    mutate(responses = str_to_lower(responses)) %>% 
    add_count(responses)


df_methods %>%
    mutate(responses = as_factor(responses),
           responses = fct_reorder(responses, n, mean, .desc = FALSE)) %>% 
    ggplot(aes(responses)) +
    geom_bar(aes(fill = ses_feature)) + coord_flip() +
    labs(x="Methods", y = "Count") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.75, 0.5), legend.key.size = unit(0.25, "cm"))

ggsave(
    filename = "figures/theories.png",
    plot = last_plot(),
    dpi = 500, width = 5, height = 5
)


## range columns:theories
cbk %>% filter(str_detect(short_names, "Q136#1")) %>% print(n=57)

df1 <- dat %>% 
    filter(Q5 == "Yes") %>% 
    select(theory = "Q152#1_1_1", starts_with("Q136#1")) %>% 
    pivot_longer(2:last_col(), names_to = "short_names", values_to = "responses") %>% 
    left_join(cbk %>% filter(str_detect(short_names, "Q136#1"))) 

df2 <- dat %>% 
    filter(Q5 == "Yes") %>% 
    select(theory = "Q152#2_1_1", starts_with("Q136#2")) %>% 
    pivot_longer(2:last_col(), names_to = "short_names", values_to = "responses") %>% 
    left_join(cbk %>% filter(str_detect(short_names, "Q136#2"))) 

df3 <- dat %>% 
    filter(Q5 == "Yes") %>% 
    select(theory = "Q152#3_1_1", starts_with("Q136#3")) %>% 
    pivot_longer(2:last_col(), names_to = "short_names", values_to = "responses") %>% 
    left_join(cbk %>% filter(str_detect(short_names, "Q136#3"))) 

df_theories <- bind_rows(df1, df2, df3) %>% 
    filter(!is.na(responses)) %>% 
    mutate(ses_feature = str_remove_all(long_names, pattern = "Theory\\/ framework (A|B|C) -"),
           ses_feature = str_remove_all(ses_feature, pattern = "[:punct:]|[:digit:]"),
           ses_feature = str_trim(ses_feature, "both"))  %>% 
    filter(!ses_feature %in% c(
        "Additional SE feature or process A",
        "Additional SE feature or process B", 
        "Theory framework A text", 
        "Theory framework B text" , 
        "Theory framework C text", 
        "Theory framework B Exploring uncertainty" ,
        "Theory framework C Exploring uncertainty" )) %>% 
    mutate(theory = str_replace_all(
        theory, 
        pattern = "Social network theories e.g. strength of weak ties theory and structural hole theory, social influence theories",
        replacement = "Social network theories ")) %>% 
    mutate(theory = str_to_lower(theory))


df_theories %>% 
    add_count(theory) %>% 
    mutate(theory = fct_reorder(theory, n, mean, .desc = FALSE)) %>% 
    ggplot(aes(theory)) +
    geom_bar(aes(fill = ses_feature)) + coord_flip()  +
    labs(x="Theories", y = "Count") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.75, 0.5), legend.key.size = unit(0.25, "cm"))



save(dat, codebook, file = "data/survey_data.RData")


#### Networks ####
library(network)
df_net <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1YerHWDrxmP-HEY3ump57RYCKVI6g-it-RYxSMbmrHzU/edit#gid=1532000679") %>% janitor::clean_names()

df_src <- df_net %>% 
    filter(!is.na(src_author)) %>% 
    select(-method_category_combination, -method) %>% 
    select(id = number, src_author, starts_with("method")) %>% 
    pivot_longer(cols = starts_with("method_category"), names_to = "names", values_to = "method") %>% 
    select(-names) %>% 
    filter(method != "N/A", !is.na(method))

mat <- df_src %>% 
    select(-src_author) %>% 
    unique() %>% 
    mutate(link = 1) %>% 
    pivot_wider(id_cols = id, names_from = method, values_from = link, values_fill = 0) %>% 
    select(-id) %>% 
    as.matrix()

metmat <- t(mat) %*% mat # methods projection

net <- metmat %>% 
    as_tibble(., rownames = "from") %>% 
    pivot_longer(2:last_col(), names_to = "to", values_to = "value") %>% 
    filter(value > 0) %>% 
    filter(from != to) %>% 
    network(directed = FALSE, bipartite = FALSE, matrix.type = "edgelist", ignore.eval = FALSE)

#quartz(width = 4, height = 4, pointsize = 6, dpi = 200)

plot.network(
    net,
    label = network.vertex.names(net),
    vertex.col = "orange",
    edge.lwd = get.edge.attribute(net, "value")/2,
    edge.col = "grey50",
    label.cex = 0.5
)

GGally::ggnet(net)

#quartz.save(file = "figures/net_methods.png",width = 4, height = 4, pointsize = 6, dpi = 200 )
