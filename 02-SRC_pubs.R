library(tidyverse)
library(network)

dat <- read_csv("data/scopus-SRC_210428.csv") %>% 
    janitor::clean_names()

dat %>% names()


## Co-authorship network:
authors <- dat %>% 
    select(authors, doi) %>% 
    mutate(authors = str_split(authors, pattern = ", ")) %>% 
    unnest(cols = authors) %>% 
    mutate(presence = 1)

mat <- authors %>% unique() %>% 
    pivot_wider(names_from = authors, values_from = presence, values_fill = 0) %>% 
    select(-doi) %>%
    as.matrix()

mat <- t(mat) %*% (mat)


df_authors <- as_tibble(mat, rownames = "author") 

df_authors <- df_authors %>% 
    pivot_longer(2:last_col(), names_to = "co_author", values_to = "n_papers") %>% 
    filter(author != co_author, n_papers > 0)

net <- df_authors %>% 
    # filtering reduces from 232k links and >4.5k authors to 708 links shared by 233 authors
    filter(n_papers > 1) %>% 
    network(., directed = FALSE, matrix.type = "edgelist")

networkD3::simpleNetwork(
    Data= df_authors %>% filter(n_papers > 1) ,
    Source = "author", Target = "co_author", 
    zoom = FALSE, linkColour = "grey", nodeColour = "blue"
)

## free memory
rm(mat, authors)



