library(tidyverse)
library(network)
library(rscopus)
library(Matrix)
library(tictoc)
library(igraph)

# if working outside RStudio:
#setwd("~/Documents/Projects/complexity_theme_SRC")

dat <- read_csv("data/scopus-SRC_221102.csv") %>%
    janitor::clean_names()
dat
dat %>% names()

#### extract references ####
refs <- dat |> 
    select(eid, references) |> 
    mutate(references = str_split(references, pattern = "; ")) |> 
    unnest(references) |># pull(references) |> unique() |> length()
    mutate(pos = str_locate(references, "\\([:digit:]{4}\\)")) |> 
    mutate(references = str_sub(references, start = 1L, end = pos[,1]-1)) |> 
    select(-pos) |> 
    #arrange((references))
    ## ., is the sep for authors, but the last one is often use before page range
    ## and it was deleted by removing everything after year in previous step
    mutate(pos = stringi::stri_locate(references, regex = "\\.,", mode = "last")) |> 
    mutate(references = str_sub(references, start = pos[,2]+1, end = -1L)) |> 
    select(-pos) |> 
    mutate(references = str_trim(references, side = "both"))

refs

# refs$references[5]
# refs$pos[5][[1]]

refs$references |> unique() |> length() # reduced to 69k


## Success: top 100 papers commonly cited by SRC publications:
refs |> 
    filter(references != '') |> 
    mutate(references = str_to_lower(references)) |> 
    select(-eid) |> group_by(references) |> 
    add_count() |> unique() |> 
    arrange(desc(n)) |> 
    print(n = 100)

## network
tic()
m <- refs |> 
    filter(references != "") |> 
    mutate(count = 1) |> 
    pivot_wider(values_from = count, names_from = references, values_fill = 0, values_fn = mean) |>
    select(-eid) |> 
    as.matrix() |> # this is a 954Mb object
    Matrix() # this is a 12.6Mb object
toc() #in both cases it takes 5s
dim(m) # 1775 papers and 69k references

## one-mode: maybe do this in Julia, very slow in R. Or make it into an igraph network before calculating stuff. 
## J230201: slow using matrix() but fast with Matrix(). No need of Julia anymore
#one <-  m %*% t(m) #one mode on SRC papers
one <- t(m) %*% m # one mode on references.

net <- graph_from_adjacency_matrix(one)
tic()
stats <- tibble(
    bet = betweenness(net, directed = TRUE, cutoff = 5),
    deg = degree(net, mode = "in")
)
toc() #40971.375s ~ 11hrs: save it!

stats |> 
    ggplot(aes(deg, bet)) +
    geom_point() 

stats$paper <- colnames(one) # Alternatively V(net)$name

stats |> 
    arrange(desc(deg)) |> 
    select(paper) |> 
    print(n=25)

save(stats, file = "data/cited_papers_stats.RData")
    


### To do:
### Standardize to lowercase so different title formats merge.


# small test to remember
# x <- matrix(rbinom(n = 50, prob = 0.1, size =1), nrow = 5, ncol = 10 )
# x %*% t(x)

#### Co-authorship network: ####
#### 
authors <- dat %>%
    select(authors, doi) %>%
    mutate(authors = str_split(authors, pattern = ", ")) %>%
    unnest(cols = authors) %>%
    mutate(presence = 1)

authors |> pull(authors) |> unique() |> sort()

mat <- authors %>% unique() %>%
    pivot_wider(names_from = authors, values_from = presence, values_fill = 0) %>%
    select(-doi) %>%
    as.matrix()

mat <- t(mat) %*% (mat)


df_authors <- as_tibble(
    (mat * upper.tri(mat)) , 
    rownames = "author")

df_authors <- df_authors %>%
    pivot_longer(2:last_col(), names_to = "co_author", values_to = "n_papers") %>%
    filter(author != co_author, n_papers > 0)

#all((df_authors$author |> unique()) %in% df_authors$co_author |> unique())

net <- df_authors %>%
    # filtering reduces from 232k links and >4.5k authors to 708 links shared by 233 authors
    #filter(n_papers > 1) %>%
    network(directed = FALSE, bipartite = FALSE, matrix.type = "edgelist", ignore.eval = FALSE)

tic()
df_stats <- tibble(
    name = network.vertex.names(net),
    deg = sna::degree(net, gmode = "graph"),
    bet = sna::betweenness(net)
)
toc()

df_stats |> arrange(desc(deg))


df_stats |> 
    mutate(label = ifelse(deg>300 | bet> 1000000, name, NA)) |> 
    ggplot(aes(bet, deg)) +
    geom_point(size = 2, alpha = 0.4) + 
    ggrepel::geom_text_repel(aes(label = label), size = 2) +
    labs(x = "Betweenness centrality", y = "Degree centrality") +
    theme_light(base_size = 6)

ggsave(filename = "figures/SRC-net_stats.png",
       plot = last_plot(), device = "png", width = 3, height = 3, bg = "white")

networkD3::simpleNetwork(
    Data= df_authors %>% filter(n_papers > 1) ,
    Source = "author", Target = "co_author",
    zoom = FALSE, linkColour = "grey", nodeColour = "blue"
)



## free memory
rm(mat, authors)

quartz(width = 7, height = 7, pointsize = 7)

plot.network(
    net,
    vertex.col = alpha("purple", 0.5),
    vertex.border = 0,
    usecurve=FALSE,
    vertex.cex= 0.5,
    label.pos= 5,
    displayisolates=T, pad = 0.5,
    edge.lwd = (0.05 + get.edge.attribute(net, "n_papers")/23),
    edge.col = alpha("grey50", 0.05 + get.edge.attribute(net, "n_papers")/23)
)

quartz.save(
    file = "figures/net_authors_full.png",
    width = 7, height = 7, pointsize = 7,
    type = "png" )



#### test ####
## try this at SU campus, not working at SRC
x <- dat$doi[1]

res <- citation_retrieval(doi = x) # VPN working, I can access abstracts but not citations
# Error:  "Requestor configuration settings insufficient for access to this resource."
res
