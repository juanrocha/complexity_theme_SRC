library(tidyverse)
library(network)
library(sna)
library(igraph)
library(intergraph)
library(tictoc)

dat <- read_csv("data/scopus-SRC_221102.csv") %>%
    janitor::clean_names()
dat
dat %>% names()

## split authors
edgelist <- dat |> 
    mutate(author = str_split(authors, pattern = ", ")) |> 
    select(author, title) |> 
    unnest(author) |> 
    unique()

## bipartite network:
## option 1
bip1 <- network(edgelist, bipartite = TRUE, directed = FALSE)
bip1

## option 2: takes time but makes 100% sure the matrix comes on the same order 
## as the network
mat <- as.sociomatrix(bip1) # take some time
one <- mat %*% t(mat)
# one[one>0]

## option 3: way faster but be aware of pontential different ordering.
mat <- edgelist |> 
    mutate(presence = 1) |> 
    pivot_wider(names_from = title, values_from = presence, values_fill = 0) |> 
    select(-author) |> 
    as.matrix()

## if you are running out of memory, you can clean up some space
 rm(mat, bip1, dat)

## Reduce the network: if necessary you can reduce the matrix to people wiht 
## higher number of connections, e.g. higher than the median 20.
# colSums(one > 0) |> summary()


## Now with the `one` object we can create and manipulate a new network
net <- network(one > 0, directed = FALSE)
net
network.density(net)
isSymmetric(one) # TRUE: undirected

## Add some attributes for visualization
net %v% "deg" <- sna::degree(net, gmode = "graph")
tic()
net %v% "bet" <- igraph::betweenness(
    asIgraph(net), directed = FALSE, cutoff = 6) 
toc() #10s in igraph

net %e% "papers" <- one

net2 <- asIgraph(net)
clus <- cluster_infomap(net2)

### this takes several minutes
# plot.network(
#     net,
#     attrname = "papers",
#     label = NULL,
#     threshold = 5,
#     usearrows = FALSE,
#     displayisolates = FALSE,
#     vertex.cex = log1p(net %v% "deg")
# )

# A perhaps faster way to check the key individuals
dat <- tibble(
    author = network.vertex.names(net),
    degree = net %v% "deg",
    bet = net %v% "bet",
    group = membership(clus) |> as.numeric()
)

dat |> 
    arrange(desc(bet)) |> 
    print(n=100)

dat |> 
    ggplot(aes(degree, bet)) +
    geom_point(aes(color = group), alpha = 0.5) +
    scale_y_log10() +
    scico::scale_color_scico(palette = "roma")

dat |> 
    filter(group == 47) |> print(n=26)

edgelist |> 
    group_by(title) |> 
    summarize(n_authors = n()) |> 
    arrange(desc(n_authors))
