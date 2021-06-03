## Networks from CLDs for Josefa internship
## Juan Rocha, 210603
library(tidyverse)
library(network)

## read data and make standard names
dat <- readxl::read_xlsx(
    ## you should change the path here to match the location in your computer
    path = "~/Downloads/Cases RSDB.xlsx",
    sheet = 6,
    skip = 1) %>% 
    janitor::clean_names()

## create the network
net <- dat %>% 
    mutate(polarity = case_when( polarity == "＋" ~  1, polarity == "—" ~ -1)) %>% 
    mutate(color = case_when(polarity == 1 ~ "dodgerblue", polarity == -1 ~ "orange")) %>% 
    filter(!is.na(head)) %>% 
    select(head, tail = tall, polarity, color) %>% 
    network(directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", 
            ignore.eval = FALSE)

## visualize the network using the network plotting method from the library "network"
plot.network(
    net, 
    vertex.col = alpha("purple", 0.5), 
    label = network.vertex.names(net),
    label.cex= 0.5, label.col = "grey24",
    main = "Josefa's CLD", col.main = "grey11", cex.main = 1.25,
    vertex.border = 0,
    usecurve=T,
    vertex.cex= 2, 
    label.pos= 5,
    edge.col= alpha(net %e% 'color', 1),
    edge.lwd =  0.05, 
    edge.curve = 0.01,
    displayisolates=T, pad = 0.5
)


## An example of writing a function:

plotnet <- function(x, ...){ # x is the network object
    plot.network(
        x, 
        vertex.col = alpha("purple", 0.5), 
        # note that the function expects a network, but inside the function 
        # it is called "x". So we use that name instead below
        label = network.vertex.names(x),
        label.cex= 0.5, label.col = "grey24",
        vertex.border = 0,
        usecurve=T,
        vertex.cex= 2, 
        label.pos= 5,
        edge.col= alpha(x %e% 'color', 1),
        edge.lwd =  0.05, 
        edge.curve = 0.01,
        displayisolates=T, pad = 0.5
    )
}

## Now we get a similar result in one line of code:
plotnet(net)
