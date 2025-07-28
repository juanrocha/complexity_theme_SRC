library(tidyverse)
library(network)
library(rscopus)
library(Matrix)
library(tictoc)
library(igraph)

# if working outside RStudio:
#setwd("~/Documents/Projects/complexity_theme_SRC")

dat <- read_csv("data/scopus-Sustainability_top-cited.csv") %>%
    janitor::clean_names()
dat
dat %>% names()
