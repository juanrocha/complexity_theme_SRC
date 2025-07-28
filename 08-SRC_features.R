library(tidyverse)
library(pdftools)
library(fs)
library(tictoc)

pdfs <- dir_ls(path = "data/SRC virtual library/")
papers <- map(pdfs, pdf_text)

## One paper
papers[[24]] |> cat()

papers |> str_which("References") 
