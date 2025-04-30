library(tidyverse)
library(pdftools)
library(fs)
library(tictoc)

pdfs <- dir_ls(path = "data/SRC virtual library/")
tic()
papers <- map(pdfs, pdf_text)
toc()

papers[[1]]


## One paper
papers[[24]] |> cat()

## make each one string, currently one string per page
tic()
papers <- map(
    papers,
    str_flatten
)
toc() # 0.3s

papers |> str_which("References") 

#### Functions ####
source("04-categories.R")

## remove bibliography
remove_refs <- function(pdf){
    # find the position where the bibliography starts
     x <- pdf |> 
        str_locate_all(pattern = "References|Bibliography|Bibliografía|REFERENCES|BIBLIOGRAPHY")
     x <- as_tibble(x[[1]])
     
    bib_start <- ifelse(nrow(x) == 0, NA, x |> pull(start) |> last())
       
    # remove all content afterwards
    if(!is.na(bib_start))
        pdf <- pdf |> str_sub(start = 1L, end = bib_start) 

    return(pdf)
}

## test remove refs: working
papers[[60]] |>
    remove_refs() |> 
    str_length() # working

## detect theories
detect_theory <- function(pdf){
    x <- pdf |> 
        str_split(pattern = "\\. ") |> unlist() |> #class()
        str_subset(pattern = "theory") |> 
        str_flatten(collapse = " | ")
    return(x)
}

## test detect theories:
papers[[5]] |> detect_theory() # working

## coding qualitative features
code_pdf <- function(pdf) {
    df_res <- tibble(
        places = pdf |> 
            str_extract_all(pattern = df_place$name) |> 
            unlist() |> unique() |> 
            str_flatten(collapse = "|"),
        methods = pdf |> 
            str_extract_all(pattern = df_methods$name) |> 
            unlist() |> unique() |> 
            str_flatten(collapse = "|"),
        method_category = df_methods$category[pdf |> str_detect(pattern = df_methods$name)] |> 
            unlist() |> unique() |> str_flatten(collapse = "|"),
        ses = pdf |> 
            str_extract_all(pattern = df_ses$name) |> 
            unlist() |> unique() |> 
            str_flatten(collapse = "|"),
        ses_category = df_ses$category[pdf |> str_detect(pattern = df_ses$name)]|> 
            unlist() |> unique() |>  str_flatten(collapse = "|"),
        ses_frmwks = pdf |> 
            str_extract_all(pattern = ses_frmwks$name) |> 
            unlist() |> unique() |> 
            str_flatten(collapse = "|"),
        # ses_frmwk_cat = getElement(ses_frmwks,"category")[str_detect(pdf, pattern = ses_frmwks$name )] |>
        #     str_flatten(collapse = "|"),
        urban = pdf |> 
            str_extract_all(pattern = df_urban$name) |> 
            unlist() |> unique() |> 
            str_flatten(collapse = "|"),
        urban_category = df_urban$category[pdf |> str_detect(pattern = df_urban$name)] |> 
            unlist() |> unique() |> str_flatten(collapse = "|")
    )
    return(df_res)
}


code_pdf(papers[[40]])

ses_frmwks$category [papers[[10]] |> str_detect(pattern = ses_frmwks$name)] |> 
    unlist() |> str_flatten(collapse = "|")


df_methods$category[papers[[4]] |> str_detect(pattern = df_methods$name)] |> 
    unlist() |> str_flatten(collapse = "|")

#### Run for all SRC papers ####

tic()
papers <- papers |> map(remove_refs)
toc() # 1.1s

tic()
theories <- papers |> map(detect_theory)
toc()

tic()
df_features <- map_df(papers, code_pdf)
toc() #11min

df_features <- df_features |> 
    add_column(paper = names(theories)) |> 
    add_column(theories = unlist(theories))

write_csv(df_features, file = "data/src_pubs_categories.csv")
