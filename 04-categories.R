library(tidyverse)


dat2 <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1UniT8_uodegRIEvf8hLzivrETNQHvqWypNIbLSoD7nY/edit#gid=1532000679", sheet =1 ) |> janitor::clean_names()


dat2 |> 
    mutate(method_category = str_remove_all(method_category, "\\n")) |> 
    mutate(method_category = str_to_lower(method_category)) |> 
    mutate(method_category = str_split(method_category, pattern = ",|;|/")) |> 
    unnest(cols = method_category) |> 
    mutate(method_category = str_trim(method_category, "both")) |> 
    filter(!is.na(method_category), method_category != "??", method_category != '') 
    #pull(method_category) |> unique() # needs cleaning
    ggplot(aes(method_category)) +
    geom_bar() +
    coord_flip()
