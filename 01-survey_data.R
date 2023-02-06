library(tidyverse)

dat <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1fZ2-oJjP4rPa8RrUTMUJLqBvC6nrajEWglP6elVhflA/edit#gid=1618401149",
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
    filter(Q4 == "Yes") %>%
    select(q = colm) %>%
    mutate(q = str_split(q, ","), id = row_number()) %>%
    unnest(q) %>% arrange(q) |> 
    mutate(q= as_factor(q)) %>%
    ggplot(aes(q)) +
    geom_bar() + coord_flip() +
    labs(x = 'Top 2 themes', y = "Number of people") +
    theme_light(base_size = 10)

ggsave(
    filename = "figures/years_SRC.png",
    plot = last_plot(),
    dpi = 500, width = 4, height = 3
)

## quantitative column
colm <- "Q14"

dat %>%
    filter(Q4 == "Yes") %>%
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
    filter(Q4 == "Yes") %>%
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
    mutate(responses = case_when(
        responses == "abm" ~ "agent-based modelling",
        responses == "agent-based modeling" ~ "agent-based modelling",
        responses == "asymptotic and transient dynamics" ~ "dynamic modelling",
        responses == "autoethnography" ~ "etnography",
        responses == "bacchi_ wpr" ~ "bacchi - wpr",
        responses == "behavioural exoeriments" ~ "behavioural experiments",
        responses == "bifurcation theory" ~ "bifurcation analysis",
        responses == "causal diagram" ~ "CLDs",
        responses == "causal loop diagram"  ~ "CLDs",
        responses == "causal loop diagrams" ~ "CLDs",
        responses == "causal loops" ~ "CLDs",
        responses == "clustering" ~ "cluster analysis",
        responses == "clustering analysis" ~ "cluster analysis",
        responses == "comparative analysis" ~ "QCA",
        responses == "controlled behavioural experiments" ~ "behavioural experiments",
        responses == "cross case (qualitative) comparison" ~ "QCA",
        responses == "dissimilarity methods" ~ "cluster analysis",
        responses == "dynamic modeling" ~ "dynamic modelling",
        responses == "dynamic optimization" ~ "dynamic modelling",
        responses == "dynamic systems analysis" ~ "dynamic modelling",
        responses == "dynamical systems modelling" ~ "dynamic modelling",
        responses == "empirical statistical analysis hi" ~ "statistics",
        responses == "experimentation7co-productions processes" ~ "co-production",
        responses == "expert focus groups" ~ "focus groups",
        responses == "expert interviews" ~ "interviews",
        responses == "indigenous methodologies embedded in place and relationships - specifically i use darug methodology buran murrira." ~ "buran murrira",
        responses == "interpretive interviews" ~ "interviews",
        responses == "interview and document analysis" ~ "interviews",
        responses == "interviews and surveys" ~ "interviews",
        responses == "interviews, scenario exercises" ~ "interviews",
        responses == "knowledge co-drations (workshops, interviews etc.)" ~ "interviews",
        responses == "knowledge elicitation" ~ "interviews",
        responses == "literature review and interdisciplinary triangulation" ~ "literature review",
        responses == "literature review and transdisciplinary triangulation" ~ "literature review",
        responses == "mathematical modelling" ~ "dynamic modelling",
        responses == "modelling (land use models)" ~ "landuse models",
        responses == "modelling (mostly cognitive or conceptual)"  ~ "conceptual models",
        responses == "network analysis" ~ "networks",
        responses == "network methods" ~ "networks",
        responses == "non linear regression" ~ "statistics",
        responses == "filed observations" ~ "observation",
        responses == "participant observation" ~ "participatory observation" ,
        responses == "policy (and other documensts) analysis" ~ "document analysis",
        responses == "policy document analysis" ~ "document analysis",
        responses == "process-tracing" ~ "process tracing",
        responses == "qca"~ "QCA",
        responses == "qualitative analysis" ~ "QCA",
        responses == "qualitative interviews and analysis" ~ "interviews",
        responses == "qualitative sociological interviews & surveys" ~ "interviews",
        responses == "resilience assessmen" ~ "resilience assessment",
        responses == "scenario development and analysis"  ~ "scenarios",
        responses == "scenario planning"   ~ "scenarios",
        responses == "seeds of ga"  ~ "scenarios",
        responses == "semi-structured expert interviews" ~ "interviews",
        responses == "semi-structured interviews"  ~ "interviews",
        responses == "ses modeling" ~ "Modelling",
        responses == "similarity measures , like hellinger distance" ~ "cluster analysis", 
        responses == "simulation model analysis" ~ "dynamic modelling",
        responses == "simulation modelling" ~ "dynamic modelling",
        responses == "stability analysis" ~ "dynamic modelling",
        responses == "stakeholder processes" ~ "co-production",
        responses == "statistical modelling" ~ "statistics",
        responses == "strucchange" ~ "statistics",
        responses == "survey" ~ "surveys",
        responses == "system dynamics" ~ "dynamic modelling",
        responses == "system modeling" ~ "dynamic modelling",
        responses == "systematic mapping" ~ "mapping",
        responses == "workshops and interviews" ~ "workshops",
        TRUE ~ responses
    )) |> add_count(responses)

df_methods |> #pull(responses) |> unique()
    mutate(responses = as_factor(responses),
           responses = fct_reorder(responses, n, mean, .desc = FALSE)) %>%
    filter(n > 1) |> 
    ggplot(aes(responses)) +
    geom_bar(aes(), alpha = 0.75) + coord_flip() + #fill = ses_feature
    scale_fill_hue("SES features") +
    labs(x="Methods", y = "Count") +
    theme_light(base_size = 8) +
    theme(legend.position = c(0.5, 0.25), legend.key.size = unit(0.25, "cm"))

ggsave(
    filename = "figures/methods.png",
    plot = last_plot(),
    dpi = 500, width = 5, height = 5
)


## range columns:theories
cbk %>% filter(str_detect(short_names, "Q136#1")) %>% print(n=57)

df1 <- dat %>%
    filter(Q4 == "Yes") %>%
    select(theory = "Q152#1_1_1", starts_with("Q136#1")) %>%
    pivot_longer(2:last_col(), names_to = "short_names", values_to = "responses") %>%
    left_join(cbk %>% filter(str_detect(short_names, "Q136#1")))

df2 <- dat %>%
    filter(Q4 == "Yes") %>%
    select(theory = "Q152#2_1_1", starts_with("Q136#2")) %>%
    pivot_longer(2:last_col(), names_to = "short_names", values_to = "responses") %>%
    left_join(cbk %>% filter(str_detect(short_names, "Q136#2")))

df3 <- dat %>%
    filter(Q4 == "Yes") %>%
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

df_theories <- df_theories |> #pull(theory) |> unique()
    mutate(theory = case_when(
        theory == "a bunch of theories about collective action and (co-)governance" ~ "collective action",
        theory == "acm framework, based ostrom's ses framework" ~ "ses framework (ostroms)",
        theory == "social-ecological systems framework" ~ "ses framework (ostroms)",
        theory == "common pool resource theory" ~ "CPR theory",
        theory == "the adaptive cycle and anarchy" ~ "Panarchy",
        theory == "ostrom's theory of collective action" ~ "ses framework (ostroms)",
        theory == "ostrom framework" ~ "ses framework (ostroms)",
        theory == "regime shift theory"  ~ "regime shifts",
        theory == "complex adaptive systems framework" ~ "CAS",
        theory == "organizing principles of cas" ~ "CAS",
        theory == "social network theories " ~ "network theory",
        theory == "3 dimensions of stewardship" ~ "stewardship",
        theory == "novelty in cas" ~ "CAS", 
        theory == "behavioural approach to rational choice theory" ~ "rational choice theory",
        theory == "biocultural diversity/approaches" ~ "biocultural diversity",
        theory == "cpr 'theory'" ~ "CPR theory",
        theory == "panarchy theory and its siblings" ~ "Panarchy",
        theory == "sense of place for ses" ~ "sense of place",
        theory == "resilience thinking" ~ "resilience",
        theory == "complex adaptive systems" ~ "CAS",
        theory == "plenty related to different ecosystem services and the mechanisms involved in their co-production." ~ "ecosystem services",
        TRUE ~ theory
    )) #|> pull(theory) |> unique()

df_theories %>%
    add_count(theory) %>%
    mutate(theory = fct_reorder(theory, n, mean, .desc = FALSE)) %>%
    ggplot(aes(theory)) +
    geom_bar(aes(fill = ses_feature)) + coord_flip()  +
    labs(x="Theories", y = "Count") +
    scale_fill_hue("SES feature") +
    theme_light(base_size = 8) +
    theme(legend.position = c(0.65, 0.25), legend.key.size = unit(0.25, "cm"))

ggsave(
    filename = "figures/theories.png",
    plot = last_plot(),
    dpi = 500, width = 5, height = 5
)

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
    mutate(link = 1, method = str_to_lower(method) ) %>%
    pivot_wider(id_cols = id, names_from = method, values_from = link, values_fill = 0) %>%
    select(-id) %>%
    as.matrix()

metmat <- t(mat) %*% mat # methods projection

net <- (metmat * upper.tri(metmat)) |> 
    as_tibble(., rownames = "from") %>%
    pivot_longer(2:last_col(), names_to = "to", values_to = "value") %>%
    filter(value > 0) %>%
    filter(from != to) %>%
    network(directed = FALSE, bipartite = FALSE, matrix.type = "edgelist", ignore.eval = FALSE)

quartz(width = 5, height = 5, pointsize = 8)

plot.network(
    net,
    vertex.col = alpha("purple", 0.5),
    label = network.vertex.names(net),
    label.cex= 0.65, label.col = "grey24",
    vertex.border = 0,
    usecurve=FALSE,
    vertex.cex= 1.5,
    label.pos= 5,
    displayisolates=T, pad = 0.5,
    edge.lwd = get.edge.attribute(net, "value")/3,
    edge.col = alpha("grey50", 0.1 + get.edge.attribute(net, "value")/12)
)


setwd("~/Documents/Projects/complexity_theme_SRC")

quartz.save(
    file = "figures/net_methods.png",
    width = 5, height = 5, pointsize = 10, dpi = 250,
    type = "png" )

library(ggnetwork)
n <- ggnetwork(net)

ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges() +
    geom_nodes()

mat <- df_net %>%
    filter(!is.na(src_author)) |>
    select(id = number, src_author) |>
    mutate(src_author = str_split(src_author, pattern = ", "),
            id = as.character(id)) |>
    unnest(col = src_author) |>
    mutate(src_author = str_remove_all(src_author, ","),
           src_author = str_trim(src_author, "both")) |>
    filter(src_author != ".") |>
    mutate(src_author = case_when(
        src_author == "Cornell S. E." ~ "Cornell S.E.",
        src_author == "Enqvist J.P." ~ "Enqvist J.",
        src_author == "Norstrom A.V." ~ "Norström A.V.",
        src_author == "Rocha J.C." ~ "Rocha J.",
        TRUE ~ src_author
    )) |>
    select(id, src_author) |>
    mutate(value = 1) |>
    pivot_wider(
        names_from = src_author, values_from = value, values_fill = 0) |>
    select(-id) |>
    as.matrix()

autmat <- t(mat) %*% mat

net <- (autmat * upper.tri(autmat)) |> 
    as_tibble(., rownames = "from") %>%
    pivot_longer(2:last_col(), names_to = "to", values_to = "value") %>%
    filter(value > 0) %>%
    filter(from != to) %>%
    network(directed = FALSE, bipartite = FALSE, matrix.type = "edgelist", ignore.eval = FALSE)



## slightly different settings for the co-author network
plot.network(
    net,
    vertex.col = alpha("salmon", 0.5),
    label = network.vertex.names(net),
    label.cex= 0.65, label.col = "grey24",
    vertex.border = 0,
    usecurve=FALSE,
    vertex.cex= 1,
    label.pos= 5,
    displayisolates=T, pad = 0.5,
    edge.lwd = get.edge.attribute(net, "value")/4,
    edge.col = alpha("grey50", 0.1 + get.edge.attribute(net, "value")/4)
)

quartz.save(
    file = "figures/net_authors_src.png",
    width = 5, height = 5, pointsize = 10, dpi = 250,
    type = "png" )

## qualitative column
colm <- "Q131"

dat %>%
    filter(Q4 == "Yes") %>%
    select(q = colm) %>%
    mutate(q = str_split(q, ","), id = row_number()) %>%
    unnest(q) %>%
    mutate(q = str_to_lower(q), q = str_trim(q, "both"), q = str_remove_all(q,"[:punct:]")) |> 
    #arrange(q) |> pull(q) |> unique()
    mutate(q = case_when(
        q == "law and policy analysis in relation to\na  biogeochemical cycles water cycle\nb and c business finance" ~ "various",
        q == "landscapes comparing different use types" ~ "landscape",
        q == "agricultural and conservation systems" ~ "agriculture",
        q == "urban\nagrofood\nsystems issues in relation to sustainability and resilience\nstewardship\nwater and natural resources" ~ "various",
        q == "relating to land use changes" ~ "landscape",
        q == "marine ses" ~ "marine",
        q == "all of them" ~ "various",
        q == "my work is about comparing across cases and scales" ~ "various",
        q == "can be applied to any types of systems" ~ "various",
        q == "network analysis is independent of system" ~ "various",
        q == "urban  ruralurban connections"  ~ "urban",
        q == "can be any marine" ~ "marine",
        q == "cpr in general" ~ "various",
        q == "production systems food and forests"~"urban",
        q == "urban systems" ~ "urban",
        q == "periurban areas" ~ "urban",
        q == "food" ~ "food systems",
        q == "forest" ~ "forests", q == "freshwater" ~ "freshwater systems",
        q == "agricultural" ~ "agriculture",
        TRUE ~ q)) |> 
    select(-id) |> group_by(q) |> 
    summarize(n = n()) |> 
    arrange(n) |> 
    mutate(q = as_factor(q)) %>%
    ggplot(aes(q,n)) +
    geom_col() + coord_flip() +
    labs(x = 'Type of SES', y = "Number of people") +
    theme_light(base_size = 10)

ggsave(
    filename = "figures/ses_type.png",
    plot = last_plot(),
    dpi = 500, width = 5, height = 5
)


## backgrounds
colm <- "Q10" # 10 and 12

a <- dat %>%
    filter(Q4 == "Yes") %>%
    select(q = colm) %>%
    mutate(q = str_split(q, ","), id = row_number()) %>%
    unnest(q) %>%  
    group_by(q) |> summarize(n=n()) |> 
    arrange(n) |>
    mutate(q= as_factor(q)) %>%
    ggplot(aes(q,n)) +
    geom_col() + coord_flip() +
    labs(x = 'Barchelors', y = "Number of people") +
    theme_light(base_size = 6)

b <- dat %>%
    filter(Q4 == "Yes") %>%
    select(q = colm) %>%
    mutate(q = str_split(q, ","), id = row_number()) %>%
    unnest(q) %>%  
    group_by(q) |> summarize(n=n()) |> 
    arrange(n) |>
    mutate(q= as_factor(q)) %>%
    ggplot(aes(q,n)) +
    geom_col() + coord_flip() +
    labs(x = 'Masters', y = "Number of people") +
    theme_light(base_size = 6)

c <- dat %>%
    filter(Q4 == "Yes") %>%
    select(q = colm) %>%
    mutate(q = str_split(q, ","), id = row_number()) %>%
    unnest(q) %>%  
    group_by(q) |> summarize(n=n()) |> 
    arrange(n) |>
    mutate(q= as_factor(q)) %>%
    ggplot(aes(q,n)) +
    geom_col() + coord_flip() +
    labs(x = 'PhD', y = "Number of people") +
    theme_light(base_size = 6)

library(patchwork)
a+b+c

ggsave(
    filename = "figures/backgrounds.png",
    plot = last_plot(),
    dpi = 500, width = 6, height = 2
)
