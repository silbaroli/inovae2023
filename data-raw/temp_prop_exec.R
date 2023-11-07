
library(dplyr)

ids <- tab_dispendios %>% 
  pull(id_prop) %>% 
  as.numeric()

unique(ids)
n_distinct(ids)


ids_agentes <- tab_proponente %>% 
  pull(id_agente_prop)

n_distinct(ids_agentes)

any(ids %in% ids_agentes)
sum(ids %in% ids_agentes)

sum(unique(ids) %in% ids_agentes)
ids[!ids %in% ids_agentes]

sum(!is.na(ids))

tab_dispendios %>% 
  filter(id_exec == id_prop) %>%
  collect() %>% 
  nrow()

tab_dispendios %>% 
  filter(id_exec != id_prop) %>%
  collect() %>% 
  nrow()

t <- tab_dispendios %>% 
  dplyr::left_join(
    tab_fomentador,
    by = c("id_formnt" = "id_formentador")
  ) %>%
  dplyr::left_join(tab_executor, by = c("id_exec" = "id_agente_exec")) %>% 
  dplyr::left_join(tab_proponente, by = c("id_prop" = "id_agente_prop")) %>% 
  dplyr::left_join(tab_categorias, by = c("id_cat2" = "id"))


collect(t) %>% View

tab_proponente %>% 
  filter(id_agente_prop %in% ids) %>%
  collect() %>% 
  View()

tab_proponente %>% 
  filter(id_agente_prop %in% ids) %>% 
  pull(id_agente_prop) %>% 
  n_distinct()
