source("data-raw/tab_auxiliares.R")

library(dplyr)
library(ggplot2)
library(stringr)

tab_projetos <- tab_projetos %>% collect()

unique(tab_projetos$id_item) %>% sort()

n_distinct(tab_projetos$id_item)

tab_projetos %>% 
  distinct(id_item, id_projeto)

n_distinct(tab_projetos$id_projeto)

tab_projetos %>% 
  count(id_projeto, id_item) %>% 
  arrange(desc(n))

tab_projetos %>% 
  group_by(id_item) %>% 
  summarise(num_id_proj = n_distinct(id_projeto)) %>% 
  arrange(desc(num_id_proj)) %>% 
  View()

tab_dispendios <- collect(tab_dispendios)

tab <- tab_dispendios %>% 
  left_join(tab_projetos, by = "id_item")

# número de linhas (dispendios?)
nrow(tab)
n_distinct(tab$id_disp)

# numero de itens (o que é?)
n_distinct(tab$id_item)

# numero de projetos
n_distinct(tab$id_projeto)

# id_item que só possuem uma linha
tab %>% 
  group_by(id_item) %>% 
  filter(n() == 1) %>% 
  nrow()

itens <- tab %>% 
  group_by(id_item) %>% 
  filter(n() > 1) %>% 
  pull(id_item)

item <- itens[33]

tab %>% 
  filter(id_item == item) %>% 
  View()

tab %>% 
  filter(id_item == itens[33]) %>% 
  summarise(soma = sum(vlr), valor_total = valor_total[1])


tab %>% 
  distinct(id_disp, ano) %>% 
  nrow()


tab %>% 
  group_by(id_disp, ano) %>% 
  filter(n() > 1) %>% 
  View()



tab %>% 
  distinct(id_disp, id_item)

all(str_detect(tab$id_item, "PD-"))

tab$id_item[!(str_detect(tab$id_item, "PD-") | str_count(tab$id_item) %in% c(7, 8, 13))]



itens <- tab_projetos %>% 
  count(id_item, sort = TRUE) %>% 
  pull(id_item)

item <- itens[1]

tab_projetos %>% 
  filter(id_item == item) %>% 
  View()




