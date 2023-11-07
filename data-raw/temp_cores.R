library(dplyr)
library(echarts4r)

tab <- tab_projetos %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(tab_categorias, by = c("id_cat2" = "id")) %>% 
  dplyr::left_join(tab_natureza, by = c("ntz_finan" = "id"))

tab %>%
  dplyr::collect() %>% 
  dplyr::group_by(duracao_anos) %>% 
  dplyr::summarise(
    valor_total = sum(valor_total, na.rm = TRUE),
    freq = dplyr::n()
  ) %>% 
  dplyr::mutate(
    p = freq/sum(freq),
    duracao_anos = as.numeric(duracao_anos)
  ) %>% 
  dplyr::mutate(
    nm_categoria_nv1 = "Todos os projetos"
  )

X <- tab %>%
  dplyr::collect() %>% 
  dplyr::group_by(duracao_anos, nm_categoria_nv1) %>% 
  dplyr::summarise(
    valor_total = sum(valor_total, na.rm = TRUE),
    freq = dplyr::n()
  ) %>% 
  dplyr::filter(!is.na(duracao_anos)) %>%
  dplyr::mutate(duracao_anos = factor(duracao_anos, 0:13)) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(nm_categoria_nv1, duracao_anos, fill = list(freq = 0, valor_total = 0)) %>%
  dplyr::group_by(nm_categoria_nv1)

X %>% 
  dplyr::mutate(
    total = sum(freq),
    p = freq/total
  ) %>% 
  dplyr::mutate(
    duracao_anos = as.numeric(duracao_anos)
  ) %>% 
  ungroup() %>% 
  mutate(
    duracao_anos = as.character(duracao_anos),
    nm_categoria_nv1 = as.character(nm_categoria_nv1)
  ) %>% 
  filter(!is.na(nm_categoria_nv1), !is.na(duracao_anos)) %>% 
  e_charts(duracao_anos) %>% 
  e_heatmap(nm_categoria_nv1, p) %>% 
  e_visual_map(p, inRange = list(color = cores_epe()[1:2])) 
