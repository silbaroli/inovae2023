tab_dispendios %>%
  dplyr::left_join(tab_fomentador, by = c("id_formnt" = "id_formentador")) %>%
  dplyr::group_by(nme_form) %>%
  dplyr::summarise(val = sum(as.numeric(vlr), na.rm = TRUE)) %>% 
  dplyr::collect() %>% View

tab_dispendios %>%
  dplyr::left_join(tab_fomentador, by = c("id_formnt" = "id_formentador")) %>%
  dplyr::group_by(ano) %>%
  dplyr::summarise(val = sum(as.numeric(vlr), na.rm = TRUE)/10^9) %>%
  dplyr::collect() %>% 
  with(sum(val))

tab_dispendios %>%
  left_join(tab_fomentador2, by = c("id_formnt" = "id_formentador")) %>%
  group_by(ano) %>%
  summarise(val = sum(as.numeric(vlr), na.rm = TRUE)/10^9) %>% with(sum(val))

tab_dispendios$vlr %>% as.numeric() %>% sum(na.rm=TRUE)

tab_dispendios %>% 
  left_join(tab_fomentador, by = c("id_formnt" = "id_formentador")) %>%
  left_join(tab_projetos, by = c("id_item" = "id_item")) %>%
  select(id_disp, id_item, id_formnt, nme_form, `título`, vlr) %>% 
  dplyr::filter(stringr::str_detect(vlr, ",")) %>% 
  write.csv2("tab_valores_estranhos.csv")

tab_dispendios %>% 
  dplyr::filter(!stringr::str_detect(vlr, ",")) %>% 
  pull(vlr) %>% 
  as.numeric() %>% sum()
