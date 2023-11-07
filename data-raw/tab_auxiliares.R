cat("Connecting with database...\n")
con <- RSQLite::dbConnect(RSQLite::SQLite(), "ebp_final.db")

cat("Creating auxiliary tables...\n")


# Criando tabelas auxiliares ----------------------------------------------

# Tabela com os deflatores
deflatores <- readxl::read_excel("data-raw/xlsx/Deflator PIB.xlsx", skip = 7) %>% 
  tidyr::gather(ano, deflator) %>% 
  dplyr::mutate(
    deflator = dplyr::if_else(is.na(deflator), 100, deflator)
  ) %>% 
  dplyr::bind_rows(
    tibble::tibble(
      ano = as.character(2021:2030),
      deflator = 100
    )
  )

# Tabela com o PIB por ano
pib_por_ano <- readxl::read_excel("data-raw/xlsx/Tabela 6784.xlsx", skip = 3) %>%
  purrr::set_names(c("ano", "pib")) %>% 
  dplyr::mutate(
    pib = pib*10^6
  ) %>% 
  dplyr::filter(!is.na(pib)) %>% 
  dplyr::bind_rows(
    tibble::tibble(
      ano = as.character(2019:2030),
      pib = rep(NA, length(as.character(2019:2030)))
    )
  ) %>% 
  tidyr::fill(
    pib
  )

# Nome das categorias de nível 1
nomes_categoria_dig_1 <- tibble::tibble(
  cat1 = as.character(1:7),
  nome_dig1 = c(
    "Eficiência energética", 
    "Combustíveis fósseis: petróleo, gás natural e carvão mineral",
    "Energias renováveis", 
    "Fissão e fusão nuclear",
    "Hidrogênio e células a combustível",
    "Outras tecnologias de energia e armazenamento",
    "Outras tecnologias tranversais"
  )
)

tab_categorias <- con %>% 
  dplyr::tbl("dm_categoria") %>% 
  dplyr::collect() %>% 
  dplyr::left_join(
    nomes_categoria_dig_1,
    by = "cat1"
  ) %>% 
  dplyr::rename(nm_categoria_nv1 = nome_dig1, nm_categoria_nv2 = nm_categoria)


# Salvando tabelas auxilares no banco -------------------------------------

RSQLite::dbWriteTable(con, "dm_categoria_revisado", tab_categorias, overwrite = TRUE)
DBI::dbWriteTable(con, "deflatores", deflatores, overwrite = TRUE)
DBI::dbWriteTable(con, "pib_por_ano", pib_por_ano, overwrite = TRUE)



# Criando conexão com tabelas do banco de dados ---------------------------

# Deflatores

tab_deflatores <- con %>% 
  dplyr::tbl("deflatores")

# PIB

tab_pib <- con %>% 
  dplyr::tbl("pib_por_ano")

# Dispêndios
tab_dispendios <- con %>% 
  dplyr::tbl("ft_dispendio") %>% 
  dplyr::select(-dta_inicio) %>%
  dplyr::filter(ano > 2012, ano < 2022) %>% 
  dplyr::left_join(tab_deflatores) %>% 
  dplyr::left_join(tab_pib) %>% 
  dplyr::mutate(
    pib = pib * (deflator/100) / 1e6,
    # isso aqui é para garantir que dentro de um mesmo 
    # ano os valores considerados são da mesma época
    vlr = as.numeric(vlr) * (deflator/100),
    vlr = vlr / 1e6,
    id_prop = as.integer(id_prop)
  )

# Natureza dispêndios
tab_natureza <- con %>% 
  dplyr::tbl("dm_nat_disp") %>% 
  dplyr::rename(natureza_desc = desc)

# Modalidade dispêndios
tab_modalidade <- con %>% 
  dplyr::tbl("dm_mod_finan") %>% 
  dplyr::rename(modalidade_desc = desc) %>% 
  dplyr::mutate(
    modalidade_desc = dplyr::case_when(
      modalidade_desc == "Reembolvável" ~ "Reembolsável",
      TRUE ~ modalidade_desc
    )
  )

# Fomentador
tab_fomentador <- dplyr::tbl(con, "dm_formentador")

# Categorias
tab_categorias <- con %>% 
  dplyr::tbl("dm_categoria_revisado") %>% 
  dplyr::select(id, nm_categoria_nv1, nm_categoria_nv2, dsc_categoria)

dispendio_por_projeto <- tab_dispendios %>%
  dplyr::left_join(
    tab_fomentador,
    by = c("id_formnt" = "id_formentador")
  ) %>% 
  dplyr::group_by(id_item, id_cat2, nme_form) %>% 
  dplyr::summarise(
    ntz_finan = ntz_finan[1],
    id_finan = id_finan[1],
    valor_total = sum(vlr, na.rm = TRUE)
  )

# Projetos
tab_projetos <- con %>% 
  dplyr::tbl("dm_projeto") %>%
  dplyr::mutate(
    duracao_anos = as.numeric(dta_limite - dta_inicio)
  ) %>% 
  dplyr::left_join(dispendio_por_projeto) %>% 
  dplyr::rename(titulo = 4, situacao = 6) %>% 
  dplyr::mutate(
    ano = as.numeric(substr(dta_inicio, 1, 4))
  )

# Executor
tab_executor <- dplyr::tbl(con, "dm_agente_empresa") %>% 
  dplyr::rename_with(.fn = ~ paste0(.x, "_exec")) 

# Proponente
tab_proponente <- dplyr::tbl(con, "dm_agente_empresa") %>% 
  dplyr::rename_with(.fn = ~ paste0(.x, "_prop"))


# Criando tabelas auxiliares que não são salvas no banco ------------------

# Opções de visualização do total de dispêndios
tab_vis_dispendios <- tibble::tibble(
  opcoes_abrev = c(
    "valor", 
    "porcProj",
    "porcPIB",
    "quantidade"
    
  ),
  opcoes_desc = c(
    "Valor investido (em milhões de reais)", 
    "% do valor em relação ao total investido",
    "% do valor em relação ao PIB",
    "Quantidade de projetos"
  )
)

# Tabela com título e subtítulo das páginas e disclaimers, novo
tab_textos_intro <- readxl::read_excel(
  "data-raw/xlsx/textos_intro_secoes.xlsx"
)


