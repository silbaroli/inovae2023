#' Conjunto de traduções do reactable
traducao_reactable <- function() {
  reactable::reactableLang(
    searchPlaceholder = "Procurar",
    searchLabel = "",
    noData = "Nenhuma informação encontrada",
    pageNext = "Próxima",
    pagePrevious = "Anterior",
    pageNumbers = "{page} de {pages}",
    pageInfo = "{rowStart}\u2013{rowEnd} de {rows} linhas",
    pageSizeOptions = "Mostrar {rows}",
    pageNextLabel = "Próxima página",
    pagePreviousLabel = "Página anterior",
    pageNumberLabel = "Página {page}",
    pageJumpLabel = "Ir para a página",
    pageSizeOptionsLabel = "Linhas por página",
    defaultGroupHeader = "Agrupado",
    detailsExpandLabel = "Expandir detalhes",
    detailsCollapseLabel = "Colapsar detalhes",
  )
}

#' Parâmetros padrão das tabelas
tabela_padrao <- function(tab, col_def, colunas) {
  reactable::reactable(
    tab,
    wrap = FALSE,
    pagination = FALSE,
    striped = TRUE,
    sortable = FALSE,
    columns = colunas,
    defaultColDef = col_def,
    class = "tabela",
    theme = reactable::reactableTheme(
      backgroundColor = "#fff",
      stripedColor = "#e5e5e5",
      headerStyle = list(
        backgroundColor = "var(--corPrincipal);",
        color = "#fff"
      )
    )
  )
}

#' Parâmetros de estilo da tabela de projetos
tabela_projetos <- function(tab, col_def = NULL, colunas, onclick,
                            pagination = FALSE) {
  reactable::reactable(
    tab,
    onClick = onclick,
    wrap = FALSE,
    pagination = pagination,
    defaultPageSize = 15,
    sortable = FALSE,
    filterable = FALSE,
    searchable = TRUE,
    columns = colunas,
    defaultColDef = col_def,
    class = "tabela",
    theme = reactable::reactableTheme(
      backgroundColor = "#fff",
      stripedColor = "#e5e5e5",
      headerStyle = list(
        backgroundColor = "var(--corPrincipal);",
        color = "#fff"
      )
    ),
    language = traducao_reactable()
  )
}

#' Parâmetros de estilo da tabela de detalhamentos do explorador de projetos
tab_projetos_min <- function(tab) {
  reactable::reactable(
    tab,
    class = "tabela",
    wrap = FALSE,
    sortable = FALSE,
    filterable = FALSE,
    searchable = TRUE,
    defaultPageSize = 5,
    language = traducao_reactable(),
    columns = list(
      id_item = reactable::colDef(
        name = "ID",
        width = 100
      ),
      titulo = reactable::colDef(
        name = "Projeto",
        width = 200
      ),
      valor_total_formatado = reactable::colDef(
        name = "Valor (em milhões de reais)",
        align = "right"
      )
    ),
    theme = reactable::reactableTheme(
      backgroundColor = "#fff",
      stripedColor = "#e5e5e5",
      headerStyle = list(
        backgroundColor = "var(--corPrincipal);",
        color = "#fff"
      )
    )
  )
}

sticky_style <- function(value, index) {
  if (index == 0) {
    list(
      position = "sticky",
      left = 0,
      background = "var(--corPrincipal);",
      zIndex = 1,
      borderRight = "1px solid #eee"
    )
  } else if (index %% 2 != 0) {
    list(
      position = "sticky",
      left = 0,
      background = "#e5e5e5",
      zIndex = 1,
      borderRight = "1px solid #eee"
    )
  } else {
    list(
      position = "sticky",
      left = 0,
      background = "#fff",
      zIndex = 1,
      borderRight = "1px solid #eee"
    )
  }
}

tabela_var_anual <- function(tab, nome_grupo = "Tecnologia") {
  
  if (any(stringr::str_detect(tab$y_format, stringr::fixed("R$")))) {
    tab <- tab %>% 
      dplyr::mutate(
        y_format = formatar_numero(y, 0.01)
      )
    vlr_na <-  "0,00"
  } else if (any(stringr::str_detect(tab$y_format, stringr::fixed("%")))) {
    vlr_na <- "0,00%"
  } else {
    vlr_na <- "0"
  }
  
  if (nome_grupo == "Tecnologia" & dplyr::n_distinct(tab$grupo) > 10)  {
    width <-  500
  } else if (nome_grupo == "Tecnologia") {
    width <- 340
  } else {
    width <- 200
  }
  
  tab <- tab %>% 
    dplyr::ungroup() %>% 
    dplyr::select(ano, y_format, grupo) %>% 
    tidyr::pivot_wider(names_from = ano, values_from = y_format) %>% 
  tabela_padrao(
    col_def = reactable::colDef(
      align = "right",
      na = vlr_na
    ),
    colunas = list(
      grupo = reactable::colDef(
        align = "left",
        name = nome_grupo,
        width = width
        # style = sticky_style,
        # headerStyle = sticky_style(0, 0)
      )
    )
  )
}

#' Parâmetros de estilo da tabela de dispendios ao longo dos anos
tabela_dispendios <- function(tab) {
  
  ylab <- unique(tab$ylab)
  
  if (any(stringr::str_detect(tab$y_format, stringr::fixed("R$")))) {
    tab <- tab %>% 
      dplyr::mutate(
        y_format = formatar_numero(y, 0.01)
      )
    vlr_na <-  "0,00"
  } else if (any(stringr::str_detect(tab$y_format, stringr::fixed("%")))) {
    vlr_na <- "0,00%"
  } else {
    vlr_na <- "0"
  }
  
  if (dplyr::n_distinct(tab$x) > 10)  {
    width <-  450
  } else {
    width <- 340
  } 
  
  tab %>% 
    dplyr::ungroup() %>% 
    dplyr::select(x, y_format) %>% 
    tabela_padrao(
      col_def = reactable::colDef(align = "right", na = vlr_na),
      colunas = list(
        x = reactable::colDef(
          name = "Tecnologia",
          align = "left",
          width = width
          #style = sticky_style,
          #headerStyle = sticky_style(0, 0)
        ),
        y_format = reactable::colDef(name = ylab)
      )
    )
}

#' Parâmetros de estilo da tabela de dispendios agrupados ao longo dos anos
tabela_dispendios_agrup <- function(tab, nome_x = "Tecnologia") {
  if (any(stringr::str_detect(tab$y_format, stringr::fixed("R$")))) {
    tab <- tab %>% 
      dplyr::mutate(
        y_format = formatar_numero(y, 0.01)
      )
    vlr_na <-  "0,00"
  } else if (any(stringr::str_detect(tab$y_format, stringr::fixed("%")))) {
    vlr_na <- "0,00%"
  } else {
    vlr_na <- "0"
  }
  
  if (nome_x == "Tecnologia" & dplyr::n_distinct(tab$x) > 10)  {
    width <-  500
  } else if (nome_x == "Tecnologia") {
    width <- 340
  } else {
    width <- 200
  }
  
  tab %>% 
    dplyr::ungroup() %>% 
    dplyr::select(x, grupo, y_format) %>% 
    tidyr::pivot_wider(names_from = grupo, values_from = y_format) %>% 
    tabela_padrao(
      col_def = reactable::colDef(align = "right", na = vlr_na),
      colunas = list(
        x = reactable::colDef(
          name = nome_x,
          align = "left",
          width = width
          #style = sticky_style,
          #headerStyle = sticky_style(0, 0)
        )
      )
    )
}
