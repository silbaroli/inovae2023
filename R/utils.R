#' Parametros gerais do app
#' 
#' @description Essa função concentra todos os parametros que afetam várias 
#' partes do app. Atualmente estão mapeadas (a) altura dos containers dos gráficos,
#' que é passada nas chamadas da função echarts4rOutput (b) tamanho da fonte das
#' legendas e (c) a altura do grid passado em algumas chamadas da funcao e_grid
parametros <- function() {
  list(
    altura_graficos = "700px",
    tamanho_da_fonte = 18,
    altura_grid = "12%"
  )
}

#' Vetor de cores da paleta da EPE
cores_epe <- function() {
  c(
    rgb(19, 71, 93, maxColorValue = 255),
    rgb(241, 151, 89, maxColorValue = 255),
    rgb(49, 133, 156, maxColorValue = 255),
    rgb(149, 55, 53, maxColorValue = 255),
    rgb(179, 162, 199, maxColorValue = 255),
    rgb(0, 103, 14, maxColorValue = 255),
    "#4169e1",
    "#7FFF00",
    "#FF00FF",
    "#DAA520",
    "#F0E68C"
  )
}

#' Função formatadora de porcentagem
#'
#' @description Wrapper da função [scales::percent] que padroniza dados em %
#' para o formato brasileiro
#'
#' @param x Vetor do tipo numérico
formatar_porc <- function(x) {
  scales::percent(
    x,
    accuracy = 0.01,
    big.mark = ".",
    scale = 1,
    decimal.mark = ","
  )
}

#' Função formatadora de números > 1
#'
#' @description Wrapper da função [scales::number] que padroniza números
#' para o formato brasileiro
#'
#' @param x Vetor do tipo numérico
formatar_numero <- function(x, acc = 1) {
  scales::number(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
  )
}

#' Função formatadora de valores monetários
#'
#' @description Wrapper da função [scales::dollar] que padroniza dados
#' para o formato brasileiro
#'
#' @param x Vetor do tipo numérico
formatar_dinheiro <- function(x) {
  scales::dollar(
    x,
    accuracy = 0.01, 
    prefix = "R$ ", 
    decimal.mark = ",",
    big.mark = "."
  )
}

#' Cria a tabela base para qualquer grafico de séries de tempo
#'
#' @description Cria a tabela base para qualquer grafico de séries de tempo. O resultado 
#' é uma tibble pronta para ser usada em alguma função gráfica e a entrada é o resultado
#' de uma chamada da funcao \code{\link[dplyr::tbl]{tbl}}. 
#'
#' @param tab Tabela retornada por uma chama da função \code{\link[dplyr::tbl]{tbl}}.
#' Essa função aplicará \code{\link[dplyr::collect]{collect}} neste objeto.
#' @param eixo_y String descrevendo o que entrará no eixo y do gráfico. coluna "y" da tabela de saída.
#' Pode assumir os valores "valor", "porcPIB", "quantidade", "procProj". Esse parâmetro define os valores
#' das colunas ylab, e format_func. Veja a seção "Value".
#' @param var_cor String descrevendo o que dará a cor de cada linhas. Coluna "grupo" da tabela de saída.
#' Deve ser um do nome das colunas de [tab].
#'
#' @return Uma tibble contendo as colunas ano, y, grupo, ylab, y_format, format_func. Em função do parâmetro
#' eixo_y, [calcular_eixo_y_barras_emp] retorna uma label (ylab) e o valor formatado da coluna y (format_func)
#' com base no que estiver preenchido na tabela [tab_vis_dispendios],
#' criada em data-raw/tab_auxiliares.R.
calcular_eixo_y_serie <- function(tab, eixo_y, 
                                  var_cor = NULL,
                                  nome_var_cor = NULL,
                                  other_level = "Demais tecnologias"
) {
  
  if (is.null(nome_var_cor)) {
    nome_var_cor <- "Todas as tecnologias energéticas"
  }
  
  if (is.null(var_cor)) {
    tab <- tab %>% 
      dplyr::mutate(grupo = nome_var_cor) %>% 
      dplyr::group_by(grupo)
    var_cor <- "grupo"
  } else {
    tab <- tab %>% 
      dplyr::group_by(.data[[var_cor]])
  }
  
  
  if (eixo_y == "valor") {
    formatador <- formatar_dinheiro
    tab <- tab %>% 
      dplyr::group_by(ano, .add = TRUE) %>% 
      dplyr::summarise(
        y = sum(vlr, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(
        grupo = .data[[var_cor]],
        y = round(y, 1)
      )
  } else if (eixo_y == "porcPIB") {
    formatador <- formatar_porc
    tab <- tab %>% 
      dplyr::group_by(ano, .add = TRUE) %>% 
      dplyr::summarise(
        y = sum(vlr / pib, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(
        grupo = .data[[var_cor]],
        y = round(y, 4)
      )
  } else if (eixo_y == "quantidade") {
    formatador <- formatar_numero
    tab <- tab %>% 
      dplyr::left_join(
        dplyr::select(tab_projetos, id_item, id_projeto),
        by = "id_item"
      ) %>% 
      dplyr::group_by(ano, .add = TRUE) %>% 
      dplyr::summarise(
        y = dplyr::n_distinct(id_projeto),
        .groups = "drop"
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(grupo = .data[[var_cor]])
  } else if (eixo_y == "porcProj") {
    formatador <- formatar_porc
    tab_invest <- tab %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(ano) %>% 
      dplyr::summarise(
        total_investido = sum(vlr, na.rm = TRUE),
        .groups = "drop"
      )
    
    tab <- tab %>% 
      dplyr::left_join(tab_invest) %>% 
      dplyr::group_by(ano, .add = TRUE) %>% 
      dplyr::summarise(
        y = sum(vlr / total_investido, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(
        grupo = .data[[var_cor]],
        y = round(y, 2)
      )
  }
  
  lab <- tab_vis_dispendios %>% 
    dplyr::filter(opcoes_abrev == eixo_y) %>% 
    dplyr::pull(opcoes_desc)
  
  tab %>% 
    dplyr::mutate(
      ylab = lab,
      y_format = formatador(y),
      format_func = list(formatador)
    )
}

agrupar_tecnologias_serie <- function(tab, other_level = "Demais tecnologias") {
  formatador <- tab$format_func[[1]]
  tab %>% 
    dplyr::mutate(
      grupo = forcats::fct_lump_n(
        f = grupo, 
        n = 6, 
        w = y,
        other_level = other_level,
        ties.method = "first"
      )
    ) %>%
    dplyr::group_by(ano, grupo) %>% 
    dplyr::summarise(
      y = sum(y),
      ylab = dplyr::first(ylab),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      y_format = formatador(y)
    )
}

#' Agrupar tabela com muitas categorias
#'
#' @description Recebe uma tabela saída de calcula_eixo_** e reduz o número de
#' valores distintos da coluna x.
#'
#' @param tab Uma tibble saída de uma função calcula_eixo_**
#' @param other_level String que será passada na função [forcats::fct_lump_n()]
agrupar_tecnologias_barras <- function(tab, other_level = "Demais tecnologias") {
  formatador <- tab$format_func[[1]]
  tab %>% 
    dplyr::mutate(
      x = forcats::fct_lump_n(
        f = x, 
        n = 6, 
        w = y,
        other_level = other_level,
        ties.method = "first"
      )
    ) %>%
    dplyr::group_by(x) %>% 
    dplyr::summarise(
      y = sum(y),
      ylab = dplyr::first(ylab),
      .groups = "drop"
    )  %>% 
    dplyr::mutate(
      y_format = formatador(y),
      x = as.character(x)
    )
}

#' Agrupar tabela com muitas categorias
#'
#' @description Recebe uma tabela saída de calcula_eixo_** e reduz o número de
#' valores distintos da coluna x.
#'
#' @param tab Uma tibble saída de uma função calcula_eixo_**
#' @param other_level String que será passada na função [forcats::fct_lump_n()]
agrupar_tecnologias_barras_agrup <- function(tab, 
                                             other_level = "Demais tecnologias") {
  formatador <- tab$format_func[[1]]
  tab %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      x = forcats::fct_lump_n(
        f = x, 
        n = 6, 
        w = y,
        other_level = other_level,
        ties.method = "first"
      )
    ) %>%
    tidyr::complete(x, grupo, fill = list(y = 0)) %>% 
    dplyr::group_by(x, grupo) %>% 
    dplyr::summarise(
      y = sum(y),
      ylab = dplyr::first(ylab),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      y_format = formatador(y),
      x = as.character(x)
    )
}

#' Cria a tabela base para todos os graficos de barras
#'
#' @description Cria a tabela base para todos os graficos de barras não empilhadas. O resultado 
#' é uma tibble pronta para ser usada em alguma função gráfica e a entrada é o resultado
#' de uma chamada da funcao \code{\link[dplyr::tbl]{tbl}}. 
#'
#' @param tab Tabela retornada por uma chama da função \code{\link[dplyr::tbl]{tbl}}.
#' Essa função aplicará \code{\link[dplyr::collect]{collect}} neste objeto.
#' @param eixo_y String descrevendo o que entrará no eixo y do gráfico. coluna "y" da tabela de saída.
#' Pode assumir os valores "valor", "porcPIB", "quantidade", "procProj". Esse parâmetro define os valores
#' das colunas ylab, e format_func. Veja a seção "Value".
#' @param eixo_x String descrevendo o que entrará no eixo x do gráfico. Coluna "x" da tabela de saída.
#' Deve ser um do nome das colunas de [tab].
#' @param var_cor String descrevendo o que dará a cor de cada barra. Coluna "grupo" da tabela de saída.
#' Deve ser um do nome das colunas de [tab].
#'
#' @return Uma tibble contendo as colunas x, y, grupo, ylab, y_format, format_func. Em função do parâmetro
#' eixo_y, [calcular_eixo_y_barras_emp] retorna uma label (ylab) e o valor formatado da coluna y (format_func)
#' com base no que estiver preenchido na tabela [tab_vis_dispendios],
#' criada em data-raw/tab_auxiliares.R.
calcular_eixo_y_barras <- function(tab, eixo_y, eixo_x, 
                                   nome_var_cor = NULL) {
  
  if (is.null(nome_var_cor)) {
    nome_var_cor <- "Todas as tecnologias energéticas"
  }
  
  if (is.null(eixo_x)) {
    tab <- tab %>% 
      dplyr::mutate(grupo = nome_var_cor) 
    eixo_x <- "grupo"
  }
  
  if (eixo_y == "valor") {
    formatador <- formatar_dinheiro
    tab <- tab %>% 
      dplyr::group_by(.data[[eixo_x]]) %>% 
      dplyr::summarise(
        y = sum(vlr, na.rm = TRUE)
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], y = round(y, 1))
  } else if (eixo_y == "porcPIB") {
    formatador <- formatar_porc
    tab <- tab %>% 
      dplyr::group_by(.data[[eixo_x]]) %>% 
      dplyr::summarise(
        y = sum(vlr / pib, na.rm = TRUE) * 100
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], y = round(y, 4))
  } else if (eixo_y == "quantidade") {
    formatador <- formatar_numero
    tab <- tab %>% 
      dplyr::left_join(
        dplyr::select(tab_projetos, id_item, id_projeto),
        by = "id_item"
      ) %>%  
      dplyr::group_by(.data[[eixo_x]]) %>% 
      dplyr::summarise(
        y = dplyr::n_distinct(id_projeto)
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]])
  } else if (eixo_y == "porcProj") {
    formatador <- formatar_porc
    tab <- tab %>% 
      dplyr::mutate(total_investido = sum(vlr, na.rm = TRUE)) %>% 
      dplyr::group_by(.data[[eixo_x]], .add = TRUE) %>% 
      dplyr::summarise(
        y = sum(vlr / total_investido, na.rm = TRUE) * 100
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], y = round(y, 2))
  }
  
  lab <- tab_vis_dispendios %>% 
    dplyr::filter(opcoes_abrev == eixo_y) %>% 
    dplyr::pull(opcoes_desc)
  
  tab %>% 
    dplyr::mutate(
      ylab = lab,
      y_format = formatador(y),
      format_func = list(formatador)
    )
}




#' Cria a tabela base para todos os graficos de barras empilhadas
#'
#' @description Cria a tabela base para todos os graficos de barras empilhadas. O resultado 
#' é uma tibble pronta para ser usada em alguma função gráfica e a entrada é o resultado
#' de uma chamada da funcao \code{\link[dplyr::tbl]{tbl}}. 
#'
#' @param tab Tabela retornada por uma chama da função \code{\link[dplyr::tbl]{tbl}}.
#' Essa função aplicará \code{\link[dplyr::collect]{collect}} neste objeto.
#' @param eixo_y String descrevendo o que entrará no eixo y do gráfico. coluna "y" da tabela de saída.
#' Pode assumir os valores "valor", "porcPIB", "quantidade", "procProj". Esse parâmetro define os valores
#' das colunas ylab, e format_func. Veja a seção "Value".
#' @param eixo_x String descrevendo o que entrará no eixo x do gráfico. Coluna "x" da tabela de saída.
#' Deve ser um do nome das colunas de [tab].
#' @param var_cor String descrevendo o que dará a cor de cada barra empilhada. Coluna "grupo" da tabela de saída.
#' Deve ser um do nome das colunas de [tab].
#'
#' @return Uma tibble contendo as colunas x, y, grupo, ylab, y_format, format_func. Em função do parâmetro
#' eixo_y, [calcular_eixo_y_barras_emp] retorna uma label (ylab) e o valor formatado da coluna y (format_func)
#' com base no que estiver preenchido na tabela [tab_vis_dispendios],
#' criada em data-raw/tab_auxiliares.R.
calcular_eixo_y_barras_emp <- function(tab, eixo_y, eixo_x, var_cor) {
  
  if (eixo_y == "valor") {
    formatador <- formatar_dinheiro
    tab <- tab %>% 
      dplyr::group_by(.data[[eixo_x]], .data[[var_cor]]) %>% 
      dplyr::summarise(
        y = sum(vlr, na.rm = TRUE)
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], grupo = .data[[var_cor]], y = round(y, 1))
  } else if (eixo_y == "porcPIB") {
    formatador <- formatar_porc
    tab <- tab %>% 
      dplyr::group_by(.data[[eixo_x]], .data[[var_cor]]) %>% 
      dplyr::summarise(
        y = sum(vlr / pib, na.rm = TRUE) * 100
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], grupo = .data[[var_cor]], y = round(y, 4))
  } else if (eixo_y == "quantidade") {
    formatador <- formatar_numero
    tab <- tab %>% 
      dplyr::left_join(
        dplyr::select(tab_projetos, id_item, id_projeto),
        by = "id_item"
      ) %>% 
      dplyr::group_by(.data[[eixo_x]], .data[[var_cor]]) %>% 
      dplyr::summarise(
        y = dplyr::n_distinct(id_projeto)
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], grupo = .data[[var_cor]])
  } else if (eixo_y == "porcProj") {
    formatador <- formatar_porc
    tab <- tab %>% 
      dplyr::group_by(.data[[eixo_x]]) %>% 
      dplyr::mutate(total_investido = sum(vlr, na.rm = TRUE)) %>% 
      dplyr::group_by(.data[[var_cor]], .add = TRUE) %>% 
      dplyr::summarise(
        y = sum(vlr / total_investido, na.rm = TRUE) * 100
      ) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(x = .data[[eixo_x]], grupo = .data[[var_cor]], y = round(y, 2))
  }
  
  lab <- tab_vis_dispendios %>% 
    dplyr::filter(opcoes_abrev == eixo_y) %>% 
    dplyr::pull(opcoes_desc)
  
  tab %>% 
    dplyr::mutate(
      ylab = lab,
      y_format = formatador(y),
      format_func = list(formatador)
    )
}

#' Cria um parágrafo contendo um título e um texto em negrito
p_info <- function(desc, valor) {
  p(
    span(class = "font-weight-bold", desc),
    HTML(paste0(span(": ", valor)))
  )
}

#' Cria o título de uma tabela
#' 
#' @tab Tibble que com uma coluna chamada ylab
criar_titulo_tabela <- function(tab, complemento = "") {
  titulo <- paste(
    unique(tab$ylab),
    complemento
  )
}