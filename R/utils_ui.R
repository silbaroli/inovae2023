intro_secao <- function(.id) {
  textos <- tab_textos_intro %>% 
    dplyr::filter(id == .id)
  titulo <- stringr::str_replace(textos$nome_aba, "/", ":")
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(titulo, class = "titulo-pagina")
      )
    ),
    fluidRow(
      class = "descricao",
      column(
        width = 12,
        p(textos$descricao)
      )
    )
  )
}

aviso_secao <- function(.id) {
  textos <- tab_textos_intro %>% 
    dplyr::filter(id == .id)
  if (!isTruthy(textos$aviso)) {
    tagList()
  } else if (textos$aviso == "aviso_padrao") {
    shinyWidgets::dropdown(
      disclaimer_padrao(),
      circle = FALSE,
      status = "warning",
      icon = icon("warning"),
      width = "90%",
      tooltip = shinyWidgets::tooltipOptions(
        title = "Clique para ver informações sobre os 
        dados que geram essa visualização."
      )
    )
  } else {
    tagList()
  }
  
}

titulo_secao <- function(text) {
  fluidRow(
    class = "text-center titulo",
    column(
      width = 12,
      h2(text)
    )
  )
}

caixa <- function(..., title, width = 12, collapsible = FALSE) {
  bs4Dash::bs4Card(
    title = title, 
    width = width,
    collapsible = collapsible,
    closable = FALSE,
    ... = ...
  )
}

wrap_text <- function(x) {
  x %>% 
    stringr::str_wrap(width = 30) %>% 
    stringr::str_replace_all("\\n", "<br>")
}

seletor_instituicoes <- function(id, label, choices, selected) {
  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    multiple = TRUE,
    selected = selected,
    width = "100%",
    options = shinyWidgets::pickerOptions(
      actionsBox = TRUE,
      selectAllText = "Marcar todas",
      deselectAllText = "Desmarcar todas",
      noneSelectedText = "Selecione ao menos uma instituição",
      selectedTextFormat = "count",
      countSelectedText = "{0}/{1} instituições selecionadas"
    )
  )
}

seletor_eixo_y <- function(id) {
  shinyWidgets::prettyRadioButtons(
    inputId = id,
    label = "Visualizar dispêndios como", 
    choices = purrr::set_names(
      tab_vis_dispendios$opcoes_abrev,
      tab_vis_dispendios$opcoes_desc
    ),
    status = "primary",
    fill = TRUE
  )
}

seletor_eixo_y_perfil <- function(id){
  shinyWidgets::prettyRadioButtons(
    inputId = id,
    label = "Analisar perfil dos projetos como...", 
    choices = purrr::set_names(
      c("duracao_anos", "porte"),
      c("Duração do projeto", "Porte")
    ),
    status = "primary",
    fill = TRUE
  )
}

col_opcoes_vis <- function(...) {
  column(
    width = 2,
    # class = "col-6 col-md-5 col-xl-6",
    ...
  )
}

col_indicadores <- function(...) {
  column(
    width = 2,
    # class = "col-6 col-md-5 col-xl-6",
    style = "position: static;",
    ...
  )
}

col_dropdown_filtros <- function(...) {
  column(
    width = 2,
    # class = "col-6 col-xl-2",
    style = "position: static;",
    ...
  )
}

col_slider_ano <- function(...) {
  column(
    width = 6,
    # class = "col-6 col-xl-2",
    ...
  )
} 

col_warning <- function(...) {
  column(
    width = 4,
    class = "text-right",
    # class = "col-6 col-xl-1 text-right",
    ...
  )
}

col_botao_download <- function(...) {
  column(
    class = "text-right",
    # class = "coluna-botao col-5 col-md-3 text-right",
    width = 2,
    ...
  )
}

ano_maximo <- function() {
  return(2022)
}

tabBox <- function(..., id, choices, width = 12) {
  div(
    class = "tabBox",
    fluidRow(
      class = "align-items-center tabBox-header",
      column(
        width = width,
        class = "text-right",
        shinyWidgets::radioGroupButtons(
          inputId = id,
          choices = choices,
          justified = TRUE,
          selected = choices[1]
        )
      )
    ),
    hr(),
    fluidRow(
      class = "tabBox-body",
      column(
        width = width,
        ...
      )
    )
  )
}

dropdown_filtros <- function(id) {
  ns <- NS(id)
  categorias_nivel1 <- tab_categorias %>% 
    dplyr::distinct(nm_categoria_nv1) %>% 
    dplyr::pull(nm_categoria_nv1)
  
  nomes_wrap <- wrap_text(categorias_nivel1)
  
  instituicoes <- tab_fomentador %>% 
    dplyr::pull(nme_form)
  
  shinyWidgets::dropdown(
    status = "primary",
    icon = icon("filter"),
    label = "Filtros",
    width = "90%",
    tooltip = shinyWidgets::tooltipOptions(
      title = "Clique para ver mais opções de visualização."
    ),
    div(
      class = "filtros-dropdown",
      fluidRow(
        column(
          class = "text-center col-md-2",
          width = 12,
          icon("lightbulb"),
          p("Tecnologias energéticas", class = "descricao")
        ),
        column(
          width = 12,
          class = "col-md-10",
          seletor_tecnologia(
            id = ns("categ_1"),
            label = "Tecnologia energética (nível 1)",
            choices = categorias_nivel1,
            selected = categorias_nivel1,
            nomes = nomes_wrap
          ),
          br(),
          seletor_tecnologia(
            ns("categ_2"),
            label = "Tecnologia energética (nível 2)",
            choices = c("Carregando" = ""),
            selected = NULL
          )
          # hr(class = "hidden-md-up visible")
        ),
      ),
      hr(),
      fluidRow(
        column(
          class = "text-center col-md-2",
          width = 12,
          icon("file-signature"),
          p("Instituições fomentadoras", class = "descricao")
        ),
        column(
          width = 12,
          class = "col-md-10",
          seletor_instituicoes(
            ns("instuicoes"),
            label = "Instituições fomentadoras",
            choices = sort(instituicoes),
            selected = instituicoes
          )
        )
      )
    )
  )
}

seletor_tecnologia <- function(id, label, choices, selected, nomes = NULL) {
  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    multiple = TRUE,
    selected = selected,
    width = "100%",
    choicesOpt = list(content = nomes),
    options = shinyWidgets::pickerOptions(
      actionsBox = TRUE,
      selectAllText = "Marcar todas",
      deselectAllText = "Desmarcar todas",
      noneSelectedText = "Selecione ao menos uma tecnologia",
      selectedTextFormat = "count",
      countSelectedText = "{0}/{1} tecnologias selecionadas",
      sanitize = TRUE
    )
  )
}

disclaimer_padrao <- function() {
  div(
    class = "text-left",
    p("Dados provenientes da ANP, FAPESP e CNEN estão agregados. 
      Dessa forma, os resultados apresentados possuem as seguintes 
      características:"),
    tags$ol(
      tags$li(
        "Dados da ANP possui apenas informação de investimento por ano 
        e não possui informação do número de projetos. Os dados de volume 
        de investimentos foram classificados na categoria 2.9 – Combustíveis
        Fósseis não-alocados."
      ),
      tags$li(
        "Dados da FAPESP possui apenas informação de investimento por categoria 
        energética, mas não possui o número de projetos."
      ),
      tags$li(
        "Dados do CNEN possui apenas informação de 
        investimento por ano e não possui informação do número de projetos."
      )
    ),
    p("Dados provenientes da FINEP são relacionados apenas a projetos 
        de natureza pública e modalidade reembolsável."),
    p("Dados provenientes do CNPq são relacionados apenas a projetos 
        financiados com fundos extra FNDCT.")
  )
}







