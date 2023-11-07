#' controles_padrao UI Function, touching
#'
#' @description Este módulo cria a seção de filtros e controles
#' das visualizações. Ele é utilizado em todas as abas, com exceção
#' da aba "Explorar projetos", que possui uma configuração de controles
#' diferente.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_controles_padrao_ui <- function(id, id_tab, choices_vis = NULL, tipo_eixo_y = "normal") {
  ns <- NS(id)
  if (is.null(choices_vis)) {
    choices_vis <- c(
      `<i class='fa fa-bar-chart'></i>` = "barras", 
      `<i class='fa fa-line-chart'></i>` = "linhas", 
      `<i class='fa fa-table'></i>` = "tabela"
    )
  }
  tagList(
    fluidRow(
      class = "align-items-center filtros",
      column(
        width = 12,
         class = "col-md-4 col-lg-4 col-xl-3",
        shinyWidgets::radioGroupButtons(
          inputId = ns("vis_escolhida"),
          label = "", 
          choices = choices_vis,
          selected = choices_vis[1],
          width = "100%"
        )
      ),
      column(
        width = 12,
        class = "indicadores col-md-3 col-lg-3 col-xl-2",
        style = "position: static;",
        shinyWidgets::dropdown(
          status = "primary",
          icon = icon("ruler-horizontal"),
          label = "Indicador",
          width = "400px",
          tooltip = shinyWidgets::tooltipOptions(
            title = "Visualizar dispêndios como..."
          ),
          div(
            class = "filtros-dropdown",
            if(tipo_eixo_y == "normal") {
              seletor_eixo_y(ns("eixo_y")) 
            } else {
              seletor_eixo_y_perfil(ns("eixo_y"))
            }
          )
        ),
        br(class = "hidden-md-up")
      ),
      column(
        width = 8,
        class = "dd-filtros col-8 col-md-3 col-lg-3 col-xl-2",
        style = "position: static;",
        dropdown_filtros(id)
      ),
      column(
        width = 2,
        class = "text-right disclaimer col-2 col-md-1 col-lg-1 col-xl-4",
        style = "position: static;",
        aviso_secao(id_tab)
      ),
      column(
        width = 2,
        class = "text-right col-md-1 col-2 col-lg-1 col-xl-1",
        mod_int_baixar_dados_ui(ns("int_baixar_dados_ui_1"))
      )
    ),
    fluidRow(
      class = "align-items-center filtros justify-content-left",
      style = "margin-top: 20px;",
      column(
        class = "slider",
        width = 12,
        class = "col-12 col-md-6",
        uiOutput(ns("ui_slider"))
      )
    ),
    br()
  )
}

#' controles_padrao Server Functions
#'
#' @noRd 
mod_controles_padrao_server <- function(id, tab_download) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Define as categorias de nível 2, conforme as categorias
    # de nível 1 selecionadas
    observeEvent(input$categ_1, ignoreNULL = FALSE, {
      
      categs <- input$categ_1
      
      categorias_nivel2 <- tab_categorias %>%
        dplyr::select(nm_categoria_nv1, id, nm_categoria_nv2) %>%
        dplyr::filter(nm_categoria_nv1 %in% categs) %>% 
        dplyr::collect() %>%
        dplyr::group_by(nm_categoria_nv1) %>%
        tidyr::nest(data = c(id), nomes = c(nm_categoria_nv2))  %>%
        with(purrr::map2(
          data,
          nomes,
          function(x, y){aux <- x[[1]]; names(aux) <- y[[1]];return(aux)}
        ))
      
      nomes <- categorias_nivel2 %>%
        purrr::map(names) %>%
        purrr::flatten_chr() %>% 
        wrap_text()
      names(categorias_nivel2) <- categs
      
      shinyWidgets::updatePickerInput(
        session,
        "categ_2",
        choices = categorias_nivel2,
        selected = unlist(categorias_nivel2),
        choicesOpt = list(content = nomes)
      )
    })
    
    # Cria o slider de ano. O slider permite animação 
    # se o gráfico for um sankey.
    output$ui_slider <- renderUI({
      ano_max <- ano_maximo()
      ano_selecionado <- isolate(input$ano)
      periodo_selecionado <- isolate(input$periodo)
      if(input$vis_escolhida == "sankey") {
        if (!isTruthy(ano_selecionado)) {
          ano_selecionado <- 2013 
        }
        shinyWidgets::sliderTextInput(
          ns("ano"),
          label = "Ano",
          choices = 2013:ano_max,
          selected = ano_selecionado,
          animate = animationOptions(interval = 1000),
          width = "100%",
          grid = FALSE
        )
      } else {
        if (!isTruthy(periodo_selecionado)) {
          periodo_selecionado <- c(2013, ano_max) 
        }
        shinyWidgets::sliderTextInput(
          ns("periodo"),
          label = "Período",
          choices = 2013:ano_max,
          selected = periodo_selecionado,
          width = "95%",
          grid = FALSE
        )
      }
    })
    
    # Server do módulo que permite baixar os dados.
    mod_int_baixar_dados_server(
      "int_baixar_dados_ui_1", 
      tab_download
    )
    
  })
}
