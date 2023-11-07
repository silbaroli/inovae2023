#' agentes_visao_geral UI Function
#'
#' @description Módulo que gera a sub-aba 'Visão geral'.
#' Essa sub-aba está abaixo do item 'Instituições fomentadoras' no menu.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_agentes_visao_geral_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "section",
    mod_controles_padrao_ui(ns("controles_padrao_ui_1"), id),
    br(),
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("ui_vis"))
      )
    )
  )
}

#' agentes_visao_geral Server Functions
#'
#' @noRd 
mod_agentes_visao_geral_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_serie()
    )
    
    # UI da visualização escolhida --------------------------------------------
    output$ui_vis <- renderUI({
      if (input[[nsf("vis_escolhida")]] == "barras") {
        echarts4r::echarts4rOutput(
          ns("plot_serie_barras"),
          height = parametros()$altura_graficos
        )
      } else if (input[[nsf("vis_escolhida")]] == "linhas") {
        echarts4r::echarts4rOutput(
          ns("plot_serie_linhas"),
          height = parametros()$altura_graficos
        )
      } else if (input[[nsf("vis_escolhida")]] == "tabela") {
        tagList(
          uiOutput(ns("tabela_serie_titulo")),
          reactable::reactableOutput(ns("tabela_serie"))
        )
      }
    })
    
    # Tabela utilizada para construir as visualizações ------------------------
    tab_serie <- reactive({
      
      req(input[[nsf("categ_2")]])
      
      categs <- input[[nsf("categ_2")]]
      anos <- input[[nsf("periodo")]][1]:input[[nsf("periodo")]][2]
      agentes <- input[[nsf("instuicoes")]]
      eixo_y <- input[[nsf("eixo_y")]]
      
      tab_dispendios %>% 
        dplyr::left_join(
          tab_fomentador,
          by = c("id_formnt" = "id_formentador")
        ) %>% 
        dplyr::filter(
          id_cat2 %in% categs,
          ano %in% anos,
          nme_form %in% agentes
        ) %>% 
        dplyr::left_join(tab_categorias, by = c("id_cat2" = "id")) %>% 
        # Descomentar as linhas abaixo caso haja interesse em incluir 
        # as instituicoes executores e proponentes no app
        # dplyr::left_join(tab_executor, by = c("id_exec" = "id_agente_exec")) %>% 
        # dplyr::left_join(tab_proponente, by = c("id_prop" = "id_agente_prop")) %>%
        calcular_eixo_y_serie(
          eixo_y = eixo_y,
          var_cor = "nme_form"
        ) %>% 
        dplyr::mutate(grupo = tidyr::replace_na(grupo, "Sem informação"))
      
    })
    
    # Plots -------------------------------------------------------------------
    output$plot_serie_barras <- echarts4r::renderEcharts4r({
      tab_serie() %>%
        plot_serie_barras() %>% 
        echarts4r::e_legend(
          selectedMode = FALSE,
          textStyle = list(
            fontSize = parametros()$tamanho_da_fonte
          )
        ) %>% 
        echarts4r::e_grid(
          containLabel = TRUE
        )
    })
    
    output$plot_serie_linhas <- echarts4r::renderEcharts4r({
      tab_serie() %>%
        plot_serie_linhas() %>% 
        echarts4r::e_legend(
          selectedMode = FALSE,
          textStyle = list(
            fontSize = parametros()$tamanho_da_fonte
          )
        ) %>% 
        echarts4r::e_grid(
          containLabel = TRUE
        )
    })
    
    # Tabelas -----------------------------------------------------------------
    output$tabela_serie_titulo <- renderUI({
      
      variavel <- "instituição fomentadora"
      
      # Descomentar as linhas abaixo caso haja interesse em incluir 
      # as instituicoes executores e proponentes no app
      # variavel <- switch (
      #   isolate(input$tab_selecionada),
      #   "nme_form" = "instituição fomentadora", 
      #   "ntz_agente_prop" = "instituição proponente",
      #   "ntz_agente_exec" = "instituição executora"
      # )
      
      complemento <- paste(
        "por", variavel,
        "e por ano"
      )
      titulo <- criar_titulo_tabela(tab_serie(), complemento)
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_serie <- reactable::renderReactable({
      tabela_var_anual(tab_serie(), nome_grupo = "Instituição")
    })
    
  })
}

