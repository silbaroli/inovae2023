#' natureza_por_ano UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_natureza_visao_geral_ui <- function(id){
  ns <- NS(id)
  div(
    class = "section",
    mod_controles_padrao_ui(
      ns("controles_padrao_ui_1"), 
      id,
      choices_vis = c(
        `<i class='fa fa-dot-circle'></i>` = "rosca", 
        `<i class='fa fa-bar-chart'></i>` = "barras", 
        `<i class='fa fa-line-chart'></i>` = "linhas", 
        `<i class='fa fa-table'></i>` = "tabela"
      )
    ),
    br(),
    tabBox(
      id = ns("tab_selecionada"),
      choices = c(
        "Natureza dos investimentos" = "natureza_desc", 
        "Modalidade dos investimentos de natureza pública" = "modalidade_desc"
      ),
      uiOutput(ns("ui_serie"))
    )
  )
}
    
#' natureza_por_ano Server Functions
#'
#' @noRd 
mod_natureza_visao_geral_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_serie()
    )
    
    # UI da visualização escolhida --------------------------------------------
    output$ui_serie <- renderUI({
      if (input[[nsf("vis_escolhida")]] == "rosca") {
        echarts4r::echarts4rOutput(
          ns("plot_rosca"),
          height = parametros()$altura_graficos
        )
      } else if (input[[nsf("vis_escolhida")]] == "barras") {
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
    
    # Tabela reativa filtrada conforme as seleções do usuário -----------------
    tab_filtrada <- reactive({
      
      req(input[[nsf("categ_2")]])
      
      categs <- input[[nsf("categ_2")]]
      anos <- input[[nsf("periodo")]][1]:input[[nsf("periodo")]][2]
      agentes <- input[[nsf("instuicoes")]]
      
      tab <- tab_dispendios %>% 
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
        dplyr::left_join(tab_natureza, by = c("ntz_finan" = "id")) %>%
        dplyr::left_join(tab_modalidade, by = c("mod_finan" = "id")) %>% 
        dplyr::mutate(
          modalidade_desc = ifelse(is.na(modalidade_desc), "Não informado", modalidade_desc)
        )
      
      if (input$tab_selecionada == "modalidade_desc") {
        tab <- tab %>% 
          dplyr::filter(natureza_desc == "Público")  
      }
      
      tab
      
    })
    
    # Tabela utilizada para criar as visualizações para as séries -------------
    tab_serie <- reactive({
      eixo_y <- input[[nsf("eixo_y")]]
      tab_filtrada() %>%
        calcular_eixo_y_serie(
          eixo_y = eixo_y,
          var_cor = input$tab_selecionada
        )
    })
    
    # Plots -------------------------------------------------------------------
    output$plot_rosca <- echarts4r::renderEcharts4r({
      eixo_y <- input[[nsf("eixo_y")]]
      tab_filtrada() %>% 
        calcular_eixo_y_barras(
          eixo_y = eixo_y,
          eixo_x = input$tab_selecionada
        ) %>% 
        dplyr::ungroup() %>% 
        plot_rosca()
    })
    
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
      variavel <- switch (
        isolate(input$tab_selecionada),
        "natureza_desc" = "natureza do investimento", 
        "modalidade_desc" = "modalidade dos investimentos de natureza pública"
      )
      complemento <- paste(
        "por", variavel,
        "e por ano"
      )
      titulo <- criar_titulo_tabela(tab_serie(), complemento)
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_serie <- reactable::renderReactable({
      variavel <- switch (
        isolate(input$tab_selecionada),
        "natureza_desc" = "Natureza", 
        "modalidade_desc" = "Modalidade"
      )
      tab_serie() %>% 
        dplyr::ungroup() %>% 
        tabela_var_anual(nome_grupo = variavel)
    })
    
  })
}
