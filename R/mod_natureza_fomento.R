#' natureza_fomento UI Function
#'
#' @description Módulo que gera a sub-aba 'Por instituição fomentadora'.
#' Essa sub-aba está abaixo do item 'Natureza e modalidade' no menu.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_natureza_fomento_ui <- function(id){
  ns <- NS(id)
  id_tabbox <- ns("TabBox")
  div(
    class = "section",
    mod_controles_padrao_ui(
      ns("controles_padrao_ui_1"),
      id,
      choices_vis = c(
        `<i class='fa fa-bar-chart'></i>` = "barras", 
        `<i class='bi bi-hexagon'></i>` = "radar", 
        `<i class='fa fa-table'></i>` = "tabela"
      )
    ),
    tabBox(
      id = ns("tab_selecionada"),
      choices = c(
        "Natureza dos investimentos" = "natureza_desc", 
        "Modalidade dos investimentos de natureza pública" = "modalidade_desc"
      ),
      uiOutput(ns("ui_vis"))
    )
  )
}
    
#' natureza_fomento Server Functions
#'
#' @noRd 
mod_natureza_fomento_server <- function(id) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_dist()
    )
    
    # UI da visualização escolhida --------------------------------------------
    output$ui_vis <- renderUI({
      if (input[[nsf("vis_escolhida")]] == "barras") {
        echarts4r::echarts4rOutput(
          ns("plot_dist_barras"),
          height = parametros()$altura_graficos
        )
      } else if (input[[nsf("vis_escolhida")]] == "radar") {
        echarts4r::echarts4rOutput(
          ns("plot_radar"),
          height = parametros()$altura_graficos
        )
      } else if (input[[nsf("vis_escolhida")]] == "tabela") {
        tagList(
          uiOutput(ns("tabela_dist_titulo")),
          reactable::reactableOutput(ns("tabela_dist"))
        )
      }
    })
    
    # Tabela utilizada para construir as visualizações ------------------------
    tab_dist <- reactive({
      
      req(input[[nsf("categ_2")]])
      
      categs <- input[[nsf("categ_2")]]
      anos <- input[[nsf("periodo")]][1]:input[[nsf("periodo")]][2]
      agentes <- input[[nsf("instuicoes")]]
      eixo_y <- input[[nsf("eixo_y")]]
      
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
        dplyr::left_join(tab_modalidade, by = c("mod_finan" = "id"))
      
      if (input$tab_selecionada == "modalidade_desc") {
        tab <- tab %>% 
          dplyr::filter(natureza_desc == "Público")  
      }
      
      tab %>%
        calcular_eixo_y_barras_emp(
          eixo_y = eixo_y,
          eixo_x = "nme_form",
          var_cor = input$tab_selecionada
        ) %>% 
        dplyr::mutate(grupo = tidyr::replace_na(grupo, "Não informado"))
      
    })
    
    # Plots -------------------------------------------------------------------
    output$plot_dist_barras <- highcharter::renderHighchart({
      plot_dist_barras_emp(tab_dist())
    })
    
    output$plot_radar <- highcharter::renderHighchart({
      tab_dist() %>% 
        plot_radar_grupo()
    })
    
    # Tabelas -----------------------------------------------------------------
    output$tabela_dist_titulo <- renderUI({
      variavel <- switch (
        isolate(input$tab_selecionada),
        "natureza_desc" = "natureza do investimento", 
        "modalidade_desc" = "modalidade dos investimentos de natureza pública"
      )
      complemento <- paste(
        "por", variavel,
        "e por instituição fomentadora"
      )
      titulo <- criar_titulo_tabela(tab_dist(), complemento)
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_dist <- highcharter::renderHighchart({
      tabela_dispendios_agrup(tab_dist(), "Instituição fomentadora")
    })
    
 
  })
}
    
