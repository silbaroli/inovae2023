#' natureza_tecno UI Function
#'
#' @description Módulo que gera as sub-abas 'Por tecnologia energética nv1' e
#' 'Por tecnologia energética nv2'. Essa sub-aba está abaixo do item 
#' 'Natureza e modalidade' no menu.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_natureza_tecno_ui <- function(id){
  ns <- NS(id)
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
      uiOutput(ns("ui_serie"))
    )
  )
}

#' natureza_tecno Server Functions
#'
#' @noRd 
mod_natureza_tecno_server <- function(id) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_dist()
    )
    
    # UI da visualização escolhida --------------------------------------------
    output$ui_serie <- renderUI({
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
      
      if (stringr::str_detect(id, "_nv1_")) {
        x <- "nm_categoria_nv1"
      } else if (stringr::str_detect(id, "_nv2_")) {
        x <- "nm_categoria_nv2"
      }

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
          modalidade_desc = ifelse(
            is.na(modalidade_desc),
            "Não informado",
            modalidade_desc
          )
        )
      
      if (input$tab_selecionada == "modalidade_desc") {
        tab <- tab %>% 
          dplyr::filter(natureza_desc == "Público")  
      }
      
      tab %>%
        calcular_eixo_y_barras_emp(
          eixo_y = eixo_y,
          eixo_x = x,
          var_cor = input$tab_selecionada
        )
      
    })
    
    # Plots -------------------------------------------------------------------
    output$plot_dist_barras <- echarts4r::renderEcharts4r({
      plot_dist_barras_emp(tab_dist())
    })
    
    output$plot_radar <- highcharter::renderHighchart({
      tab_dist() %>%
        agrupar_tecnologias_barras_agrup() %>%
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
        "por", variavel
      )
      titulo <- criar_titulo_tabela(tab_dist(), complemento)
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_dist <- highcharter::renderHighchart({
      tabela_dispendios_agrup(tab_dist())
    })
    
    
  })
}

