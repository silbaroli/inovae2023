#' dispendios_vis_detalhada UI Function
#'
#' @description Módulo que gera a aba 'Distribuição dos
#' investimentos'.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dist_investimentos_ui_1 <- function(id) {
  ns <- NS(id)
  div(
    class = "section",
    mod_controles_padrao_ui(
      ns("controles_padrao_ui_1"),
      id,
      choices_vis = c(
        `<i class='fa fa-th-large'></i>` = "arvore", 
        `<i class='fa fa-bar-chart'></i>` = "barras", 
        `<i class='bi bi-hexagon'></i>` = "radar", 
        `<i class='fa fa-table'></i>` = "tabela"
      )
    ),
    br(),
    uiOutput(ns("ui_vis"))
  )
}

#' dispendios_vis_detalhada Server Functions
#'
#' @noRd 
mod_dist_investimentos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_dist()
    )
    
    # UI da visualização escolhida --------------------------------------------
    tab_box <- function(...) {
      tabBox(
        id = ns("tab_selecionada"),
        choices = c(
          "Tecnologia energética nível 1" = "nm_categoria_nv1", 
          "Tecnologia energética nível 2" = "nm_categoria_nv2"
        ),
        ...
      )
    }
    
    output$ui_vis <- renderUI({
      if (input[[nsf("vis_escolhida")]] == "arvore") {
        fluidRow(
          column(
            width = 12,
            echarts4r::echarts4rOutput(
              ns("plot_arvore"),
              height = parametros()$altura_graficos
            )
          )
        )
      }  else if (input[[nsf("vis_escolhida")]] == "barras") {
        tab_box(
          echarts4r::echarts4rOutput(
            ns("plot_dist_barras"),
            height = parametros()$altura_graficos
          )
        )
      } else if (input[[nsf("vis_escolhida")]] == "radar") {
        tab_box(
          echarts4r::echarts4rOutput(
            ns("plot_radar"),
            height = parametros()$altura_graficos
          )
        )
      } else if (input[[nsf("vis_escolhida")]] == "tabela") {
        tab_box(
          uiOutput(ns("tabela_dist_titulo")),
          reactable::reactableOutput(ns("tabela_dist"))
        )
      }
    })
    
    # Tabela utilizada para construir o gráfico de barras e gráfico radar -----
    tab_dist <- reactive({
      
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
        calcular_eixo_y_barras(
          eixo_y = eixo_y,
          eixo_x = input$tab_selecionada
        )
      
    })
    
    # Tabela utilizada para construir o treemap -------------------------------
    tab_arvore <- reactive({
      req(input[[nsf("categ_2")]])
      
      validate(need(
        input[[nsf("eixo_y")]] != "porcPIB",
        "Visualização não disponível para o indicador escolhido."
      ))
      
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
        calcular_eixo_y_barras(
          eixo_y,
          "nm_categoria_nv2"
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-nm_categoria_nv2) %>% 
        dplyr::left_join(
          tab_categorias %>% 
            dplyr::select(dplyr::starts_with("nm_categ")) %>% 
            dplyr::collect(),
          by = c("x" = "nm_categoria_nv2")
        ) %>% 
        dplyr::group_by(nm_categoria_nv1) %>%
        dplyr::mutate(val = sum(y)) %>%
        dplyr::select(x, val, name = x, value = y, ylab, y_format) %>% 
        tidyr::nest(children = c(name, value, ylab, y_format)) %>% 
        dplyr::rename(name = nm_categoria_nv1, value = val) %>% 
        dplyr::ungroup()
        
    })
    
    # Plots -------------------------------------------------------------------
    output$plot_dist_barras <- echarts4r::renderEcharts4r({
      plot_dist_barras(tab_dist())
    })
    
    output$plot_arvore <-  echarts4r::renderEcharts4r({
      periodo <- isolate(input[[nsf("periodo")]])
      if (length(periodo) > 1) {
        periodo <- paste0(periodo[1], " a ", periodo[2])
      }
      plot_arvore(tab_arvore(), periodo = periodo)
    })
    
    
    output$plot_radar <- highcharter::renderHighchart({
      tab_dist() %>%
        agrupar_tecnologias_barras() %>%
        plot_radar()
    })
    
    # Tabelas -----------------------------------------------------------------
    output$tabela_dist_titulo <- renderUI({
      titulo <- criar_titulo_tabela(tab_dist(), "por tecnologia energética")
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_dist <- highcharter::renderHighchart({
      tabela_dispendios(tab_dist())
    })
    
  })
}
