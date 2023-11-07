#' dispendios_linha_tempo UI Function
#'
#' @description Módulo que gera a aba 'Evolução dos
#' investimentos'.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evolucao_investimentos_ui_1 <- function(id) {
  ns <- NS(id)
  div(
    class = "section",
    mod_controles_padrao_ui(
      ns("controles_padrao_ui_1"),
      id,
      choices_vis = c(
        `<i class='fa fa-bar-chart'></i>` = "barras", 
        `<i class='fa fa-line-chart'></i>` = "linhas",
        `<i class='fa fa-random'></i>` = "sankey",
        `<i class='fa fa-table'></i>` = "tabela"
      )
    ),
    uiOutput(ns("ui_vis"))
  )
}

#' dispendios_linha_tempo Server Functions
#'
#' @noRd 
mod_evolucao_investimentos_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_serie()
    )
    
    # UI da visualização escolhida --------------------------------------------
    tab_box <- function(...) {
      tabBox(
        id = ns("tab_selecionada"),
        choices = c(
          "Tecnologia energética nível 1" = "nm_categoria_nv1", 
          "Tecnologia energética nível 2" = "nm_categoria_nv2",
          "Não agrupar" = "Não agrupar"
        ),
        ...
      )
    }
    
    output$ui_vis <- renderUI({
      if (input[[nsf("vis_escolhida")]] == "barras") {
        tab_box(
          echarts4r::echarts4rOutput(
            ns("plot_serie_barras"),
            height = parametros()$altura_graficos
          )
        )
      } else if (input[[nsf("vis_escolhida")]] == "linhas") {
        tab_box(
          echarts4r::echarts4rOutput(
            ns("plot_serie_linhas"),
            height = parametros()$altura_graficos
          )
        )
      } else if (input[[nsf("vis_escolhida")]] == "tabela") {
        tab_box(
          uiOutput(ns("tabela_serie_titulo")),
          reactable::reactableOutput(ns("tabela_serie"))
        )
      } else if (input[[nsf("vis_escolhida")]] == "sankey") {
        fluidRow(
          column(
            width = 12,
            echarts4r::echarts4rOutput(
              ns("plot_sankey"),
              height = parametros()$altura_graficos
            )
          )
        )
      }
    })
    
    # Tabela utilizada para construir as visualizações da série temporal ------
    tab_serie <- reactive({
      
      req(input[[nsf("categ_2")]])
      req(input[[nsf("periodo")]])
      
      categs <- input[[nsf("categ_2")]]
      anos <- input[[nsf("periodo")]][1]:input[[nsf("periodo")]][2]
      eixo_y <- input[[nsf("eixo_y")]]
      
      agentes <- input[[nsf("instuicoes")]]
      
      if (input$tab_selecionada == "Não agrupar") {
        agrup <- NULL
      } else {
        agrup <- input$tab_selecionada
      }
      
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
        calcular_eixo_y_serie(
          eixo_y = eixo_y,
          var_cor = agrup
        )
      
    })
    
    # Tabela utilizada para construir o sankey plot ---------------------------
    tab_sankey <- reactive({
      req(input[[nsf("categ_2")]])
      req(input[[nsf("ano")]])
      
      validate(need(
        input[[nsf("eixo_y")]] != "porcPIB",
        "Visualização não disponível para o indicador escolhido."
      ))
      
      categs <- input[[nsf("categ_2")]]
      ano_selecionado <- input[[nsf("ano")]]
      eixo_y <- input[[nsf("eixo_y")]]
      
      agentes <- input[[nsf("instuicoes")]]
      
      tab_dispendios %>% 
        dplyr::left_join(
          tab_fomentador,
          by = c("id_formnt" = "id_formentador")
        ) %>% 
        dplyr::filter(
          id_cat2 %in% categs,
          ano %in% ano_selecionado,
          nme_form %in% agentes
        ) %>% 
        dplyr::left_join(tab_categorias, by = c("id_cat2" = "id")) %>% 
        calcular_eixo_y_barras_emp(
          eixo_y,
          "nm_categoria_nv1", 
          "nm_categoria_nv2"
        )
    })
    
    # Plots -------------------------------------------------------------------
    
    legenda_serie <- function(p) {
      p %>% 
        echarts4r::e_legend(
          textStyle = list(
            fontSize = parametros()$tamanho_da_fonte
          ),
          formatter = htmlwidgets::JS(
            "function(name) {
          if (window.innerWidth <= 700) {
            return wordWrap2(name, 40);
          } else {
            return name;
          }
        }" 
          ),
          left = "4%",
          top = "60%",
          orient = "vertical",
          height = "40%",
          symbol_size = 1,
          itemGap = 15,
          itemWidth = 12,
          selectedMode = FALSE
        ) %>% 
        echarts4r::e_grid(
          containLabel = TRUE,
          height = "55%",
          top = 15
        ) 
    }
    
    output$plot_serie_barras <- echarts4r::renderEcharts4r({
      p <- tab_serie() %>%
        agrupar_tecnologias_serie() %>% 
        plot_serie_barras()
      
      if (isolate(input$tab_selecionada) != "Não agrupar") {
        legenda_serie(p)
      } else {
        p %>% 
          echarts4r::e_legend(
            show = FALSE
          ) %>% 
          echarts4r::e_grid(
            containLabel = TRUE
          )
      }
    })
    
    output$plot_serie_linhas <- echarts4r::renderEcharts4r({
      p <- tab_serie() %>%
        agrupar_tecnologias_serie() %>%
        plot_serie_linhas()
      
      if (isolate(input$tab_selecionada) != "Não agrupar") {
        legenda_serie(p)
      } else {
        p %>% 
          echarts4r::e_legend(
            show = FALSE
          ) %>% 
          echarts4r::e_grid(
            containLabel = TRUE
          )
      }
    })
    
    output$plot_sankey <- echarts4r::renderEcharts4r({
      ano <- isolate(input[[nsf("ano")]])
      tab_sankey() %>%
        plot_sankey(ano = ano, animation = FALSE, definir_cores = TRUE)
    })
    
    # Tabelas -----------------------------------------------------------------
    output$tabela_serie_titulo <- renderUI({
      titulo <- criar_titulo_tabela(tab_serie(), "por ano")
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_serie <- reactable::renderReactable({
      tabela_var_anual(tab_serie())
    })
    
    
  })
}

