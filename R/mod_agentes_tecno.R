#' agentes_tecno UI Function
#'
#' @description Módulo que gera a sub-aba 'Por tecnologia energética nv1'
#' e 'Por tecnologia energética nv2'.
#' Essa sub-aba está abaixo do item 'Instituições fomentadoras' no menu.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_agentes_tecno_ui <- function(id){
  ns <- NS(id)
  div(
    class = "section",
    mod_controles_padrao_ui(
      ns("controles_padrao_ui_1"),
      id,
      choices_vis = c(
        `<i class='fa fa-bar-chart'></i>` = "barras", 
        `<i class='bi bi-hexagon'></i>` = "radar",
        `<i class='fa fa-random'></i>` = "sankey_",
        `<i class='fa fa-table'></i>` = "tabela"
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("ui_vis"))
      )
    )
  )
}
    
#' agentes_tecno Server Functions
#'
#' @noRd 
mod_agentes_tecno_server <- function(id){
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
      } else if (input[[nsf("vis_escolhida")]] == "sankey_") {
        echarts4r::echarts4rOutput(
          ns("plot_sankey"),
          height = parametros()$altura_graficos
        )
      } else if (input[[nsf("vis_escolhida")]] == "tabela") {
        tagList(
          uiOutput(ns("tabela_dist_titulo")),
          reactable::reactableOutput(ns("tabela_dist"))
        )
      }
    })
    

    # Tabela reativa filtrada conforme as seleções do usuário -----------------
    tab_filtrada <- reactive({
      
      req(input[[nsf("categ_2")]])
      
      categs <- input[[nsf("categ_2")]]
      anos <- input[[nsf("periodo")]][1]:input[[nsf("periodo")]][2]
      agentes <- input[[nsf("instuicoes")]]
      
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
        dplyr::left_join(tab_categorias, by = c("id_cat2" = "id"))
        # Descomentar as linhas abaixo caso haja interesse em incluir 
        # as instituicoes executores e proponentes no app
        # dplyr::left_join(tab_executor, by = c("id_exec" = "id_agente_exec")) %>% 
        # dplyr::left_join(tab_proponente, by = c("id_prop" = "id_agente_prop"))
      
    })
    
    

    # Tabela que é usada para plotar as distribuicoes -------------------------
    tab_dist <- reactive({
      eixo_y <- input[[nsf("eixo_y")]]
      if (stringr::str_detect(id, "_nv1_")) {
        x <- "nm_categoria_nv1"
      } else if (stringr::str_detect(id, "_nv2_")) {
        x <- "nm_categoria_nv2"
      }
      tab_filtrada() %>% 
        calcular_eixo_y_barras_emp(
          eixo_y = eixo_y,
          eixo_x = x,
          var_cor = "nme_form"
        ) %>% 
        dplyr::mutate(grupo = tidyr::replace_na(grupo, "Sem informação"))
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
    
    output$plot_sankey <- echarts4r::renderEcharts4r({
      eixo_y <- input[[nsf("eixo_y")]]
      if (stringr::str_detect(id, "_nv1_")) {
        x <- "nm_categoria_nv1"
      } else if (stringr::str_detect(id, "_nv2_")) {
        x <- "nm_categoria_nv2"
      }
      
      if (isolate(input[[nsf("periodo")]][1]) != 
          isolate(input[[nsf("periodo")]][2])) {
        periodo <- paste(
          isolate(input[[nsf("periodo")]][1]),
          "a",
          isolate(input[[nsf("periodo")]][2])
        )
      } else {
        periodo <- isolate(input[[nsf("periodo")]][1])
      }
      
      tab_filtrada() %>%
        calcular_eixo_y_barras_emp(
          eixo_y,
          "nme_form", 
          x
        ) %>%
        plot_sankey(ano = periodo)
    })
    

    # Tabelas -----------------------------------------------------------------
    output$tabela_dist_titulo <- renderUI({
      
      variavel <- "instituição fomentadora"
      
      # Descomentar caso seja necessário incluir instituicoes fomentadoras
      # e executoras
      # variavel <- switch (
      #   isolate(input$tab_selecionada),
      #   "nme_form" = "instituição fomentadora", 
      #   "ntz_agente_prop" = "instituição proponente",
      #   "ntz_agente_exec" = "instituição executora"
      # )
      
      complemento <- paste(
        "por", variavel,
        "e por tecnologia energética"
      )
      
      titulo <- criar_titulo_tabela(tab_dist(), complemento)
      h4(class = "titulo-tabela", titulo)
    })
    
    output$tabela_dist <- highcharter::renderHighchart({
      tabela_dispendios_agrup(tab_dist())
    })
  })
}
