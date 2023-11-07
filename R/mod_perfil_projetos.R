#' duracao_st7_1 UI Function, touching
#'
#' @description Módulo que gera a aba 'Perfil dos projetos'.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_perfil_projetos_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "section",
    mod_controles_padrao_ui(
      ns("controles_padrao_ui_1"),
      id,
      choices_vis = c(
        `<i class='fa fa-burn'></i>` = "hc_heatmap", 
        `<i class='fa fa-area-chart'></i>` = "hc_histograma", 
        `<i class='fa fa-table'></i>` = "tabela"
      ),
    tipo_eixo_y = "duracao"
    ),
    tabBox(
      id = ns("var_selecionada"),
      choices = c(
        "Tecnologia energética nível 1" = "nm_categoria_nv1", 
        "Instituição fomentadora" = "nme_form",
        "Natureza do investimento" = "natureza_desc",
        "Não agrupar" = ""
      ),
      uiOutput(ns("ui_serie"))
    )
  )
}

#' dispendios_linha_tempo Server Functions
#'
#' @noRd 
mod_perfil_projetos_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Carrega o server dos inputs ---------------------------------------------
    nsf <- NS("controles_padrao_ui_1")
    
    mod_controles_padrao_server(
      "controles_padrao_ui_1",
      tab_serie()
    )
    
    # UI da visualização escolhida --------------------------------------------
    output$ui_serie <- renderUI({
      
      eixo_x <- input[[nsf("eixo_y")]]
      
      #print(input[[nsf("vis_escolhida")]])
      
      if(input[[nsf("vis_escolhida")]] == "hc_heatmap"){
        echarts4r::echarts4rOutput(
          ns("ec_heatmap"),
          height = "600px")
      } else if (input[[nsf("vis_escolhida")]] == "hc_histograma"){
        echarts4r::echarts4rOutput(
          ns("hc_histograma"),
          height = "600px")
      } else if (input[[nsf("vis_escolhida")]] == "tabela"){
        reactable::reactableOutput(
          ns("tabela_perfil")
        )
      }
    })
  
    
    # Tabela utilizada para construir as visualizações ------------------------
    tab_plot <- reactive({
      
      eixo_x <- input[[nsf("eixo_y")]]
      
      req(input[[nsf("categ_2")]])
      
      categs <- input[[nsf("categ_2")]]
      if (length(input[[nsf("periodo")]]) == 1) {
        anos <- input[[nsf("periodo")]]
      }  else {
        anos <- input[[nsf("periodo")]][1]:input[[nsf("periodo")]][2]
      }
      
      agentes <- input[[nsf("instuicoes")]]
      
      tab <- tab_projetos %>% 
        dplyr::ungroup() %>% 
        dplyr::left_join(tab_categorias, by = c("id_cat2" = "id")) %>% 
        dplyr::left_join(tab_natureza, by = c("ntz_finan" = "id")) %>% 
        dplyr::filter(
          id_cat2 %in% categs,
          ano %in% anos,
          nme_form %in% agentes
        ) 
      
      if(input$var_selecionada == ""){
        
        tab %>%
          dplyr::collect() %>% 
          dplyr::mutate(
            porte = santoku::chop(valor_total, c(0, .5, seq(1, 100, 10), Inf), santoku::lbl_format("Entre %.3g e %.3g"))
          ) %>% 
          dplyr::group_by(!!dplyr::sym(eixo_x)) %>% 
          dplyr::summarise(
            valor_total = sum(valor_total, na.rm = TRUE),
            freq = dplyr::n(), .groups = "keep"
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            p = 100*freq/sum(freq),
          ) %>% 
          dplyr::mutate(
            nm_categoria_nv1 = "Todos os projetos"
          )
        
      } else {
        X <- tab %>%
          dplyr::collect() %>% 
          dplyr::mutate(
            porte = santoku::chop(valor_total, c(0, .5, seq(1, 100, 10), Inf), santoku::lbl_format("Entre %.3g e %.3g"))
          ) %>% 
          dplyr::group_by(!!dplyr::sym(eixo_x), !!dplyr::sym(input$var_selecionada)) %>% 
          dplyr::summarise(
            valor_total = sum(valor_total, na.rm = TRUE),
            freq = dplyr::n(), .groups = "keep"
          ) %>% 
          dplyr::ungroup() %>% 
          tidyr::complete(!!dplyr::sym(input$var_selecionada), !!dplyr::sym(eixo_x), fill = list(freq = 0)) %>%
          dplyr::group_by(!!dplyr::sym(input$var_selecionada))
        
        X %>% 
          dplyr::mutate(
            total = sum(freq),
            p = 100*freq/total
          ) %>% 
          dplyr::ungroup()
      }
      
    })
    
    # Plots -------------------------------------------------------------------
    output$ec_heatmap <- echarts4r::renderEcharts4r({
      
      eixo_x <- input[[nsf("eixo_y")]]
      xlab <- ifelse(
        eixo_x == "porte",
        "Porte (milhões de reais)",
        "Duração do projeto (anos)"
      )
      
      
      if(input$var_selecionada == ""){
        
        tab_plot() %>% 
          dplyr::mutate(
            x = !!dplyr::sym(eixo_x),
            y = "Todos os projetos",
            v = p
          ) %>% 
          dplyr::mutate(
            x = as.character(x),
            y = as.character(y)
          ) %>%
          ec_heatmap(xlab)
        
      } else {
       
        tab_plot() %>% 
          dplyr::mutate(
            x = !!dplyr::sym(eixo_x),
            y = !!dplyr::sym(input$var_selecionada),
            v = p
          ) %>% 
          dplyr::mutate(
            x = as.character(x),
            y = as.character(y)
          ) %>%
          ec_heatmap(xlab)
         
      }
    })
    
    output$hc_histograma <- echarts4r::renderEcharts4r({
      eixo_x <- input[[nsf("eixo_y")]]
      
      texto_eixo_x <- ifelse(eixo_x == "duracao_anos", "Duração (anos)", "Valor total do projeto (R$)")
      
      print(input$var_selecionada)
      if(input$var_selecionada == ""){
        grafico <- tab_plot() %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(
            x = !!dplyr::sym(eixo_x),
            y = freq/sum(freq),
            GRUPO = !!dplyr::sym(input$var_selecionada),
            COR = !!dplyr::sym(input$var_selecionada)
          ) %>% 
          dplyr::ungroup() %>% 
          ec_hist() 
      } else {
        grafico <- tab_plot() %>% 
          dplyr::mutate(
            x = !!dplyr::sym(eixo_x),
            y = p,
            GRUPO = !!dplyr::sym(input$var_selecionada),
            COR = !!dplyr::sym(input$var_selecionada)
          ) %>% 
          dplyr::group_by(GRUPO) %>% 
          ec_hist()
      }
      
      grafico %>% 
        echarts4r::e_x_axis(name = texto_eixo_x, nameLocation = 'center') %>% 
        echarts4r::e_y_axis(name = "% dos projetos", nameLocation = 'center')
        
    })
    
    # Tabelas -----------------------------------------------------------------
    output$tabela_perfil <- reactable::renderReactable({
      
      eixo_x <- input[[nsf("eixo_y")]]
      
      nome_coluna <- dplyr::case_when(
        input$var_selecionada == "nm_categoria_nv1" ~ "Tecnologia energética nível 1",
        input$var_selecionada == "nme_form" ~ "Instituição fomentadora",
        input$var_selecionada == "natureza_desc" ~ "Natureza do investimento",
        TRUE ~ ""
      )
      
      tab_plot() %>% 
        dplyr::select(
          !!dplyr::sym(eixo_x),
          y = !!dplyr::sym(input$var_selecionada),
          freq,
          p
        ) %>% 
        dplyr::mutate(
          valor = scales::percent(p / 100, accuracy = 0.1)
        ) %>% 
        dplyr::select(-freq, -p) %>% 
        tidyr::pivot_wider(values_from = valor, names_from = eixo_x
        ) %>% 
        reactable::reactable(
          wrap = FALSE,
          pagination = FALSE,
          striped = TRUE,
          sortable = FALSE,
          columns = list(
            y = reactable::colDef(
              align = "left",
              name = nome_coluna,
              width = 300
            )
          ),
          defaultColDef = reactable::colDef(align = "right", na = "0"),
          class = "tabela",
          theme = reactable::reactableTheme(
            backgroundColor = "#fff",
            stripedColor = "#e5e5e5",
            headerStyle = list(
              backgroundColor = "var(--corPrincipal);",
              color = "#fff"
            )
          )
        )
    })
    
})}

