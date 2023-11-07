#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #test
  # Highcharter options
  lang <- getOption("highcharter.lang")
  lang$decimalPoint <- ","
  lang$numericSymbols <- c("null", "M", "B", "T", "null", "null")
  options(highcharter.lang = lang)
  
  output$titulo <- renderUI({
    tab <- tab_textos_intro %>% 
      dplyr::filter(id == input$tabs)
    
    titulo <- tab %>% 
      dplyr::pull(titulo)
    
    subtitulo <- dplyr::pull(tab, subtitulo)
    subtitulo <- ifelse(is.na(subtitulo), " ", subtitulo)
    
    tagList(
      h2(titulo),
      h5(subtitulo)
    )
    
  })
  
  mod_evolucao_investimentos_server("evolucao_investimentos_ui_1")
  mod_dist_investimentos_server("dist_investimentos_ui_1")
  mod_natureza_visao_geral_server("natureza_visao_geral_ui_1")
  mod_natureza_tecno_server("natureza_tecno_nv1_ui_1")
  mod_natureza_tecno_server("natureza_tecno_nv2_ui_1")
  mod_natureza_fomento_server("natureza_fomento_ui_1")
  mod_agentes_visao_geral_server("agentes_visao_geral_ui_1")
  mod_agentes_tecno_server("agentes_tecno_nv1_ui_1")
  mod_agentes_tecno_server("agentes_tecno_nv2_ui_1")
  mod_perfil_projetos_server("mod_perfil_projetos_ui_1")
  mod_explorar_projetos_server("explorar_projetos_ui_1")
}
