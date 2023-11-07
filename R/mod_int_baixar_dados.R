#' int_baixar_dados UI Function
#'
#' @description Módulo que cria o botão e a lógica de backend para baixar os
#' dados.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param tab Um data.frame contendo a tabela que será baixada pelo usuário. 
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_int_baixar_dados_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(
      ns("baixar_dados"),
      label = "",
      width = "100%"
    ),
    tippy::tippy_this(
      ns("baixar_dados"), 
      tooltip = "Clique para baixar os dados que geram esta visualização.",
      arrow = TRUE
    )
  )
}
    
#' int_baixar_dados Server Functions
#'
#' @noRd 
mod_int_baixar_dados_server <- function(id, tab) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
 
    output$baixar_dados <- downloadHandler(
      filename = "dados_panorama_epe.csv",
      content = function(file) {
        tab %>% 
          dplyr::select(where(~!is.list(.x))) %>% 
          write.csv2(file = file)
      }
    )
    
  })
}
    