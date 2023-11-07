#' explorar_projetos UI Function
#'
#' @description Módulo que gera a aba 'Explorar projetos".
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_explorar_projetos_ui <- function(id){
  ns <- NS(id)
  ano_max <- ano_maximo()
  div(
    class = "section",
    fluidRow(
      class = "align-items-center filtros",
      column(
        class = "slider col-12 col-md-5 col-lg-5 col-xl-5",
        width = 12,
        shinyWidgets::sliderTextInput(
          ns("periodo"),
          label = "Período",
          choices = 2013:ano_max,
          selected = c(2013, ano_max),
          width = "80%",
          grid = FALSE
        ),
        br(class = "hidden-md-up")
      ),
      column(
        width = 8,
        class = "dd-filtros offset-md-1 col-8 col-md-4 col-lg-4 col-xl-3",
        style = "position: static;",
        dropdown_filtros(id)
      ),
      column(
        width = 2,
        class = "text-right disclaimer col-2 col-md-1 col-lg-1 col-xl-2",
        style = "position: static;",
        aviso_secao(id)
      ),
      column(
        class = "text-right col-2 col-md-1 col-lg-1 col-xl-1",
        width = 2,
        mod_int_baixar_dados_ui(ns("int_baixar_dados_ui_1"))
      )
    ),
    br(),
    br(),
    fluidRow(
      column(
        width = 12,
        shinyWidgets::addSpinner(
          reactable::reactableOutput(
            ns("tab_projetos"),
            height = 715
          ),
          color = "var(--corPrincipal);"
        )
      )
    )
  )
}
    
#' explorar_projetos Server Functions
#'
#' @noRd 
mod_explorar_projetos_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Define as categorias de nível 2, conforme as categorias
    # de nível 1 selecionadas
    observeEvent(input$categ_1, {
      
      categs <- input$categ_1
      
      categorias_nivel2 <- tab_categorias %>% 
        dplyr::select(nm_categoria_nv1, id, nm_categoria_nv2) %>% 
        dplyr::filter(nm_categoria_nv1 %in% categs) %>% 
        dplyr::collect() %>% 
        dplyr::group_by(nm_categoria_nv1) %>%
        tidyr::nest(data = c(id), nomes = c(nm_categoria_nv2))  %>%
        with(purrr::map2(
          data, 
          nomes, 
          function(x, y){aux <- x[[1]]; names(aux) <- y[[1]];return(aux)}
        ))
      
      names(categorias_nivel2) <- categs
      
      shinyWidgets::updatePickerInput(
        session,
        "categ_2",
        choices = categorias_nivel2,
        selected = unlist(categorias_nivel2)
      )
    })
    # Tabela reativa filtrada conforme as seleções do usuário -----------------
    tab_filtrada <- reactive({
      
      req(input$categ_2)
      
      categs <- input$categ_2
      anos <- input$periodo[1]:input$periodo[2]
      agentes <- input$instuicoes
      
      tab_dispendios %>% 
        dplyr::left_join(
          tab_fomentador,
          by = c("id_formnt" = "id_formentador")
        ) %>% 
        dplyr::filter(
          ano %in% anos,
          id_cat2 %in% categs,
          nme_form %in% agentes
        ) %>% 
        dplyr::left_join(tab_executor, by = c("id_exec" = "id_agente_exec")) %>% 
        dplyr::left_join(tab_proponente, by = c("id_prop" = "id_agente_prop")) %>% 
        dplyr::left_join(tab_categorias, by = c("id_cat2" = "id"))
      
    })
    
    # Tabela de projetos ------------------------------------------------------
    tabela <- reactive({
      tab_projetos %>% 
        dplyr::select(-nme_form, -ntz_finan, -id_projeto) %>% 
        dplyr::right_join(tab_filtrada(), by = "id_item") %>%
        dplyr::distinct(
          id_item, titulo,
          dta_inicio, dta_limite, duracao_anos,
          situacao,
          nme_form, nme_agente_prop, nme_agente_exec,
          valor_total
        ) %>% 
        dplyr::collect() %>% 
        dplyr::filter(nme_form != "FAPESP") %>% 
        dplyr::arrange(dplyr::desc(valor_total)) %>% 
        dplyr::mutate(
          valor_total_formatado = formatar_numero(valor_total),
          dta_inicio = as.Date(dta_inicio),
          dta_inicio = format(dta_inicio, "%d/%m/%Y"),
          dta_limite = stringr::str_sub(dta_limite, 1, 10),
          dta_limite = as.Date(dta_limite),
          dta_limite = format(dta_limite, "%d/%m/%Y"),
          dplyr::across(.fns = ~ifelse(.x == "NA", NA, .x)),
          dplyr::across(where(is.character), tidyr::replace_na, replace = "Sem informação")
        ) 
    })
    
    output$tab_projetos <- reactable::renderReactable({
      input_name_gatilho <- ns("gatilho")
      input_name_linha <- ns("tab_linha")
      tabela() %>%
        dplyr::select(
          id_item, titulo,
          nme_form,
          dta_inicio,
          valor_total_formatado
        ) %>% 
        dplyr::mutate(detalhes = NA) %>% 
        tabela_projetos(
          pagination = TRUE,
          onclick = reactable::JS(
            glue::glue(
              "function(rowInfo, colInfo) {
                  if (colInfo.id !== 'detalhes') {
                     return
                  }
                  Shiny.setInputValue('{{input_name_gatilho}}', Math.random());
                  Shiny.setInputValue('{{input_name_linha}}', rowInfo.index);
                }", 
              .open = "{{", .close = "}}"
            )
          ),
          colunas = list(
            id_item = reactable::colDef(
              name = "ID",
              width = 100
            ),
            titulo = reactable::colDef(
              name = "Projeto",
              width = 200
            ),
            nme_form = reactable::colDef(
              name = "Agente fomentador",
              align = "center"
            ),
            dta_inicio = reactable::colDef(
              name = "Data de início",
              align = "center"
            ),
            valor_total_formatado = reactable::colDef(
              name = "Valor (milhões de reais)",
              align = "right"
            ), 
            detalhes = reactable::colDef(
              name = "",
              sortable = FALSE,
              align = "center",
              width = 50,
              cell = function() htmltools::tags$button(icon("search")) 
            )
          )
        )
      
    })
    
    # Popup com detalhes de cada projeto. Aparece na tela quando a usuária 
    # clica na lupa, na última coluna da tabela.
    observeEvent(input$gatilho, {
      
      tab <- tabela() %>% 
        dplyr::slice(input$tab_linha + 1)
      
      id_proj <- tab$id_item
      fomentador <- tab$nme_form
      proponente <- tab$nme_agente_prop
      executor <- tab$nme_agente_exec
      
      tab_proj_agentes <- tab_filtrada() %>% 
        dplyr::filter(nme_form == fomentador) %>% 
        dplyr::left_join(tab_projetos, by = "id_item") %>%
        dplyr::distinct(
          id_item, titulo, valor_total, nme_agente_prop, nme_agente_exec
        ) %>% 
        dplyr::filter(id_item != id_proj) %>% 
        dplyr::collect() %>% 
        dplyr::arrange(dplyr::desc(valor_total)) %>% 
        dplyr::mutate(
          valor_total_formatado = formatar_numero(valor_total),
          dplyr::across(.fns = ~ifelse(.x == "NA", NA, .x)),
          dplyr::across(.fns = tidyr::replace_na, replace = "Sem informação")
        )
      
      tab_prop <- tab_proj_agentes %>% 
        dplyr::filter(nme_agente_prop == proponente) %>% 
        dplyr::select(
          id_item,
          titulo,
          `Agente proponente` = nme_agente_prop,
          valor_total_formatado
        )
      
      tab_exec <- tab_proj_agentes %>% 
        dplyr::filter(nme_agente_exec == executor) %>% 
        dplyr::select(
          id_item,
          titulo,
          `Agente executor` = nme_agente_exec,
          valor_total_formatado
        )
      
      if (!is.na(tab$duracao_anos)) {
        ano_s <- ifelse(tab$duracao_anos == 1, "ano", "anos")
        duracao <- paste(tab$duracao_anos, ano_s)
      } else {
        duracao <- "Sem informação"
      }
      
      showModal(modalDialog(
        title = tab$titulo,
        class = "detalhe-projetos",
        fluidRow(
          column(
            width = 12,
            p_info("Agente fomentador", tab$nme_form),
            p_info("Agente proponente", tab$nme_agente_prop),
            p_info("Agente executor", tab$nme_agente_exec),
            p_info("Duração", duracao),
            p_info("Valor total (em milhões de reais)", formatar_dinheiro(tab$valor_total))
          )
        ),
        fluidRow(
          bs4Dash::bs4Card(
            width = 12,
            closable = FALSE,
            collapsed = TRUE,
            title = paste0(
              fomentador, ": outros projetos com esse mesmo agente proponente"
            ),
            tab_projetos_min(tab_prop)
          ),
          bs4Dash::bs4Card(
            width = 12,
            closable = FALSE,
            collapsed = TRUE,
            title = paste0(
              fomentador, ": outros projetos com esse mesmo agente executor"
            ),
            tab_projetos_min(tab_exec)
          )
        ),
        footer = modalButton("Fechar"),
        size = "l",
        easyClose = TRUE
      ))
    })
    
    # Server do módulo de baixar dados
    mod_int_baixar_dados_server("int_baixar_dados_ui_1", tab_filtrada())
    
  })
}
    