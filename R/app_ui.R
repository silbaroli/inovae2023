#' The application User-Interface, touching
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources test
    golem_add_external_resources(),
    bs4Dash::bs4DashPage(
      title = "Panorama dos Investimentos em PD&D em Energia no Brasil",
      sidebar_collapsed = FALSE,
      sidebar_mini = TRUE,
      navbar = bs4Dash::bs4DashNavbar(
        # class = "fixed-top",
        compact = TRUE,
        uiOutput("titulo")
      ),
      sidebar = bs4Dash::bs4DashSidebar(
        skin = "light",
        # url = "https://www.epe.gov.br/",
        # src = "www/logo_epe.png",
        elevation = 0,
        bs4Dash::bs4SidebarMenu(
          id = "tabs",
          bs4Dash::bs4SidebarMenuItem(
            "Evolução dos investimentos",
            icon = "chart-line",
            tabName = "evolucao_investimentos_ui_1"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Distribuição dos investimentos",
            icon = "lightbulb",
            tabName = "dist_investimentos_ui_1"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Natureza e modalidade",
            icon = "file-invoice-dollar",
            bs4Dash::bs4SidebarMenuSubItem(
              "Visão geral",
              icon = "calendar-alt",
              tabName = "natureza_visao_geral_ui_1" 
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Por tecnologia energética nv1",
              icon = "angle-right",
              tabName = "natureza_tecno_nv1_ui_1" 
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Por tecnologia energética nv2",
              icon = "angle-double-right",
              tabName = "natureza_tecno_nv2_ui_1" 
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Por instituição fomentadora",
              icon = "file-signature",
              tabName = "natureza_fomento_ui_1" 
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Instituições fomentadoras",
            icon = "users",
            bs4Dash::bs4SidebarMenuSubItem(
              "Visão geral",
              icon = "calendar-alt",
              tabName = "agentes_visao_geral_ui_1"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Por tecnologia energética nv1",
              icon = "angle-right",
              tabName = "agentes_tecno_nv1_ui_1" 
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              "Por tecnologia energética nv2",
              icon = "angle-double-right",
              tabName = "agentes_tecno_nv2_ui_1" 
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Perfil dos projetos",
            icon = "file",
            tabName = "mod_perfil_projetos_ui_1"
            ),                
            bs4Dash::bs4SidebarMenuItem(
              "Explorar projetos",
              icon = "binoculars",
              tabName = "explorar_projetos_ui_1"
            )
        )
      ),
      body = bs4Dash::bs4DashBody(
        # uiOutput("bread_pitt"),
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "evolucao_investimentos_ui_1",
            mod_evolucao_investimentos_ui_1("evolucao_investimentos_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "dist_investimentos_ui_1",
            mod_dist_investimentos_ui_1("dist_investimentos_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "natureza_visao_geral_ui_1",
            mod_natureza_visao_geral_ui("natureza_visao_geral_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "natureza_tecno_nv1_ui_1",
            mod_natureza_tecno_ui("natureza_tecno_nv1_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "natureza_tecno_nv2_ui_1",
            mod_natureza_tecno_ui("natureza_tecno_nv2_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "natureza_fomento_ui_1",
            mod_natureza_fomento_ui("natureza_fomento_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "agentes_visao_geral_ui_1",
            mod_agentes_visao_geral_ui("agentes_visao_geral_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "agentes_tecno_nv1_ui_1",
            mod_agentes_tecno_ui("agentes_tecno_nv1_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "agentes_tecno_nv2_ui_1",
            mod_agentes_tecno_ui("agentes_tecno_nv2_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "mod_perfil_projetos_ui_1",
            mod_perfil_projetos_ui("mod_perfil_projetos_ui_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "explorar_projetos_ui_1",
            mod_explorar_projetos_ui("explorar_projetos_ui_1")
          )
        ),
        tippy::tippy_class(
          "fa-bar-chart",
          content = "Gráfico de barras"
        ),
        tippy::tippy_class(
          "fa-line-chart",
          content = "Gráfico de linhas"
        ),
        tippy::tippy_class(
          "fa-table",
          content = "Tabela"
        ),
        tippy::tippy_class(
          "fa-random",
          content = "Sankey plot"
        ),
        tippy::tippy_class(
          "fa-th-large",
          content = "Gráfico de árvore"
        ),
        tippy::tippy_class(
          "bi-hexagon",
          content = "Gráfico de radar"
        ),
        tippy::tippy_class(
          "fa-dot-circle",
          content = "Gráfico de rosca"
        ),
        tippy::tippy_class(
          "fa-burn",
          content = "Mapa de calor"
        ),
        tippy::tippy_class(
          "fa-area-chart",
          content = "Gráfico de área"
        ),
        div(
          class = "alerta-tela",
          p("Este app foi desenvolvido para funcionar em telas 
            horizontais. Por favor, deixe a tela do seu celular na 
            horizontal para continuar navegando"),
          br(),
          shiny::icon("mobile-alt", class = 'icone-celular'),
          shiny::icon("sync-alt")
        )
      )
      # footer = bs4Dash::dashboardFooter(
      #   copyrights = a(
      #     href = "https://curso-r.com",
      #     target = "_blank",
      #     "Desenvolvido pela Curso-R", 
      #     span(" com ❤️")
      #   ),
      #   right_text = "2021 | epe — Empresa de Pesquisa Energética"
      # )
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Panorama dos Investimentos em PD&D em Energia no Brasil'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shiny::tags$script('$(document).on("shiny:connected", function(e) {var jsWidth = screen.width;Shiny.onInputChange("GetScreenWidth",jsWidth);});'),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/cgee-slider.css"), 
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.4.1/font/bootstrap-icons.css"
    )
  )
}

