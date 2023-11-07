#' A partir de uma tabela pronta para colocar num grafico, retorna o formatador
#' correspondente
#'
#' @param tab Uma tibble produzida por uma função calcular_eixo_**
#' @param JS Variavel logica. O retorno da funcao deve ser um código
#' em JavaScript ou o nome de uma função declarada em "www/script.js"?
#'
#' @return String contendo um código JS para ser executado posteriormente
escolher_fomatador <- function(tab, JS = TRUE) {
  if(JS) {
    if(any(stringr::str_detect(tab$y_format, stringr::fixed("R$")))) {
      htmlwidgets::JS("function(value, index){return formatarDinheiro(value, 0);}")
    } else if (any(stringr::str_detect(tab$y_format, "%"))){
      htmlwidgets::JS("function(value, index){return formatarPorcentagem(value);}")
    } else {
      htmlwidgets::JS("function(value, index){return formatarNumero(value);}")
    }
  } else {
    if(any(stringr::str_detect(tab$y_format, stringr::fixed("R$")))) {
      "formatarDinheiro"
    } else if (any(stringr::str_detect(tab$y_format, "%"))){
      "formatarPorcentagem"
    } else {
      "formatarNumero"
    }
  }
  
}

#' Tooltip do echarts4r
#'
#' @description A função e_tooltip do echarts4r não permite que se passe os
#' parametros adicionais diretamente nos ... Essa função modifica o comportamento
#' da função original para que seja possivel fazer e_tooltip(e, "axis", list(...))
#'
#' @param e An echarts4r object as returned by e_charts or a proxy as returned by echarts4rProxy.
#' @param trigger What triggers the tooltip, one of item or item.
#' @param ... Any other option to pass.
#'
#' @return
e_tooltip <- function (e, trigger = c("item", "axis"), ...) 
{
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  tooltip <- list(trigger = trigger[1], ...)
  if (!e$x$tl) {
    e$x$opts$tooltip <- tooltip
  }
  else {
    e$x$opts$baseOption$tooltip <- tooltip
  }
  e
}

# Histograma --------------------------------------------------------------

#' Constrói um gráfico de área em echarts4r
#'
#' @param tab Uma tibble com colunas x e y. Caso essa coluna esteja agrupada, os gráficos
#' de área serão construídos respeito esse agrupamento.
#'
#' @return Um objeto do tipo echarts4r plotando y contra x, respeitando eventuais agrupamentos
#' do parâmetro tab.
ec_hist <- function(tab){
  
  mostrar_legenda <- dplyr::is.grouped_df(tab)
  
  tamanho_da_fonte <- parametros()$tamanho_da_fonte
  tab %>% 
    echarts4r::e_chart(x) %>% 
    echarts4r::e_area(y, smooth = TRUE) %>% 
    echarts4r::e_grid(
      containLabel = TRUE,
      top = parametros()$altura_grid
    ) %>% 
    echarts4r::e_y_axis(
      nameTextStyle = list(
        fontSize = tamanho_da_fonte,
        padding = 5
      ),
      nameLocation = "center",
      nameGap = 45,
      axisTick = list(
        show = FALSE
      ),
      axisLabel = list(
        fontSize = tamanho_da_fonte 
      ) 
    ) %>% 
    echarts4r::e_x_axis(
      axisLabel = list(
        fontSize = tamanho_da_fonte,
        margin = 15
      ),
      nameLocation = "center",
      nameGap = 40,
      fontSize = tamanho_da_fonte,
      axisTick = list(
        show = FALSE
      ),
      nameTextStyle = list(
        fontSize = tamanho_da_fonte,
        padding = 5
      )
    ) %>% 
    echarts4r::e_legend(
      show = mostrar_legenda,
      type = "scroll",
      textStyle = list(
        fontSize = tamanho_da_fonte
      ),
      top = "top",
      symbol_size = 1,
      itemGap = 15,
      itemWidth = 12,
      selectedMode = FALSE
    ) %>% 
    e_tooltip(
      trigger = "item",
      formatter = "{a}"
    )
  
}

# Heatmap -----------------------------------------------------------------

#'  Constrói um heatmap em echarts4r
#'
#' @param tab Uma tibble com colunas x, y e v. x e y representam os valores dos
#' eixos x e y, respectivamente. v representa a cor do heatmap
#'
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna v
ec_heatmap <- function(tab, xlab) {
  
  tooltip = htmlwidgets::JS("function(params){
                                  return(parseFloat((params.value[2] * 10) / 10).toFixed(1) + '% dos projetos em <br>' + params.value[1] + '<br> Duram até ' + params.value[0] + ' ano(s)')
                              }")
  
  tamanho_da_fonte <- parametros()$tamanho_da_fonte
  
  tab %>% 
    echarts4r::e_charts(x) %>% 
    echarts4r::e_heatmap(y, v) %>% 
    echarts4r::e_visual_map(
      v, 
      inRange = list(color = cores_epe()[1:2]), 
      precision = 2,
      calculable = FALSE
    ) %>% 
    echarts4r::e_grid(
      containLabel = TRUE,
      left = 30,
      right = 10
    ) %>% 
    echarts4r::e_y_axis(
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function(value, index){return wordWrap2(value, 30);}"
        ),
        fontSize = parametros()$tamanho_da_fonte
      ),
      axisTick = list(show = FALSE)
    ) %>% 
    echarts4r::e_x_axis(
      axisTick = list(show = FALSE),
      name = xlab,
      nameLocation = "center",
      nameGap = 25,
      nameTextStyle = list(
        fontSize = parametros()$tamanho_da_fonte,
        padding = 5
      )) %>% 
    e_tooltip(
      trigger = "item",
      formatter = tooltip
    ) 
}

# Series ------------------------------------------------------------------

#' Constrói um série de tempo de barras empilhadas em echarts4r
#'
#' @param tab Uma tabela saída de calcular_eixo_y_serie
#'
#' @return Um objeto do tipo echarts4r plotando y contra ano, pintado pela coluna grupo
#' 
plot_serie_barras <- function(tab) {
  ylab <- unique(tab$ylab)
  
  tamanho_da_fonte <- parametros()$tamanho_da_fonte
  
  fun_format <- escolher_fomatador(tab, FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        var text = params.value[0] + '<br>';
        text += params.marker + ' ' + params.seriesName 
        text += ': <b>' + formatarNumero(params.value[1]) + '</b>';
        return text;
        }",
      .open = "{{",
      .close = "}}"
    )
  )
  
  formatador <- escolher_fomatador(tab)
  
  tab %>% 
    dplyr::group_by(grupo) %>% 
    echarts4r::e_charts(ano) %>% 
    echarts4r::e_bar(y, stack = "grupo") %>% 
    e_tooltip(
      formatter = tooltip
    ) %>% 
    echarts4r::e_y_axis(
      nameTextStyle = list(
        fontSize = tamanho_da_fonte,
        padding = 5
      ),
      name = ylab,
      nameLocation = "center",
      nameGap = 65,
      axisLabel = list(
        formatter = formatador,
        fontSize = tamanho_da_fonte 
      ) 
    ) %>%
    echarts4r::e_x_axis(
      axisLabel = list(
        fontSize = tamanho_da_fonte,
        margin = 15
      ),
      fontSize = tamanho_da_fonte,
      axisTick = list(
        show = FALSE
      )
    ) %>% 
    echarts4r::e_animation(duration = 0) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
  
}

#' Constrói um série de linhas do tempo em echarts4r
#'
#' @param tab Uma tabela saída de calcular_eixo_y_serie
#'
#' @return Um objeto do tipo echarts4r plotando y contra ano, pintado pela coluna grupo
#' 
plot_serie_linhas <- function(tab) {
  ylab <- unique(tab$ylab)

  tamanho_da_fonte <- parametros()$tamanho_da_fonte
  
  fun_format <- escolher_fomatador(tab, FALSE)
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        var text = params.value[0] + '<br>';
        text += params.marker + ' ' + params.seriesName 
        text += ': <b>' + formatarNumero(params.value[1]) + '</b>';
        return text;
        }",
      .open = "{{",
      .close = "}}"
    )
  )
  
  formatador <- escolher_fomatador(tab)
  
  tab %>% 
    dplyr::group_by(grupo) %>% 
    echarts4r::e_charts(ano) %>% 
    echarts4r::e_line(y, symbolSize = 10) %>% 
    e_tooltip(
      formatter = tooltip
    ) %>% 
    echarts4r::e_y_axis(
      name = ylab,
      nameLocation = "center",
      nameGap = 65,
      nameTextStyle = list(
        fontSize = tamanho_da_fonte,
        padding = 5
      ),
      axisLabel = list(
        formatter = formatador,
        fontSize = tamanho_da_fonte 
      )
    ) %>% 
    echarts4r::e_x_axis(
      axisLabel = list(
        fontSize = tamanho_da_fonte,
        margin = 15
      ),
      fontSize = tamanho_da_fonte,
      axisTick = list(
        show = FALSE
      )
    ) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
  
}

# sankey ------------------------------------------------------------------

#' Constrói um sankey plot em echarts4r
#'
#' @param tab Um tibbl produzida pela função calcular_eixo_y_barras_emp
#' @param height Altura do gráfico. Parâmetro passado para a função e_grid
#' @param ano String contendo o ano que será exibido na tooltip
#' @param animation Booleana. O gráfico executará uma animação ao ser construído?
#' @param definir_cores Booleana. O gráfico deve usar o vetor cores_epe()
#'
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
#' @examples
plot_sankey <- function(tab, height = "95%", ano = "", 
                        animation = TRUE, definir_cores = FALSE) {
  
  ylab <- unique(tab$ylab)
  fun_format <- escolher_fomatador(tab, FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        if (params.dataType === 'edge') {
          var header = '<b>{{ylab}}</b><br>';
          header += '<b><p>{{ano}} — ' + params.data.source +  '</p></b>';
          var text = header + wordWrap(params.data.target, 30) + ': <b>' + params.value + '</b>'; 
        } else if (params.dataType === 'node') {
          var header = '<b>{{ylab}}</b><br>';
          header += '<b><p>{{ano}}</p></b>';
          var text = header + params.marker + wordWrap(params.name, 30) + ': <b>' + params.value + '</b>';
        }
        return text;
        }",
      .open = "{{",
      .close = "}}",
    )
  )
  
  tab1 <- tab %>% 
    dplyr::group_by(x) %>% 
    dplyr::summarise(
      total_investido = "Total investido",
      y_total = sum(y, na.rm = TRUE)
    )
  
  tab_sankey <- tibble::tibble(
    source = c(tab1$total_investido, tab$x),
    target = c(tab1$x, tab$grupo),
    value = c(tab1$y_total, tab$y)
  )
  
  p <- tab_sankey %>% 
    echarts4r::e_charts() %>% 
    echarts4r::e_sankey(
      source = source,
      target = target,
      value = value,
      animation = animation,
      label = list(
        position = "left",
        formatter = htmlwidgets::JS(
          "function(params){return formatarTexto(params.name);}"
        )
      ),
      nodeGap = 15,
      nodeWidth = 8,
      left = "85px",
      right = "1%"
    ) %>% e_tooltip(
      trigger = "item",
      formatter = tooltip
    ) %>% 
    echarts4r::e_grid(
      top = 0,
      height = height,
      containLabel = TRUE
    )
  
  if (definir_cores) {
    tab_cores <- tab_sankey %>%
      dplyr::filter(source != "Total investido") %>% 
      dplyr::distinct(source) %>%
      dplyr::mutate(
        cor = cores_epe()[1:dplyr::n()]
      ) %>%
      dplyr::right_join(tab_sankey, by = "source") %>%
      dplyr::filter(!is.na(cor)) %>% 
      tidyr::pivot_longer(
        cols = c(source, target),
        names_to = "var",
        values_to = "name"
      ) %>% 
      dplyr::distinct(name, cor) %>% 
      tibble::add_row(
        name = "Total investido",
        cor = "purple"
      )

    lista_cores <- purrr::map2(
      tab_cores$name,
      tab_cores$cor,
      ~ list(name = .x, itemStyle = list(color = .y))
    )

    p$x$opts$series[[1]]$data <- lista_cores

    p
  } else {
    p
  }
  
}

# rosca -------------------------------------------------------------------

#' Constrói um gráfico de rosca em echarts4r
#'
#' @param tab Tibble saída da função calcular_eixo_y_barras
#'
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
plot_rosca <- function(tab) {
  
  fun_format <- escolher_fomatador(tab, FALSE)
  
  explicacao_pub <- "Os investimentos públicos de PD&D são calculados a partir dos dispêndios em projetos de PD&D reembolsáveis e não-reembolsáveis realizados por meio de instituições públicas de fomento à inovação no Brasil. Nas estatísticas apresentadas nesta plataforma fazem parte do escopo dos investimentos públicos em PD&D os seguintes órgãos federais: BNDES, CNEN, CNPq, FINEP; e também do estado de São Paulo: FAPESP."
  explicacao_pub_ori <- "Os investimentos publicamente orientados se referem ao investimento privado induzido por políticas públicas, sendo compulsório para as empresas do setor de energia. São recursos que se enquadram dentro de programas públicos cuja finalidade é induzir as empresas a efetuarem investimentos em PD&D. Nas estatísticas apresentadas nesta plataforma fazem parte do escopo os projetos de P&D regulados pelas agências ANEEL e ANP."
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        var header = '<b>' + params.name + '</b><br>';
        if (params.name === 'Público') {
          var sobre = '<p>{{explicacao_pub}}</p>';
        } else if (params.name === 'Publicamente orientado') {
          var sobre = '<p>{{explicacao_pub_ori}}</p>';
        } else {
          var sobre = '';
        }
        
        return header + wordWrap(sobre, 30);
      }",
      .open = "{{",
      .close = "}}",
    )
  )
  
  tab %>% 
    echarts4r::e_charts(x) %>% 
    echarts4r::e_pie(
      y, 
      radius = c("30%", "70%"),
      label = list(
        normal = list(
          formatter = "{d}%",
          position = "inside",
          fontWeight = "bold",
          fontSize = 18
        )
      )
    ) %>% 
    e_tooltip(
      trigger = "item",
      formatter = tooltip
    ) %>% 
    echarts4r::e_legend(
      selectedMode = FALSE,
      textStyle = list(
        fontSize = parametros()$tamanho_da_fonte
      )
    ) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
}


# treemap -----------------------------------------------------------------

#' Constrói um treemap em echarts4r
#'
#' @param tab Tibble contendo as colunas name, children e value
#' @param height Altura do gráfico. Parâmetro passado para a função e_grid
#' 
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
plot_arvore <- function(tab, height = "95%", periodo = "") {
  ylab <- unique(tab$children[[1]]$ylab)
  
  fun_format <- tab %>% 
    dplyr::select(-name, -value) %>% 
    tidyr::unnest(cols = c(children)) %>% 
    escolher_fomatador(FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        var header = '<b>{{ylab}}</b><br>';
        var periodo = 'Período: {{periodo}}<br><br>'
        var valor = wordWrap(params.name) + ': <b>' +  {{fun_format}}(params.value) + '</b>';
        var text = wordWrap(header) + periodo + valor;
        return text;
       }",
      .open = "{{",
      .close = "}}",
    )
  )
  
  num_cores <- dplyr::n_distinct(tab$name)
  cores <- cores_epe()[1:num_cores]
  
  tab <- tab %>% 
    dplyr::mutate(itemStyle = tibble::tibble(color = cores))
  
  tab %>% 
    echarts4r::e_charts() %>% 
    echarts4r::e_treemap(
      roam = FALSE,
      width = "100%",
      height = "90%",
      leafDepth = 1,
      drillDownIcon = "",
      label = list(
        fontSize = parametros()$tamanho_da_fonte
      )
    ) %>% 
    echarts4r::e_grid(height = height)%>% 
    e_tooltip(
      trigger = "item",
      formatter = tooltip
    ) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
}


# radar -------------------------------------------------------------------

#' Constrói um gráfico de radar em echarts4r
#'
#' @param tab Tibble saída da função agrupar_tecnologias_barras
#' @param height Altura do gráfico. Parâmetro passado para a função e_grid
#' 
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
plot_radar <- function(tab, height = "95%") {
  
  ylab <- unique(tab$ylab)
  fun_format <- escolher_fomatador(tab, FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        text = '<p><b>{{ylab}}</b></p>';
        for (var i = 0; i < params.data.name.length; i++) {
          var xx = params.data.name[i] + ': <b>' + {{fun_format}}(params.data.value[i]) + '</b><br>';
          text += xx;
        }
        return text;
       }",
      .open = "{{",
      .close = "}}",
    )
  )
  
  maximo <- max(tab$y)
  
  opts <- list(
    radar = list(
      indicator = purrr::map(tab$x, ~list(name = .x, max = maximo)),
      name = list(
        formatter = htmlwidgets::JS(
        "function(value, index){
          return wordWrap2(value, 20);
        }"
        )
      ),
      radius = list("0%", "65%")
    ),
    tooltip = list(
      trigger = "item",
      formatter = tooltip
    ),
    series = list(
      list(
        name = "y",
        type = "radar",
        data = list(list(name = tab$x, value = tab$y))
      )
    ),
    textStyle = list(
      color = 'black',
      fontSize = parametros()$tamanho_da_fonte
    )
  )
  
  echarts4r::e_charts() %>% 
    echarts4r::e_list(opts) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
  
}

#' Constrói um gráfico de radar agrupado em echarts4r
#'
#' @param tab Tibble saída da função agrupar_tecnologias_barras
#' @param height Altura do gráfico. Parâmetro passado para a função e_grid
#' 
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
plot_radar_grupo <- function(tab, height = "95%") {
  
  ylab <- na.omit(unique(tab$ylab))
  fun_format <- escolher_fomatador(tab, FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        var text = '<p><b>{{ylab}}</b></p>';
        for (var i = 0; i < params.data.value.length; i++) {
          var xx = params.marker + wordWrap(params.data.x[i]) + ': <b>' + {{fun_format}}(params.data.value[i]) + '</b><br>';
          text += xx;
        }
        return text;
       }",
      .open = "{{",
      .close = "}}",
    )
  )
  
  variaveis <- unique(tab$grupo)
  maximo <- max(tab$y)
  
  tab_plot <- tab %>% 
    dplyr::ungroup() %>% 
    dplyr::select(grupo, y, x) %>% 
    tidyr::pivot_wider(names_from = grupo, values_from = y) %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, replace = 0))
  
  opts <- list(
    radar = list(
      indicator = purrr::map(unique(tab$x), ~list(name = .x, max = maximo)),
      name = list(
        formatter = htmlwidgets::JS(
          "function(value, index){
            return wordWrap2(value, 20);
          }"
        )
      ),
      radius = list("0%", "65%")
    ),
    tooltip = list(
      trigger = "item",
      formatter = tooltip
    ),
    legend = list(
      type = "scroll", 
      show = TRUE, 
      selectedMode = FALSE,
      top = "top",
      symbol_size = 1,
      itemGap = 15,
      itemWidth = 12,
      textStyle = list(
        fontSize = parametros()$tamanho_da_fonte
      )
    ),
    series = list(
      list(
        name = "y",
        type = "radar",
        data = purrr::map(
          variaveis,
          ~list(name = .x, x = tab_plot$x, value = tab_plot[[.x]])
        )
      )
    ),
    textStyle = list(
      color = 'black',
      fontSize = parametros()$tamanho_da_fonte
    )
  )
  
  echarts4r::e_charts() %>% 
    echarts4r::e_list(opts) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
  
  
  # p <- tab_plot %>% 
  #   echarts4r::e_charts(x) %>% 
  #   echarts4r::e_grid(
  #     top = 0,
  #     height = height
  #   )
  # 
  # for(v in variaveis) {
  #   p <- do.call(echarts4r::e_radar, list(p, as.symbol(v), max = maximo))
  # }
  # 
  # p
}


# barras ------------------------------------------------------------------


#' Constrói um gráfico de barras em echarts4r
#'
#' @param tab Tibble saída da função calcular_eixo_y_barras
#' 
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
plot_dist_barras <- function(tab) {
  
  tamanho_da_fonte <- parametros()$tamanho_da_fonte
  
  ylab <- unique(tab$ylab)
  fun_format <- escolher_fomatador(tab, FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
      var valor = {{fun_format}}(params[0].value[0]);
      var header = '<p><b>{{ylab}}</b></p>'; 
      var text = params[0].value[1] + ': <b>' + valor + '</b>';
      return header + text; 
      }",
      .open = "{{",
      .close = "}}",
    )
  )
  formatador <- escolher_fomatador(tab)
  
  
  tab %>% 
    echarts4r::e_charts(y) %>% 
    echarts4r::e_bar(x, legend = FALSE) %>% 
    echarts4r::e_y_axis(
      type = "category",
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function(value, index){return formatarTexto(value);}"
        ),
        fontSize = tamanho_da_fonte 
      )
    ) %>% 
    e_tooltip(
      trigger = "axis",
      formatter = tooltip
    ) %>% 
    echarts4r::e_x_axis(
      name = ylab,
      nameLocation = "center",
      nameTextStyle = list(
        fontSize = tamanho_da_fonte,
        padding = 5
      ),
      nameGap = 40,
      axisLabel = list(
        formatter = formatador,
        fontSize = tamanho_da_fonte,
        margin = 15
      ),
      fontSize = tamanho_da_fonte
    ) %>% 
    echarts4r::e_grid(
      bottom = "10%",
      top = "5%",
      left = 0,
      right = "5%",
      containLabel = TRUE
    ) %>% 
    echarts4r::e_color(
      color = cores_epe()
    )
  
}

#' Constrói um gráfico de barras empilhadas em echarts4r
#'
#' @param tab Tibble saída da função calcular_eixo_y_barras_emp
#' 
#' @return Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
#'
plot_dist_barras_emp <- function(tab, height = "80%") {
  
  ylab <- unique(tab$ylab)
  fun_format <- escolher_fomatador(tab, FALSE)
  
  tooltip <- htmlwidgets::JS(
    glue::glue(
      "function (params) {
        var text = params.value[1] + '<br>';
        text += params.marker + ' ' + params.seriesName 
        text += ': <b>' + formatarNumero(params.value[0]) + '</b>';
        return text;
        }",
      .open = "{{",
      .close = "}}"
    )
  )
  formatador <- escolher_fomatador(tab)
  
  tamanho_da_fonte <- parametros()$tamanho_da_fonte
  
  tab %>% 
    dplyr::group_by(grupo) %>% 
    echarts4r::e_charts(y) %>% 
    echarts4r::e_bar(x, stack = "grupo") %>% 
    echarts4r::e_y_axis(
      type = "category",
      axisLabel = list(
        formatter = htmlwidgets::JS("function(value, index){return formatarTexto(value);}"),
        fontSize = tamanho_da_fonte
      )
    ) %>% 
    e_tooltip(
      formatter = tooltip
    ) %>% 
    echarts4r::e_animation(duration = 0) %>% 
    echarts4r::e_x_axis(
      name = ylab,
      nameLocation = "center",
      nameGap = 40,
      nameTextStyle = list(
        fontSize = tamanho_da_fonte,
        padding = 5
      ),
      axisLabel = list(
        formatter = formatador,
        fontSize = tamanho_da_fonte,
        margin = 15
      )
    ) %>% 
    echarts4r::e_legend(
      selectedMode = FALSE,
      textStyle = list(
        fontSize = parametros()$tamanho_da_fonte
      )
    ) %>% 
    echarts4r::e_grid(
      containLabel = TRUE,
      left = 0,
      right = "10%"
    ) %>%  
    echarts4r::e_color(
      color = cores_epe()
    )
}

