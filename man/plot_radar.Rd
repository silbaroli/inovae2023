% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_graficos.R
\name{plot_radar}
\alias{plot_radar}
\title{Constrói um gráfico de radar em echarts4r}
\usage{
plot_radar(tab, height = "95\%")
}
\arguments{
\item{tab}{Tibble saída da função agrupar_tecnologias_barras}

\item{height}{Altura do gráfico. Parâmetro passado para a função e_grid}
}
\value{
Um objeto do tipo echarts4r plotando y contra x, pintado pela coluna grupo
}
\description{
Constrói um gráfico de radar em echarts4r
}
