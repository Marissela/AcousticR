% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Vdist.R
\name{Vdist}
\alias{Vdist}
\title{Gráficos de distribución vertical}
\usage{
Vdist(datos, sp, xlim = c(3,18), ylim = c(0,100,10), plot.type = 1)
}
\arguments{
\item{datos}{data-frame con los datos de las regiones a analizar.}

\item{sp}{vector con la(s) especie(s) a analizar. Las especies se
consideran: anchoveta (ANC), munida (MUN), jurel (JUR), vinciguerria
(VIN), pota (POT), bagre (BAG), caballa (CAB) y samasa (SAM).}

\item{xlim}{rango de latitud (valores positivos). Por default se
considera el rango latitudinal del Perú.}

\item{ylim}{rango de profundidad (valores positivos). El último
valor corresponde al paso que se desea considerar para el eje y.}

\item{plot.type}{Tipo de gráfico a realizar. Por default, \code{plot.type = 1}
realiza gráficos de latitud vs profundidad. Si, \code{plot.type = 2}, se
genera el gráfico de horas del día vs profundidad.}
}
\description{
Gráficos de distribución vertical
}
\details{
Se realizan dos subplots acerca del comportamiento vertical de la(s)
especie(s) a analizar.

Para gráficos en donde se analiza solo una especie se incluye la línea de la
profundidad media por grado de latitud.
}
