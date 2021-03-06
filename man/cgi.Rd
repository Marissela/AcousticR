% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cgi.R
\name{cgi}
\alias{cgi}
\title{Cálculo del centro de gravedad, inercia e isotropía}
\usage{
cgi(x, y, z = NA, w = NA, modproj = NA, mlong = NA, mlat = NA, plot = F,
polig.color = NULL, path.save, name.file)
}
\arguments{
\item{x}{vector de x-coordenadas}

\item{y}{vector de y-coordenadas}

\item{z}{vector correspondiente a la variable regionalizada en 2D.
De faltar, los resultados de 'cgi' solo abarcaran a las muestras.}

\item{w}{peso o área de influencia (opcional)}

\item{modproj}{indica el tipo de proyección a realizar (opcional)}

\item{mlong}{longitud media en grados de la base de datos a ser transformada.}

\item{mlat}{latitud media en grados de la base de datos a ser transformada.}

\item{plot}{si \code{plot = T}, la elipse correspondiente a la inercia se añadirá
automáticamente en una gráfica ya existente.}

\item{polig.color}{si \code{plot = T}, se utilizará este color para la elipse
añadida al gráfico.}

\item{path.save}{ruta en donde se guardarán los gráficos y resultados.}

\item{name.file}{nombre del archivo final con los resultados de la función.}
}
\value{
El resultado consiste en una lista que incluye:
\item{cg}{coordenadas del centro de gravedad}
\item{dc.CG}{distancia a la costa del centro de gravedad, expresado en millas naúticas}
\item{dsb.CG}{distancia a la plataforma continental del centro de gravedad,
expresado en millas naúticas. El signo del valor representa el lado donde se encuentra
ubicado el punto con respecto a la plataforma, positivo cuando se encuentra fuera
y negativo cuando se encuentra dentro de la plataforma}
\item{I}{valor de la incercia alrededor del centro de gravedad}
\item{Imax}{valor de la incercia de acuerdo con el primer eje
principal de la inercia}
\item{Imin}{valor de la incercia de acuerdo con el segundo eje
principal de la inercia}
\item{Iso}{valor de la isotropía}
\item{xaxe1, yaxe1}{coordenadas del primer eje principal de la inercia}
\item{xaxe2, yaxe2}{coordenadas del segundo eje principal de la inercia}
}
\description{
Cálculo de índices espaciales: centro de gravedad, inercia
e isotropía para un conjunto de datos georeferenciados.
}
\details{
Desarrollo basado en la rutina del Geostatistics for Estimating Fish Abundance (GEFA)
& EU program Fisboat, DG-Fish

Authors : M.Woillez (Mines-ParisTech), N.Bez (IRD)
and J.Rivoirard (Mines-ParisTech)
}
\references{
Woillez, M., Rivoirard, J., & Petitgas, P. (2009).
Notes on survey-based spatial indicators for monitoring fish populations.
Aquatic Living Resources, 22(2), 155–164. https://doi.org/10.1051/alr/2009017
}
