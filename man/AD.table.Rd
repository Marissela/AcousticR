% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{AD.table}
\alias{AD.table}
\title{Tablas de descriptores acústicos de cardúmenes}
\usage{
AD.table(datos, sp, var, var.names, path.save)
}
\arguments{
\item{datos}{data-frame con los datos de las regiones a analizar.}

\item{sp}{vector con la especie a analizar. Las especies se
consideran: anchoveta (ANC), munida (MUN), jurel (JUR), vinciguerria
(VIN), pota (POT), bagre (BAG), caballa (CAB) y samasa (SAM).}

\item{var}{vector con los nombres de las variables a analizar}

\item{var.names}{vector con los nombres de las variables tal como se
desea que aparezcan en la tabla}

\item{path.save}{ruta de donde guardar el archivo}
}
\description{
Resumen de los estadísticos principales para los descriptores acústicos a analizar.
}
\details{
Se genera una tabla con los estadísticos principales para las variables
seleccionadas (i.e. mínimo, máximo, promedio, desviación estandar y coeficiente
de variación).

Si \code{sp = 'ANC'}, se obtienen 3 tablas, cada una de ellas de acuerdo a las regiones
de pesca de la especie (i.e. región norte, centro y sur). Para las otras especies,
los resultados son obtenidos de manera global en toda el área de estudio.

Las regiones de pesca de anchoveta son consideradas de la siguiente manera:
\describe{
\item{region1}{Región norte, menor a 9.5°S}
\item{region2}{Región centro, entre 9.5 a 16°S}
\item{region3}{Región sur, mayor igual a 16°S}
}
}
