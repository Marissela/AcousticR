#' @title Tablas de descriptores acústicos de cardúmenes
#' @description Resumen de los estadísticos principales para los descriptores acústicos a analizar.
#' @usage AD.table(datos, sp = 'ANC', var, var.names, path.save)
#' @param datos data-frame con los datos de las regiones a analizar.
#' @param sp vector con la especie a analizar. Las especies se
#' consideran: anchoveta (ANC), munida (MUN), jurel (JUR), vinciguerria
#' (VIN), pota (POT), bagre (BAG), caballa (CAB) y samasa (SAM).
#' @param var vector con los nombres de las variables a analizar
#' @param var.names vector con los nombres de las variables tal como se
#' desea que aparezcan en la tabla
#' @param path.save ruta de donde guardar el archivo
#' @details
#' Se genera una tabla con los estadísticos principales para las variables
#' seleccionadas (i.e. mínimo, máximo, promedio, desviación estandar y coeficiente
#' de variación).
#'
#' Si \code{sp = 'ANC'}, se obtienen 3 tablas, cada una de ellas de acuerdo a las regiones
#' de pesca de la especie (i.e. región norte, centro y sur). Para las otras especies,
#' los resultados son obtenidos de manera global en toda el área de estudio.
#' @export



AD.table = function(datos, sp = 'ANC', var, var.names, path.save)
{
  ## Especie a analizar
  datos$Region_class = gsub(" ", "", datos$Region_class)

  if (sp == 'ANC')
  {
    dat = datos[which(datos$Region_class == 'ANCP' | datos$Region_class == 'ANCG'),]
    dat$Region_class = "ANC"
  } else {
    dat = datos[which(datos$Region_class == sp),]
  }


  ## Regiones de pesca (anchoveta)
  if (sp == 'ANC')
  {
    region = abs(dat$Lat_M)

    indN = which(region < 9.5)
    indC = which(region >= 9.5 & region < 16)
    indS = which(region >= 16)

    region[indN] = 1
    region[indC] = 2
    region[indS] = 3
  }


  ## Seleccion de variables
  dat = dat[ ,var]


  ## Tablas de descriptores acusticos
  if (sp == 'ANC')
  {
    for (iregion in 1:3)
    {
      ## estadisticos basicos para cada variable
      index = which(region == iregion)

      min         = round(as.vector(apply(dat[index,], 2, min, na.rm = TRUE)), 2)
      prom        = round(as.vector(apply(dat[index,], 2, mean, na.rm = TRUE)), 2)
      max         = round(as.vector(apply(dat[index,], 2, max, na.rm = TRUE)), 2)
      varianza    = apply(dat[index,], 2, var, na.rm = TRUE)
      sd          = round(as.vector(varianza^(1/2)),2)
      CV          = round(as.vector(sd/prom), 2)

      names(dat) = var.names

      stats = data.frame(Descriptores = names(dat), Minimo = min,
                         Maximo = max, Promedio = prom, SD = sd,
                         CV = CV)

      ## guardar tabla
      file = paste(path.save, 'descriptores_', sp, '_region', iregion, '.csv', sep = '')
      utils::write.csv(stats, file, row.names = F)
    }
  } else{
    ## estadisticos basicos para cada variable
    min         = round(as.vector(apply(dat, 2, min, na.rm = TRUE)), 2)
    prom        = round(as.vector(apply(dat, 2, mean, na.rm = TRUE)), 2)
    max         = round(as.vector(apply(dat, 2, max, na.rm = TRUE)), 2)
    varianza    = apply(dat, 2, var, na.rm = TRUE)
    sd          = round(as.vector(varianza^(1/2)),2)
    CV          = round(as.vector(sd/prom), 2)

    names(dat) = var.names

    stats = data.frame(Descriptores = names(dat), Minimo = min,
                       Maximo = max, Promedio = prom, SD = sd,
                       CV = CV)

    ## guardar tabla
    file = paste(path.save, 'descriptores_', sp, '.csv', sep = '')
    utils::write.csv(stats, file, row.names = F)

  }

}
