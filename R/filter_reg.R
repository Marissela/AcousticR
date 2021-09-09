#' @title Limpieza de datos en la bitácora de regiones
#' @description Función para filtrar la base de datos de regiones teniendo en
#' cuenta los errores producidos durante la medición de los datos.
#' @usage filter.reg(file, sp = c("ANC", "MUN", "JUR",
#'             "VIN", "POT", "BAG", "CAB", "SAM"), frequency,
#'             path.save)
#' @param file bitácora de regiones en formato *.csv*
#' @param sp especies a analizar. Por default se consideran: anchoveta
#' (ANC), munida (MUN), jurel (JUR), vinciguerria (VIN), pota (POT),
#' bagre (BAG), caballa (CAB) y samasa (SAM).
#' @param frequency frecuencia a la que corresponden los datos.
#' @param path.save ruta donde será guardada la nueva base de datos.
#' @details
#' Los filtros se realizan en dos niveles: generales y por especie.
#'
#' En los filtros generales se corrigen todos los valores que por default
#' produce echoview para las medidas de longitud, latitud y Sv.
#'
#' Para evitar malas detecciones dentro del campo cercano del transductor, se
#' considera el campo cercano de cada frecuencia y se eliminan las regiones
#' que se encuentran dentro de la mismas.
#'
#' En los filtros por especie consideran el valor de Sv por especie
#' y la profundidad máxima a la cual pueden llegar.
#' @export



filter.reg = function(file, sp = c('ANC', 'MUN', 'JUR', 'VIN', 'POT', 'BAG', 'CAB',
                                   'SAM'), frequency, path.save)
{
  datos = utils::read.csv(file)

  # FILTROS GENERALES
  ## lon & lat
  indLon  = which(datos$Lon_M > 0)
  indLat  = which(datos$Lat_M > 0)

  ## Sv
  indSv = which(datos$Sv_mean == -999)

  ## No. pings
  indPing = which(datos$Ping_E - datos$Ping_S < 3)

  ## eliminando datos
  index = unique(c(indLon, indLat, indSv, indPing))
  if (length(index) > 0)
  {
    datos = datos[-index,]
  }


  ## campo cercano de acuerdo a cada frecuencia
  ## SIMRAD. (2015). Simrad EK60 Installation manual
  datos$tsup = datos$Depth_mean - (datos$Height_mean/2)

  if (frequency == 38){
    indNF = which(datos$tsup > 2.08)
    datos = datos[indNF,]
  } else if(frequency == 70){
    indNF = which(datos$tsup > 1.13)
    datos = datos[indNF,]
  } else if(frequency == 120){
    indNF = which(datos$tsup > 0.66)
    datos = datos[indNF,]
  } else if(frequency == 200){
    indNF = which(datos$tsup > 0.39)
    datos = datos[indNF,]
  }


  # FILTROS POR ESPECIE
  for (isp in 1:length(sp))
  {
    ## anchoveta
    if (sp[isp] == 'ANC' && length(which(datos$Region_class == ' ANCP' | datos$Region_class == ' ANCG')) > 0)
    {
      indSP  = which(datos$Region_class == ' ANCP' | datos$Region_class == ' ANCG')

      indSv     = which(datos$Sv_mean <= -70 | datos$Sv_mean > -30)
      indProf   = which(datos$Depth_mean > 100)
      indLen    = which(datos$Corrected_length < 0.5 | datos$Corrected_length > 14000)
      indThick  = which(datos$Corrected_thickness < 0.5 | datos$Corrected_thickness > 60)
      index = unique(c(indSv, indProf, indLen, indThick))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }


    ## munida
    if (sp[isp] == 'MUN' && length(which(datos$Region_class == ' MUN')) > 0)
    {
      indSP  = which(datos$Region_class == ' MUN')

      indSv = which(datos$Sv_mean <= -75 | datos$Sv_mean > -40)
      indProf = which(datos$Depth_mean > 150)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

    ## jurel
    if (sp[isp] == 'JUR' && length(which(datos$Region_class == ' JUR')) > 0)
    {
      indSP  = which(datos$Region_class == ' JUR')

      indSv = which(datos$Sv_mean <= -65 | datos$Sv_mean > -35)
      indProf = which(datos$Depth_mean > 100)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

    ## vinciguerria
    if (sp[isp] == 'VIN' && length(which(datos$Region_class == ' VIN')) > 0)
    {
      indSP  = which(datos$Region_class == ' VIN')

      indSv = which(datos$Sv_mean <= -65 | datos$Sv_mean > -35)
      indProf = which(datos$Depth_mean > 500)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

    ## pota
    if (sp[isp] == 'POT' && length(which(datos$Region_class == ' POT')) > 0)
    {
      indSP  = which(datos$Region_class == ' POT')

      indSv = which(datos$Sv_mean <= -75 | datos$Sv_mean > -45)
      indProf = which(datos$Depth_mean > 500)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

    ## bagre
    if (sp[isp] == 'BAG' && length(which(datos$Region_class == ' BAG')) > 0)
    {
      indSP  = which(datos$Region_class == ' BAG')

      indSv = which(datos$Sv_mean <= -65 | datos$Sv_mean > -35)
      indProf = which(datos$Depth_mean > 100)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

    ## caballa
    if (sp[isp] == 'CAB' && length(which(datos$Region_class == ' CAB')) > 0)
    {
      indSP  = which(datos$Region_class == ' CAB')

      indSv = which(datos$Sv_mean <= -65 | datos$Sv_mean > -35)
      indProf = which(datos$Depth_mean > 100)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

    ## samasa
    if (sp[isp] == 'SAM' && length(which(datos$Region_class == ' SAM')) > 0)
    {
      indSP  = which(datos$Region_class == ' SAM')

      indSv = which(datos$Sv_mean <= -65 | datos$Sv_mean > -35)
      indProf = which(datos$Depth_mean > 100)
      index = unique(c(indSv, indProf))

      indTRUE = which(indSP %in% index == TRUE)

      indexclude = indSP[indTRUE]
      datos$Depth_mean[indexclude] = -999

    }

  }

  # EXTRAYENDO LOS DATOS FINALES
  datos = datos[which(datos$Depth_mean != -999),]

  # GUARDAR
  if (grepl('/', file, fixed = T) == TRUE)
  {
    file = unlist(strsplit(file, '/'))
    file = file[length(file)]
  }

  n = nchar(file)
  file = substr(file,1,n-4)

  utils::write.csv(datos, paste0(path.save, file,'_OK.csv'), row.names = F)

}
