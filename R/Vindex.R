#' @title Cálculo del Índice de Agregación
#' @description Cálculo de índice de agregación para los cardúmenes.
#' @usage IA(Sv_mean, Depth_mean)
#' @param Sv_mean vector con los valores de Sv promedio
#' @param Depth_mean vector con los valores de prodfundidad promedio
#' @export



IA = function(Sv_mean,Depth_mean)
{
  Sv_mean_Lineal = (10^(Sv_mean/10))

  IA = sum(Sv_mean_Lineal^2) / sum(Sv_mean_Lineal)^2
  return(IA)
}


#' @title Cálculo del Índice de Centro de Masa
#' @description Cálculo de índice de centro de masa para los cardúmenes.
#' @usage CM(Sv_mean, Depth_mean)
#' @param Sv_mean vector con los valores de Sv promedio
#' @param Depth_mean vector con los valores de prodfundidad promedio
#' @export



CM = function(Sv_mean, Depth_mean)
{
  z = Depth_mean
  Sv_mean_Lineal = (10^(Sv_mean/10))

  CM = sum(Sv_mean_Lineal*z)/sum(Sv_mean_Lineal)
  return(CM)
}


#' @title Cálculo de Inercia
#' @description Cálculo de inercia vertical para los cardúmenes.
#' @usage Inertia(Depth_mean, CM, Sv_mean)
#' @param Depth_mean vector con los valores de prodfundidad promedio
#' @param CM vector con el valor del centro de masa
#' @param Sv_mean vector con los valores de Sv promedio
#' @export



Inertia = function(Depth_mean,CM,Sv_mean)
{
  z = Depth_mean
  Sv_mean_Lineal = (10^(Sv_mean/10))

  Inercia = sum( (z - CM)^2*Sv_mean_Lineal)/sum(Sv_mean_Lineal)
  return(Inercia)
}
