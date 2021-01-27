#' @title Cálculo del centro de gravedad, inercia e isotropía
#' @description Cálculo de índices espaciales: centro de gravedad, inercia
#' e isotropía para un conjunto de datos georeferenciados.
#' @usage cgi(x, y, z = NA, w = NA, modproj = NA, mlong = NA, mlat = NA, plot = F,
#' polig.color = NULL, path.save, name.file)
#' @param x vector de x-coordenadas
#' @param y vector de y-coordenadas
#' @param z vector correspondiente a la variable regionalizada en 2D.
#' De faltar, los resultados de 'cgi' solo abarcaran a las muestras.
#' @param w peso o área de influencia (opcional)
#' @param modproj indica el tipo de proyección a realizar (opcional)
#' @param mlong longitud media en grados de la base de datos a ser transformada.
#' @param mlat latitud media en grados de la base de datos a ser transformada.
#' @param plot si \code{plot = T}, la elipse correspondiente a la inercia se añadirá
#' automáticamente en una gráfica ya existente.
#' @param polig.color si \code{plot = T}, se utilizará este color para la elipse
#' añadida al gráfico.
#' @param path.save ruta en donde se guardarán los gráficos y resultados.
#' @param name.file nombre del archivo final con los resultados de la función.
#' @return
#' El resultado consiste en una lista que incluye:
#' \item{cg}{coordenadas del centro de gravedad}
#' \item{dc.CG}{distancia a la costa del centro de gravedad, expresado en millas naúticas}
#' \item{dsb.CG}{distancia a la plataforma continental del centro de gravedad,
#' expresado en millas naúticas. El signo del valor representa el lado donde se encuentra
#' ubicado el punto con respecto a la plataforma, positivo cuando se encuentra fuera
#' y negativo cuando se encuentra dentro de la plataforma}
#' \item{I}{valor de la incercia alrededor del centro de gravedad}
#' \item{Imax}{valor de la incercia de acuerdo con el primer eje
#' principal de la inercia}
#' \item{Imin}{valor de la incercia de acuerdo con el segundo eje
#' principal de la inercia}
#' \item{Iso}{valor de la isotropía}
#' \item{xaxe1, yaxe1}{coordenadas del primer eje principal de la inercia}
#' \item{xaxe2, yaxe2}{coordenadas del segundo eje principal de la inercia}
#' @details
#' Desarrollo basado en la rutina del Geostatistics for Estimating Fish Abundance (GEFA)
#' & EU program Fisboat, DG-Fish
#'
#' Authors : M.Woillez (Mines-ParisTech), N.Bez (IRD)
#' and J.Rivoirard (Mines-ParisTech)
#' @references Woillez, M., Rivoirard, J., & Petitgas, P. (2009).
#' Notes on survey-based spatial indicators for monitoring fish populations.
#' Aquatic Living Resources, 22(2), 155–164. https://doi.org/10.1051/alr/2009017
#' @export



cgi = function(x, y, z = NA, w = NA, modproj = NA, mlong = NA, mlat = NA,
               plot = F, polig.color, path.save, name.file)
{
  miss <- function(x){
    length(x) == 1 && is.na(x)
  }

  if(miss(z))
    z <- rep(1, length(x))

  if(miss(w))
    w <- rep(1, length(x))

  sel <- !is.na(x * y * z * w)
  x <- x[sel]
  y <- y[sel]
  z <- z[sel]
  w <- w[sel]

  if(length(x[!is.na(x)]) > 0) {
    if(!miss(modproj)) {
      bid <- dg2nm(x = x, y = y, modproj = modproj, mlong = mlong, mlat = mlat)
      x <- bid$x
      y <- bid$y
    }

    # Center of gravity coordinates
    xg <- sum(x * z * w)/sum(z * w)
    yg <- sum(y * z * w)/sum(z * w)

    # Inertia
    dx <- x - xg
    dy <- y - yg
    d <- sqrt(dx^2 + dy^2)
    inert <- sum(z * w * (d^2))/sum(z * w)
    I <- inert

    # Weigthed PCA
    if(!is.na(I)) {
      M11 <- sum(dx^2 * z * w)
      M22 <- sum(dy^2 * z * w)
      M21 <- sum(dx * dy * z * w)
      M12 <- M21
      M <- matrix(c(M11, M12, M21, M22), byrow = T, ncol = 2)
      x1 <- eigen(M)$vectors[1, 1]
      y1 <- eigen(M)$vectors[2, 1]
      x2 <- eigen(M)$vectors[1, 2]
      y2 <- eigen(M)$vectors[2, 2]
      r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])

      # Principal axis coordinates
      e1 <- (y1/x1)^2
      sx1 <- x1/abs(x1)
      sy1 <- y1/abs(y1)
      sx2 <- x2/abs(x2)
      sy2 <- y2/abs(y2)
      xa <- xg + sx1 * sqrt((r1 * inert)/(1 + e1))
      ya <- yg + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
      xb <- 2 * xg - xa
      yb <- 2 * yg - ya
      xc <- xg + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
      yc <- yg + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
      xd <- 2 * xg - xc
      yd <- 2 * yg - yc
      Imax <- r1*inert
      Imin <- (1-r1)*inert
      Iso <- sqrt(Imin/Imax)
    } else {
      xa <- NA
      ya <- NA
      xb <- NA
      yb <- NA
      xc <- NA
      yc <- NA
      xd <- NA
      yd <- NA
      Imax <- NA
      Imin <- NA
      Iso <- NA
    }

    if(!miss(modproj)) {
      bid <- nm2dg(x = c(xg, xa, xb, xc, xd), y = c(yg, ya, yb, yc, yd),
                   modproj = modproj, mlong = mlong, mlat = mlat)
      res <- list(cg = c(bid$x[1], bid$y[1]), I = I, Imax = Imax,
                  Imin = Imin, Iso = Iso, xaxe1 = bid$x[2:3], yaxe1 = bid$y[2:3],
                  xaxe2 = bid$x[4:5],	yaxe2 = bid$y[4:5])
    } else {
      res <- list(cg = c(xg, yg), I = I, Imax = Imax, Imin = Imin,
                  Iso = Iso, xaxe1 = c(xa, xb), yaxe1 = c(ya, yb), xaxe2 = c(xc, xd),
                  yaxe2 = c(yc, yd))
    }
  } else {
    res <- list(cg = NA, I = NA, Imax = NA, Imin = NA, Iso = NA,
                xaxe1 = NA, yaxe1 = NA, xaxe2 = NA, yaxe2 = NA)
  }

  if(plot == T) {
    ## calculando el poligono (elipse)
    xc  = res$cg[1]
    yc  = res$cg[2]
    a   = sqrt(((res$xaxe1[2]-res$xaxe1[1])^2) + ((res$yaxe1[2]-res$yaxe1[1])^2))/2  # major axis length
    b   = sqrt(((res$xaxe2[2]-res$xaxe2[1])^2) + ((res$yaxe2[2]-res$yaxe2[1])^2))/2 # minor axis length
    ## calculando phi
    dy  = res$yaxe1[2] - res$yaxe1[1]
    dx  = res$xaxe1[2] - res$xaxe1[1]
    m   = dy/dx
    phi = atan(m)
    if(phi < 0){
      phi = pi + phi
    }

    t   = seq(0, 2*pi, 0.01)
    xP  = xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
    yP  = yc + a*cos(t)*sin(phi) + b*sin(t)*cos(phi)
    graphics::polygon(xP, yP, col = grDevices::adjustcolor(polig.color, alpha.f = 0.5) , border = NA)
  }

  ## Distancias al centro de gravedad
  dcCG  = fenix::estima_dc(res$cg[1], res$cg[2])
  dsbCG = fenix::estima_distance_signo(res$cg[1], res$cg[2], x1 = fenix::shelfbreak$lon, y1 = fenix::shelfbreak$lat)
  dsbCG = dsbCG$dc*dsbCG$signo

  ## Guardar los datos
  datF = matrix(NA, nrow = 11, ncol = 3)

  datF[,1]    = c('CG', 'I.norte', 'I.sur', 'I.este', 'I.oeste', 'DC.CG', 'DSB.CG', 'Inercia', 'Imax', 'Imin', 'Iso')
  datF[1,2:3] = c(res$cg[1], res$cg[2])
  datF[2,2:3] = c(res$xaxe1[1], res$yaxe1[1])
  datF[3,2:3] = c(res$xaxe1[2], res$yaxe1[2])
  datF[4,2:3] = c(res$xaxe2[2], res$yaxe2[2])
  datF[5,2:3] = c(res$xaxe2[1], res$yaxe2[1])
  datF[6,2] = dcCG
  datF[7,2] = dsbCG

  datF[8,2] = res$I
  datF[9,2] = res$Imax
  datF[10,2] = res$Imin
  datF[11,2] = res$Iso

  res.csv = data.frame(var = as.factor(datF[,1]), lon = as.numeric(datF[,2]),
                       lat = as.numeric(datF[,3]), stringsAsFactors = F)
  utils::write.csv(res.csv, file = paste0(path.save, name.file),row.names = F)

  ## agregando las distancias al CG calculadas al res
  res = append(res, c(dcCG, dsbCG), after=1)
  names(res) = c("cg", "dc.CG", "dsb.CG", "I", "Imax", "Imin", "Iso", "xaxe1", "yaxe1",
                 "xaxe2", "yaxe2")
  return(res)
}
