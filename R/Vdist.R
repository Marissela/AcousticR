#' @title Gráficos de distribución vertical
#' @description Gráficos de distribución vertical
#' @usage Vdist(datos, sp, xlim = c(3,18), ylim = c(0,100,10), plot.type = 1)
#' @param datos data-frame con los datos de las regiones a analizar.
#' @param sp vector con la(s) especie(s) a analizar. Las especies se
#' consideran: anchoveta (ANC), munida (MUN), jurel (JUR), vinciguerria
#' (VIN), pota (POT), bagre (BAG), caballa (CAB) y samasa (SAM).
#' @param xlim rango de latitud (valores positivos). Por default se
#' considera el rango latitudinal del Perú.
#' @param ylim rango de profundidad (valores positivos). El último
#' valor corresponde al paso que se desea considerar para el eje y.
#' @param plot.type Tipo de gráfico a realizar. Por default, \code{plot.type = 1}
#' realiza gráficos de latitud vs profundidad. Si, \code{plot.type = 2}, se
#' genera el gráfico de horas del día vs profundidad.
#' @details
#' Se realizan dos subplots acerca del comportamiento vertical de la(s)
#' especie(s) a analizar.
#'
#' Para gráficos en donde se analiza solo una especie se incluye la línea de la
#' profundidad media por grado de latitud.
#' @export



Vdist = function(datos, sp, xlim = c(3,18), ylim = c(0,100,10), plot.type = 1)
{
  ## Tabla de caracteristicas por especie
  species   = c('ANC', 'MUN', 'JUR', 'VIN', 'POT', 'BAG', 'CAB', 'SAM')
  col1      = c('lightcoral', 'slategray2', 'seagreen1', 'plum', 'khaki1', 'gray87', 'peachpuff3', 'mistyrose')
  col2      = c('red4', 'Blue4', 'seagreen4', 'darkmagenta', 'darkorange', 'gray12', 'tan4', 'hotpink')
  Sv_mean1  = c(-35, -35, -35, -35, -45, -35, -35, -35)
  Sv_mean2  = c(-65, -65, -65, -65, -75, -65, -65, -65)
  sp_name   = c('Anchoveta', 'Munida', 'Jurel', 'Vinciguerria', 'Pota', 'Bagre', 'Caballa', 'Samasa')
  sp.col    = data.frame(species, col1, col2, Sv_mean1, Sv_mean2, sp_name)

  ## Elaborando una lista con los datos de las especies
  list_SP = list()
  for (isp in 1:length(sp))
  {
    ## Leer los datos por especie
    datos$Region_class = gsub(" ", "", datos$Region_class)

    if (sp[isp] == 'ANC')
    {
      dat = datos[which(datos$Region_class == 'ANCP' | datos$Region_class == 'ANCG'),]
    } else {
      dat = datos[which(datos$Region_class == sp[isp]),]
    }

    ## Variables adicionales
    dat$tsup      = dat$Depth_mean - (dat$Height_mean/2)
    dat$tinf      = dat$Height_mean + dat$tsup

    dat$hora      = as.numeric(substr(as.character(strptime(dat$Time_M,"%X")),12,13))
    dat$minuto    = as.numeric(substr(as.character(strptime(dat$Time_M,"%X")),15,16))
    dat$segundo   = as.numeric(substr(as.character(strptime(dat$Time_M,"%X")),18,19))
    dat$horacorr  = dat$hora+dat$minuto/60+dat$segundo/3600

    dat$depth_mean_floor  = floor(dat$Depth_mean)
    dat$latitud           = abs(ceiling(dat$Lat_M))

    ## Seleccionar datos de acuerdo con xlim & ylim
    indXY = which(abs(dat$Lat_M) >= xlim[1] & abs(dat$Lat_M) <= xlim[2] &
                    dat$Depth_mean >= ylim[1] & dat$Depth_mean <= ylim[2])
    dat   = dat[indXY,]

    ## Base final
    list_SP[sp[isp]] = list(dat)
  }


  ## Plot 1. Lat vs Prof | Hours vs Prof
  cols = matrix(NA, nrow = length(sp), ncol = 3)
  for (isp in 1:length(sp))
  {
    ## Paleta de colores
    indSP = which(sp.col[,1] == sp[isp])
    palette = grDevices::colorRampPalette(colors = c(as.character(sp.col[indSP,2]), as.character(sp.col[indSP,3])),
                                          space = "Lab")
    list_SP[[sp[isp]]]$Svmean_factor = cut(list_SP[[sp[isp]]]$Sv_mean, breaks = seq(from = sp.col[indSP,5], to = sp.col[indSP,4], by = 10))
    num_colors = nlevels(list_SP[[sp[isp]]]$Svmean_factor)
    colors = palette(num_colors)
    cols[isp,] = colors

    ## Plot 1. Lat vs Prof
    if (plot.type == 1)
    {
      profLat = tapply(list_SP[[sp[isp]]]$tsup, list_SP[[sp[isp]]]$latitud, mean)

      if (isp == 1)
      {
        x11(width = 13.4, height = 4.9) # inches

        figura = graphics::layout(matrix(c(1,2,3,4), ncol = 2, byrow = TRUE),
                        widths = c(0.80, 0.20, 1), heights = c(6,1))
        graphics::layout.show(figura)

        # aa = par() # default settings
        graphics::par(cex.lab = 1.6, cex.axis = 1.2, mai = c(0.7, 0.8, 0.5, 0.15))
        graphics::plot(abs(list_SP[[sp[isp]]]$Lat_M), -list_SP[[sp[isp]]]$tsup, type = 'n', xlim = xlim,
             ylim = c(-ylim[2], ylim[1]), axes = F, xlab = 'Latitud (\u00B0S)',
             ylab = 'Profundidad (m)')
        graphics::abline(h = -seq(ylim[1], ylim[2] , by = ylim[3]),
               v = seq(xlim[1], xlim[2], by = 1), col = 'gray85')
        graphics::axis(1, at = seq(xlim[1], xlim[2], by = 1), cex = 2.3)
        graphics::axis(2, at = -seq(ylim[1], ylim[2] , by = ylim[3]),
             labels = seq(ylim[1], ylim[2] , by = ylim[3]), cex = 1.6, las = 1)
        graphics::box(lwd = 2)

        ## Colocar puertos de acuerdo al xlim
        puertos     = c("Pta.Sal", "La Negra", "Salaverry", "Huarmey", "Callao", "Pisco", "Atico", "M.Sama")
        at.puertos  = c(3.5, 5.5, 7.8, 10, 12, 14, 16, 18)

        graphics::mtext(puertos[which(at.puertos >= xlim[1] & at.puertos <= xlim[2])], side = 3, line = 0.5,
                        at = at.puertos[which(at.puertos >= xlim[1] & at.puertos <= xlim[2])], cex = 1.6)

        graphics::points(abs(list_SP[[sp[isp]]]$Lat_M), -list_SP[[sp[isp]]]$tsup, col = colors[list_SP[[sp[isp]]]$Svmean_factor],
               cex = 0.5)

        if (length(sp) == 1)
        {
          graphics::lines(unlist(dimnames(profLat)), -as.vector(profLat), lwd = 2, type = 'o')
        }

      } else {
        graphics::points(abs(list_SP[[sp[isp]]]$Lat_M), -list_SP[[sp[isp]]]$tsup, col = colors[list_SP[[sp[isp]]]$Svmean_factor],
               cex = 0.5)
      }
    }

    ## Plot1. Hours vs Prof
    if (plot.type == 2)
    {
      if (isp == 1)
      {
        x11(width = 13.4, height = 4.9) # inches

        figura = graphics::layout(matrix(c(1,2,3,4), ncol = 2, byrow = TRUE),
                        widths = c(0.80, 0.20, 1), heights = c(6,1))
        graphics::layout.show(figura)

        # aa = par() # default settings
        graphics::par(cex.lab = 1.6, cex.axis = 1.2, mai = c(0.7, 0.8, 0.5, 0.15))
        graphics::plot(list_SP[[sp[isp]]]$horacorr, -list_SP[[sp[isp]]]$tsup, type = 'n', xlim = c(0,24),
             ylim = c(-ylim[2], ylim[1]), axes = F, xlab = 'Hora del d\u00EDa (hrs)',
             ylab = 'Profundidad (m)')
        graphics::abline(h = -seq(ylim[1], ylim[2] , by = ylim[3]),
               v = c(6,12,18), col = 'gray85')
        graphics::axis(1, at = c(0,6,12,18,24), labels = c(0,6,12,18,24), cex = 2.3)
        graphics::axis(2, at = -seq(ylim[1], ylim[2] , by = ylim[3]),
             labels = seq(ylim[1], ylim[2] , by = ylim[3]), cex = 1.6, las = 1)
        graphics::box(lwd = 2)

        graphics::points(list_SP[[sp[isp]]]$horacorr, -list_SP[[sp[isp]]]$tsup, col = colors[list_SP[[sp[isp]]]$Svmean_factor],
               cex = 0.5)

      } else {
        graphics::points(abs(list_SP[[sp[isp]]]$horacorr), -list_SP[[sp[isp]]]$tsup, col = colors[list_SP[[sp[isp]]]$Svmean_factor],
               cex = 0.5)
      }
    }
  }

  ## Plot2. Sv vs. Prof
  for (isp in 1:length(sp))
  {
    if (length(sp) == 1)
    {
      Sv        = tapply(list_SP[[sp[isp]]]$Sv_mean, list_SP[[sp[isp]]]$depth_mean_floor, seewave::meandB)

      indday    = which(list_SP[[sp[isp]]]$horacorr >= 6 & list_SP[[sp[isp]]]$horacorr <= 18)
      Svday     = tapply(list_SP[[sp[isp]]]$Sv_mean[indday], list_SP[[sp[isp]]]$depth_mean_floor[indday], seewave::meandB)
      Svmday    = round(seewave::meandB(list_SP[[sp[isp]]]$Sv_mean[indday], level="IL"),2)

      indnight  = which(list_SP[[sp[isp]]]$horacorr < 6 | list_SP[[sp[isp]]]$horacorr > 18)
      Svnight   = tapply(list_SP[[sp[isp]]]$Sv_mean[indnight], list_SP[[sp[isp]]]$depth_mean_floor[indnight], seewave::meandB)
      Svmnight  = round(seewave::meandB(list_SP[[sp[isp]]]$Sv_mean[indnight], level="IL"),2)
    } else{
      Sv = tapply(list_SP[[sp[isp]]]$Sv_mean, list_SP[[sp[isp]]]$depth_mean_floor, seewave::meandB)
    }

    if (isp == 1)
    {
      graphics::par(cex.lab = 1.6, cex.axis = 1.2, mai = c(0.7, 0, 0.5, 0.15))
      graphics::plot(as.vector(Sv), -as.numeric(unlist(dimnames(Sv))), type = 'n',
           xlim = c(-80,-30), ylim = c(-ylim[2], ylim[1]), axes = F,
           xlab = 'Sv (dB)', ylab = '')
      graphics::abline(h = -seq(ylim[1], ylim[2] , by = ylim[3]),
             v = seq(-80,-30, by = 10), col = 'gray85')
      graphics::axis(1, at = seq(-80,-30, by = 10), cex = 2.3)
      graphics::box(lwd = 2)

      if(length(sp) == 1)
      {
        graphics::lines(as.vector(Svday), -as.numeric(unlist(dimnames(Svday))), lty = 1,
              lwd = 2, col = 'darkgoldenrod1')
        graphics::lines(as.vector(Svnight), -as.numeric(unlist(dimnames(Svnight))), lty = 1,
              lwd = 2, col = 'blue4')
      } else {
        graphics::lines(as.vector(Sv), -as.numeric(unlist(dimnames(Sv))), lty = 1,
              lwd = 2, col = cols[isp,3])
      }

    } else{
      graphics::lines(as.vector(Sv), -as.numeric(unlist(dimnames(Sv))), lty = 1,
            lwd = 2, col = cols[isp,3])
    }
  }

  ## Legends: Plot1
  for (isp in 1:length(sp))
  {
    if (length(sp) == 1)
    {
      indSP = which(sp.col[,1] == sp[isp])
      graphics::par(mai=c(0,0,0,0))
      graphics::plot.new()
      graphics::legend(0, 0.94, legend = sp.col$sp_name[indSP], cex = 1.5,
             text.font = 4, bg = 'white', horiz = T, bty = 'n')
      graphics::legend(0.17, 0.94, col = colors, pch = 16, cex = 1.5, inset=c(0.2,-0.2),
             text.font = 4, bg = 'white', horiz = T, bty = 'n',
             legend = c(paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[1], 'Disperso', sep = ' '),
                        paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[2], 'Denso', sep = ' '),
                        paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[3], 'Muy denso', sep = ' ')))
    } else if (length(sp) > 1 && isp == 1)
    {
      indSP = which(sp.col[,1] == sp[isp])
      graphics::par(mai=c(0,0,0,0))
      graphics::plot.new()
      graphics::legend(0, 1.2, legend = sp.col$sp_name[indSP], cex = 1.5,
             text.font = 4, bg = 'white', horiz = T, bty = 'n')
      graphics::legend(0.17, 1.2, col = cols[isp,], pch = 16, cex = 1.5, inset=c(0.2,-0.2),
             text.font = 4, bg = 'white', horiz = T, bty = 'n',
             legend = c(paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[1], 'Disperso', sep = ' '),
                        paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[2], 'Denso', sep = ' '),
                        paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[3], 'Muy denso', sep = ' ')))
    } else {
      indSP = which(sp.col[,1] == sp[isp])
      graphics::legend(0, 1.2-0.4, legend = sp.col$sp_name[indSP], cex = 1.5,
             text.font = 4, bg = 'white', horiz = T, bty = 'n')
      graphics::legend(0.17, 1.2-0.4, col = cols[isp,], pch = 16, cex = 1.5, inset=c(0.2,-0.2),
             text.font = 4, bg = 'white', horiz = T, bty = 'n',
             legend = c(paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[1], 'Disperso', sep = ' '),
                        paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[2], 'Denso', sep = ' '),
                        paste(levels(list_SP[[sp[isp]]]$Svmean_factor)[3], 'Muy denso', sep = ' ')))
    }
  }

  ## Legends: Plot2
  for (isp in 1:length(sp))
  {
    if (length(sp) == 1)
    {
      graphics::par(mai=c(0,0,0,0))
      graphics::plot.new()
      graphics::legend('left', col = c('darkgoldenrod1', 'blue4'), cex = 1.5,
             lwd = 2, text.font = 4, bg = 'white', bty = 'n',
             legend = c(paste("D\u00EDa:", Svmday), paste("Noche:", Svmnight)))
    } else if (length(sp) > 1 && isp == 1)
    {
      # indSP = which(sp.col[,1] == sp[isp])
      SvmSP = round(seewave::meandB(list_SP[[sp[isp]]]$Sv_mean, level="IL"),2)

      graphics::par(mai=c(0,0,0,0))
      graphics::plot.new()
      graphics::legend(-0.15, 1.2, col = cols[isp,3], lty = 1, cex = 1.5,
             lwd = 2, text.font = 4, bg = 'white', bty = 'n',
             # legend = paste0(sp.col$sp_name[indSP], ': ', SvmSP))
             legend = paste0('Sv prom: ', SvmSP))
    } else{
      # indSP = which(sp.col[,1] == sp[isp])
      SvmSP = round(seewave::meandB(list_SP[[sp[isp]]]$Sv_mean, level="IL"),2)

      graphics::legend(-0.15, 1.2-0.4, col = cols[isp,3], lty = 1, cex = 1.5,
             lwd = 2, text.font = 4, bg = 'white', bty = 'n',
             # legend = paste0(sp.col$sp_name[indSP], ': ', SvmSP))
             legend = paste0('Sv prom: ', SvmSP))
    }
  }
}
