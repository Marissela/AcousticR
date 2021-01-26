"dg2nm" <-
  function(x, y = NA, modproj, mlong, mlat)
  {
    #===============================================================================
    # PROJECTION from decimal degrees to nautical miles
    #
    # Routine from Geostatistics for Estimating Fish Abundance (GEFA)
    # & EU program Fisboat, DG-Fish, STREP n? 502572
    # Authors : M.Woillez (Mines-ParisTech), N.Bez (IRD)
    #           and J.Rivoirard (Mines-ParisTech)
    # Last update : 01 march 2008
    #
    # Argument:
    # x, y     2 vectors of same length or a list with x and y.
    # modproj  Type of projection
    #          if modproj = "": x and y are NOT changed.
    #          if modproj = "mean": longitudes are modified by the same cosine equal
    #             to the cosine of the mean latitude of y.
    #          if modproj = 0.3: longitudes are modified by the same given cosine.
    #          if modproj = "cosine": each longitude is modified according to
    #             its latitude.
    # mlong    mean longitude in DEGREES of the data set to be transformed
    # mlat     mean latitude in DEGREES of the data set to be transformed
    #
    #===============================================================================

    miss <- function(x){
      length(x) == 1 && is.na(x)
    }

    if(is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if(!miss(modproj)) {
      x <- x - mlong
      y <- y - mlat
      if(modproj == "mean") {
        x <- x * 60 * cos((mlat * pi)/180)
        y <- y * 60
      }
      else if(is.numeric(modproj)) {
        x <- x * 60 * modproj
        y <- y * 60
      }
      else if(modproj == "cosine") {
        x <- x * 60 * cos((y * pi)/180)
        y <- y * 60
      }
    }
    list(x = x, y = y)
  }

"nm2dg" <-
  function(x, y = NA, modproj, mlong, mlat)
  {
    #===============================================================================
    # PROJECTION from nautical miles to decimal degrees
    #
    # Routine from Geostatistics for Estimating Fish Abundance (GEFA)
    # & EU program Fisboat, DG-Fish, STREP n? 502572
    # Authors : M.Woillez (Mines-ParisTech), N.Bez (IRD)
    #           and J.Rivoirard (Mines-ParisTech)
    # Last update : 01 march 2008
    #
    # Argument:
    # x, y     2 vectors of same length or a list with x and y.
    # modproj  if modproj = "": x and y are NOT changed.
    #          if modproj = "mean": longitudes are modified by the same cosine equal
    #                         to the cosine of the mean latitude of y.
    #          if modproj = 0.3: longitudes are modified by the same given cosine.
    #          if modproj = "cosine": each longitude is modified according to
    #                         its latitude.
    # mlong    mean longitude in DEGREES of the data set to be transformed
    # mlat     mean latitude in DEGREES of the data set to be transformed
    #
    #===============================================================================

    miss <- function(x){
      length(x) == 1 && is.na(x)
    }

    if(is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if(!miss(modproj)) {
      if(modproj == "mean") {
        y <- y/60
        x <- x/(60 * cos((mlat * pi)/180))
      }
      else if(is.numeric(modproj)) {
        x <- x/(60 * modproj)
        y <- y/60
      }
      else if(modproj == "cosine") {
        y <- y/60
        x <- x/(60 * cos(((y + mlat) * pi)/180))
      }
    }
    x <- x + mlong
    y <- y + mlat
    list(x = x, y = y)
  }
