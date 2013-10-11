vectorplot <- function(u, v, x = 0, y = 0, colvar = NULL, ..., 
                       col = NULL, NAcol = "white", colkey = NULL, 
                       by = 1, arr = FALSE, xfac = NULL, 
                       clim = NULL, clab = NULL, add = FALSE) {
  
  dots <- splitpardots(list(...))
 
  if (add) 
      plist <- getplist()
  else plist <- NULL
  setplist(plist)

  if (!is.null(colvar)) {
    varlim <- clim
    if (is.null(varlim)) 
      varlim <- range(colvar, na.rm = TRUE)

    if (is.null(col)) 
      col <- jet.col(100)
    if (dots$clog) {
      colvar <- log(colvar)
      if (!is.null(clim)) 
        clim <- log(clim)
    }
    iscolkey <- is.colkey(colkey, col)
    if (iscolkey) {
      colkey <- check.colkey(colkey, add)
      if (! add) 
        plist$plt$main <- colkey$parplt
    }  
    if (length(colvar) != length(u)) 
      stop("length of 'colvar' should be equal to length of 'u' and 'v'")
    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
    Col <- variablecol(colvar, col, NAcol, clim)
  }
  else {
    Col <- col
    if (is.null(Col)) 
      Col <- "black"
      iscolkey <- FALSE
  }
  par (plt = plist$plt$main)
     
  dm <- dots$main
  dp <- dots$points
  
  if (is.null(dm$xlab)) 
    dm$xlab <- "x"
  if (is.null(dm$ylab)) 
    dm$ylab <- "y"
  
  y0 <- rep(y, length.out = length(u))
  ii <- seq(1, length(u), by = by)
  y0 <- y0[ii]
  ll <- length(ii)

# the plot
#  if (length(y) > 1) 
#    stop ("y should be one number")

  if (length(x) == 1) {
    if (is.null(dm$ylim)) 
      dm$ylim <- range(v[ii] + y0)
    if (is.null(dm$xlim)) 
      dm$xlim <- range(u[ii]) + x
    x0 <- rep(x, ll)
    xe <- u[ii]+x
    ye <- v[ii]
    LL <- c(alist(0, type = "n"), dm)
    if (! add) 
      do.call("plot",LL)
  } else {
    ii <- seq(1, length(x), by = by)
    ye <- y0 + v[ii]
    if (is.null(xfac))
      xfac <- diff(range(x))/diff(range(c(y0, ye)))
    x0 <- x[ii]
    xe <- x[ii] + u[ii]*xfac
    if (is.null(dm$ylim)) 
      dm$ylim <- range(c(y0, ye))
    if (is.null(dm$xlim)) 
      dm$xlim <- range(c(x0, xe))
    LL <- c(alist(0, type = "n"), dm)
    if (! add) 
      do.call("plot",LL)
    pusr <- par("usr")
    if (is.null(xfac))
      xfac <- diff(pusr[1:2])/diff(pusr[3:4])
    xe <- x[ii] + u[ii]*xfac
  }

# the segments/arrows
  Ls <- c(alist(x0, y0, xe, ye, col = Col), dp) 
  if (arr) {
    if (is.null(dp$arr.length)) 
      dp$arr.length <- 0.1
    if (is.null(dp$arr.type)) 
      dp$arr.type <- "triangle"
    do.call("Arrows", Ls)
  } else 
    do.call("segments", Ls)
    
    if (iscolkey) {
      colkey$parleg <- colkey$parplt <- NULL
      do.call("colkey", c(alist(col = col, clim = varlim, clab = clab, 
        clog = dots$clog, add = TRUE), colkey))
      par(plt = plist$plt$ori)  
    }    
    par(mar = par("mar")) 
}


