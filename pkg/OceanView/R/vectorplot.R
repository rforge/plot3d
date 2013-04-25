vectorplot <- function(u, v, x = 0, y = 0, colvar = NULL, ..., 
                       col = NULL, NAcol = "white", colkey = list(side = 4), 
                       by = 1, arr = FALSE, xfac = NULL, 
                       clim = NULL, clab = NULL, add = FALSE) {
  
  dots <- list(...)
  clog <- FALSE
  if (!is.null(dots$log)) {
    if (length(grep("c", dots[["log"]])) > 0) {
      dots[["log"]] <- gsub("c", "", dots[["log"]])
      if (dots[["log"]] == "")
        dots[["log"]] <- NULL
      clog <- TRUE  
    }
  }
  dots <- splitpardots(dots)
 
  if (!is.null(colvar)) {
    varlim <- clim
    if (is.null(varlim)) 
      varlim <- range(colvar, na.rm = TRUE)

    if (is.null(col)) 
      col <- jet.col(100)
    if (clog) {
      colvar <- log(colvar)
      if (!is.null(clim)) 
        clim <- log(clim)
    }
    iscolkey <- is.colkey(colkey, col)
    if (iscolkey) {
      colkey <- check.colkey(colkey, add)
      if (!add) 
        plt.or <- par(plt = colkey$parplt)
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
     
  dm <- dots$main
  dp <- dots$points
  
  if (is.null(dm$xlab)) 
    dm$xlab <- "x"
  if (is.null(dm$ylab)) 
    dm$ylab <- "y"
  
  ii <- seq(1, length(u), by = by)
  ll <- length(ii)

# the plot
  if (length(y) > 1) 
    stop ("y should be one number")
  
  if (length(x) == 1) {
    if (is.null(dm$ylim)) 
      dm$ylim <- range(v[ii])
    if (is.null(dm$xlim)) 
      dm$xlim <- range(u[ii]) + x
    x0 <- rep(x, ll)
    y0 <- rep(0, ll)
    xe <- u[ii]+x
    ye <- v[ii]
    LL <- c(alist(0, type = "n"), dm)
    if (! add) 
      do.call("plot",LL)
  } else {
    ii <- seq(1, length(x), by = by)
    y0 <- rep(0, length(x[ii]))
    ye <- v[ii]
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
      do.call("colkey", c(alist(col = col, clim = varlim, clab = clab, clog = clog, add = TRUE)))
      if (! add) 
        par(plt = plt.or)  
      par(mar = par("mar"))
    }    
}


