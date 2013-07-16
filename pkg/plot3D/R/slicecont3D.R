## =============================================================================
## 3-d representation using contour slices in x, y or z
## =============================================================================
# x, y, z vectors or arrays, colvar: array

slicecont3D <- function(x, y, z, colvar, ..., 
                   phi = 40, theta = 40, 
                   xs = NULL,
                   ys = NULL,
                   zs = NULL, level = NULL,
                   col = jet.col(100), NAcol = "white", 
                   border = NA, facets = TRUE, 
                   colkey = list(side = 4), panel.first = NULL,
                   clim = NULL, clab = NULL, bty = "b", 
                   dDepth = 0., add = FALSE, plot = TRUE) {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  dot <- splitdotpersp(list(...), bty, NULL, x, y, z, plist = plist)

  if (! ispresent(colvar))
    stop("'colvar' has to be defined and be an array of dimension 3")

 # check dimensionality 
  DD <- dim(colvar)
  if (length(DD) != 3)
    stop("'colvar' has to be an array of dimension 3")
  if (DD[1] !=  length(x))
    stop("dimension of 'colvar' not compatible with length of 'x'")
  if (DD[2] !=  length(y))
    stop("dimension of 'colvar' not compatible with length of 'y'")
  if (DD[3] !=  length(z))
    stop("dimension of 'colvar' not compatible with length of 'z'")

  if (! is.null(xs))
    if (! is.vector(xs))
      stop("'xs' should be a vector")
  if (! is.null(ys))
    if (! is.vector(ys))
      stop("'ys' should be a vector")
  if (! is.null(zs))
    if (! is.vector(zs))
      stop("'zs' should be a vector")
    
  if (is.null(col)) 
    col <- jet.col(100)

  if (! is.null(dot$alpha)) col <- setalpha(col, dot$alpha)
  
  iscolkey <- is.colkey(colkey, col)    
  if (iscolkey) 
    colkey <- check.colkey(colkey)
        
  if (is.null(clim))
    clim <- range(colvar, na.rm = TRUE)     

  if (dot$clog) {
    colvar <- log(colvar)
    clim <- log(clim)
  }

 # Colors for values = NA 
  if (any (is.na(colvar)) & ! is.null(NAcol) ) {
    CC <- checkcolors(colvar, col, NAcol, clim)
    clim   <- CC$lim
    col    <- CC$col
    colvar <- CC$colvar
  }

  crange <- diff(clim)
  N      <- length(col) -1

  getcol <- function(v) 
    col[1 + trunc((v - clim[1])/crange*N)]

  lwd <- dot$points$lwd
  if (is.null(lwd)) lwd<- 1
  dot$points$lwd   <- NULL
  
  lty <- dot$points$lty
  if (is.null(lty)) lty<- 1
  dot$points$lty   <- NULL

  if (is.null(plist)) {
    do.call("perspbox", c(alist(x = range(x), y = range(y), 
             z = range(z, na.rm = TRUE),
             phi = phi, theta = theta, plot = plot, colkey = colkey, col = col), 
             dot$persp))
    plist <- getplist()
  }  
  if (is.function(panel.first)) 
    panel.first(plist$mat)         
  Seg <- NULL
  templines <- function(clines, x = NULL, y = NULL, z = NULL) {
    
    if (! is.null(x)){
      x <- rep(x, length.out = length(clines[[2]]))
      y <- clines[[2]]
      z <- clines[[3]]
    } else if (! is.null(y)) {
      y <- rep(y, length.out = length(clines[[2]]))
      x <- clines[[2]]
      z <- clines[[3]]
    } else if (! is.null(z)) {
      z <- rep(z, length.out = length(clines[[2]]))
      x <- clines[[2]] 
      y <- clines[[3]] 
    }
    Col <- getcol(clines[[1]])

    Seg <<- addlines(Seg, x = x,
                    y = y, z = z, col = Col,
                    plist = plist, lwd = lwd, lty = lty)
  
  }

  if (is.null(level)) 
    level <- pretty(clim, 10)
  
  for (x.s in xs) {
    ix <- max(1,FindInterval(x.s, x, all.inside = FALSE))
    Data <- colvar[ix,,]
    cL <- contourLines(y, z, Data, levels = level)  
    invisible(lapply(cL, templines, x = x.s))
  }
  for (y.s in ys) {
    iy <- max(1,FindInterval(y.s, y, all.inside = FALSE))
    Data <- colvar[,iy,]
    cL <- contourLines(x, z, Data, levels = level)  
    invisible(lapply(cL, templines, y = y.s))
  }
    
  for (z.s in zs)  {
    iz <- max(1,FindInterval(z.s, z, all.inside = FALSE))
    Data <- colvar[,,iz]
    cL <- contourLines(x, y, Data, levels = level)  
    invisible(lapply(cL, templines, z = z.s))
  }

  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, dot$clog, type = "slicecont3D") 

  if (ispresent(border)) {
    for (x.s in xs) 
      Seg <- addlines(Seg, x = rep(x.s, length.out = 4), 
                            y = c(y[1], y[1], y[length(y)], y[length(y)]),
                            z = c(z[1], z[length(z)], z[length(z)], z[1]), 
                            plist = plist, col = border)

    for (y.s in ys) 
      Seg <- addlines(Seg, z = c(z[1], z[length(z)], z[length(z)], z[1]),
                            x = c(x[1], x[1], x[length(x)], x[length(x)]), 
                            y = rep(y.s, length.out = 4), 
                            plist = plist, col = border)

    for (z.s in zs)  
      Seg <- addlines(Seg, x = c(x[1], x[length(x)], x[length(x)], x[1]),
                            y = c(y[1], y[1], y[length(y)], y[length(y)]), 
                            z = rep(z.s, length.out = 4), 
                            plist = plist, col = border)
  }
  Seg$proj <- Seg$proj + dDepth*plist$persp$expand 
  
 # plot it
  plist <- plot.struct.3D(plist, segm = Seg, plot = plot)  
  
  setplist(plist)
  invisible(plist$mat)
}
