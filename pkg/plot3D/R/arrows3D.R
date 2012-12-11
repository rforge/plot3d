## =============================================================================
## 3-D arrows function
## =============================================================================

arrows3D  <- function(x0, y0, z0, x1 = x0, y1 = y0, z1 = z0,
                    colvar = NULL, 
                    ..., phi = 40, theta = 40,
                    col = NULL, NAcol = "white", 
                    colkey = list(side = 4), panel.first = NULL,
                    clim = NULL, clab = NULL, bty = "f", 
                    add = FALSE, plot = TRUE)  {
  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  dot  <- splitdotpersp(list(...), bty, NULL, c(x0, x1), c(y0, y1), c(z0, z1), plist = plist)

# checks
  len <- length(x0)
  if (length(y0) != len)
    stop("'y0' should have same length as 'x0'")
  if (length(z0) != len)
    stop("'z0' should have same length as 'x0'")
  if (length(x1) != len)
    stop("'x1' should have same length as 'x0'")
  if (length(y1) != len)
    stop("'y1' should have same length as 'x0'")
  if (length(z1) != len)
    stop("'z1' should have same length as 'x0'")

  # colors
  if (ispresent(colvar)) { 
    if (length(colvar) != len)
      stop("'colvar' should have same length as 'x0', 'y0' and 'z0'")
    
    if (is.null(col))
      col <- jet.col(100)
    
    if (length(col) == 1)
      col <- c(col, col)

    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
    
    if (dot$clog) {                       # log transformation of color-values 
      colvar <- log(colvar)
      clim <- log(clim)
    }

    iscolkey <- is.colkey(colkey, col) 
    if (iscolkey) 
      colkey <- check.colkey(colkey)
     
    Col <- variablecol(colvar, col, NAcol, clim) # generate color scheme

  } else {
    if (is.null(col))
      col <- "black"
    Col <- rep(col, length.out = len)  
    iscolkey <- FALSE
  }   

  if (is.null(plist)) {
    do.call("perspbox", 
       c(alist(x = range(c(x0, x1)), y = range(c(y0, y1)), 
               z = range(c(z0, z1)), 
               phi = phi, theta = theta, plot = plot, col = col), dot$persp))
    plist <-  getplist()
  }  
  
  if (is.function(panel.first)) 
    panel.first(plist$mat)
  
  length <- dot$points$length ; if (is.null(length)) length <- 0.25
  angle <- dot$points$angle ; if (is.null(angle)) angle <- 30
  code <- dot$points$code ; if (is.null(code)) code <- 2
  lwd <- dot$points$lwd ; if (is.null(lwd)) lwd <- 1
  lty <- dot$points$lty ; if (is.null(lty)) lty <- 1

 # sort points according to view
  Proj <- project (0.5*(x0 + x1), 0.5*(y0 + y1), 0.5*(z0 + z1), plist)

  arr  <- list(x.from = x0, 
               x.to   = x1,
               y.from = y0, 
               y.to   = y1,                                  
               z.from = z0, 
               z.to   = z1,                                  
               col    = Col,
               length = rep(length, length.out = len),
               code   = rep(code  , length.out = len),
               angle  = rep(angle , length.out = len),
               lwd    = rep(lwd   , length.out = len),
               lty    = rep(lty   , length.out = len),
               proj   = Proj)
  class(arr) <- "arr"

  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, dot$clog) 

 # plot it
  plist <- plot.struct.3D(plist, arr = arr, plot = plot)  

  setplist(plist)   
  invisible(plist$mat)
}




