## =============================================================================
## Perspective plot, x, y matrix or vector; z = matrix
## =============================================================================

ribbon3D <- function(x = seq(0, 1, length.out = nrow(z)), 
                   y = seq(0, 1, length.out = ncol(z)), 
                   z, colvar = z, ..., 
                   phi = 40, theta = 40,
                   col = NULL,  NAcol = "white", 
                   border = NA, facets = TRUE,
                   colkey = list(side = 4), resfac = 1, 
                   image = FALSE, contour = FALSE, panel.first = NULL,
                   clim = NULL, clab = NULL, bty = "b", 
                   lighting = FALSE, space = 0.4, dir = "x", 
                   add = FALSE, plot = TRUE) {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  dot <- splitdotpersp(list(...), bty, lighting, x, y, z, plist = plist)

  if (any(space > 0.9))
    stop("'space' too large, should be smaller than or equal to 0.9")
  else if (any(space < 0.1))
    stop("'space' cannot be smaller than 0.1")
  space <- rep(space, length.out = 2)/2 # in x- and y
  
 # input check
  if (length(grep("x", dir)) == 0 &  length(grep("y", dir)) == 0)
    stop ("'dir' should contain at least one of 'x' or 'y'")

  if (! is.vector(x) & length(dim(x)) != 1)
    stop("'x' should be a vector")
    
  if (! is.vector(y) & length(dim(y)) != 1)
    stop("'y' should be a vector")

  if (length(x) != nrow(z))
    stop("'x' should be of length = nrow(z)")

  if (length(y) != ncol(z))
    stop("'y' should be of length = ncol(z)")

  if (any(resfac != 1)) {   # change resolution
    res <- changeres(resfac, x, y, z, colvar)
    x <- res$x ; y <- res$y ; z <- res$z
    colvar <- res$colvar
  }

 # swap if decreasing
  if (! is.matrix(x) & all(diff(x) < 0)) {    # swap
    if (is.null(dot$persp$xlim)) 
      dot$persp$xlim <- rev(range(x))
    x <- rev(x)
    z <- z[nrow(z):1, ]
    if (! is.null(colvar)) 
      colvar <- colvar[nrow(colvar):1, ]
  }
 
  if (! is.matrix(y) & all(diff(y) < 0)) {    # swap
    if (is.null(dot$persp$ylim)) 
      dot$persp$ylim <- rev(range(y))
    y <- rev(y)
    z <- z[, (ncol(z):1)]
    if (! is.null(colvar)) 
      colvar <- colvar[, (ncol(colvar):1)]
  }
  image   <- check.args(image)
  contour <- check.args(contour)
  if (contour$add) 
    cv <- colvar

 # check colvar and colors
  CC <- check.colvar.persp(colvar, z, col, 2, clim)
  colvar <- CC$colvar; col <- CC$col

  if (ispresent(colvar)) {

    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
  
    if (dot$clog) {                    # log transformation of color-values 
      colvar <- log(colvar)
      clim   <- log(clim)
    }
  
    iscolkey <- is.colkey(colkey, col)     # check if colkey is needed
    if (iscolkey) 
      colkey <- check.colkey(colkey)
  } else
    iscolkey <- FALSE
    
  if (is.null(plist)) {
    do.call("perspbox", c(alist(x = range(x), y = range(y), 
             z = range(z, na.rm = TRUE),
             phi = phi, theta = theta, plot = plot, colkey = colkey, col = col), 
             dot$persp))
    plist <- getplist()
  }

  if (is.function(panel.first)) 
    panel.first(plist$mat)         

 # draw ribbons as polygons 
  shade <- dot$shade$shade
  if (is.null(dot$shade$shade))
    dot$shade$shade <- NA

  Poly <- NULL

  Nx <- dim(z) [1]
  Ny <- dim(z) [2]

  lwd <- dot$points$lwd; if (is.null(lwd)) lwd <- 1
  lty <- dot$points$lty; if (is.null(lty)) lty <- 1
  
  if (length(grep("x", dir)) > 0) {
    X <- cbind(x, x)

    for (i in Ny : 2) {
      dy <- space[2] * (y[i] - y[i-1])
      Z  <- 0.5*(z[,i] + z[,i-1])
      CV <-  0.5*(colvar[,i] + colvar[,i-1])
      Y  <- cbind(rep(y[i-1]+dy, Nx), rep(y[i]-dy, Nx))  
      Poly <- addpoly(Poly, X, Y,  cbind(Z, Z), cbind(CV, CV), plist, 
                      col, NAcol, border, facets, lwd = lwd, lty = lty,
                      resfac = 1, clim, 
                      dot$shade$ltheta, dot$shade$lphi, dot$shade$shade,
                      lighting)
    }
  }
 
  if (length(grep("y", dir)) > 0) {
    Y <- cbind(y, y)

    for (i in Nx : 2) {
      dx <- space[1] * (x[i] - x[i-1])
      Z  <- 0.5*(z[i,] + z[i-1,])
      CV <-  0.5*(colvar[i,] + colvar[i-1,])
      X  <- cbind(rep(x[i-1]+dx, Ny), rep(x[i]-dx, Ny))  
      Poly <- addpoly(Poly, X, Y,  cbind(Z, Z), cbind(CV, CV), plist, 
                      col, NAcol, border, facets, lwd = lwd, lty = lty,
                      resfac = 1, clim, 
                      dot$shade$ltheta,dot$shade$lphi, dot$shade$shade,
                      lighting)
    }
  }

  if (image$add) 
    Poly <- XYimage (Poly, image, x, y, z, plist, col) 

  if (contour$add) 
    segm <- contourfunc(contour, x, y, z, plist, cv, clim)
  else
    segm <- NULL

  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, dot$clog) 
   
  plist <- plot.struct.3D(plist, poly = Poly, segm = segm, plot = plot)  

  setplist(plist)
  invisible(plist$mat)
}

