## =============================================================================
## Perspective plot, x, y matrix or vector; z = matrix
## =============================================================================

persp3D <- function(x = seq(0, 1, length.out = nrow(z)), 
                  y = seq(0, 1, length.out = ncol(z)), 
                  z, colvar = z, ..., 
                  phi = 40, theta = 40,
                  col = NULL,  NAcol = "white", 
                  border = NA, facets = TRUE,
                  colkey = list(side = 4), resfac = 1, 
                  image = FALSE, contour = FALSE, panel.first = NULL,
                  clim = NULL, clab = NULL, bty = "b",
                  lighting = FALSE, inttype = 1, 
                  add = FALSE, plot = TRUE){

  if (add) 
    plist <- getplist()
  else
    plist <- NULL
      
  dot <- splitdotpersp(list(...), bty, lighting, x, y, z, plist = plist)
    
 # check dimensionality
  if (! is.vector(x) & length(dim(x)) != 1)
    stop("'x' should be a vector")
    
  if (! is.vector(y) & length(dim(y)) != 1)
    stop("'y' should be a vector")

  if (! is.matrix(z)) {
    if (length(z) > 1)
      stop("'z'  should be a matrix or one value") 
    z <- matrix(nrow = length(x), ncol = length(y), data = z)  

  } else {
    if (length(x) != nrow(z))
      stop("'x' should be of length = nrow(z)")
    if (length(y) != ncol(z))
      stop("'y' should be of length = ncol(z)")
  }
  
  if (any(resfac != 1)) {   # change resolution
    res <- changeres(resfac, x, y, z, colvar)
    x <- res$x ; y <- res$y ; z <- res$z
    colvar <- res$colvar
  }
  
 # swap if decreasing
  if (all(diff(x) < 0)) {    # swap
    if (is.null(dot$persp$xlim)) 
      dot$persp$xlim <- rev(range(x))
    x <- rev(x)
    z <- z[nrow(z):1, ]
    if (ispresent(colvar)) 
      colvar <- colvar[nrow(colvar):1, ]
  }
 
  if (all(diff(y) < 0)) {    # swap
    if (is.null(dot$persp$ylim)) 
      dot$persp$ylim <- rev(range(y))
    y <- rev(y)
    z <- z[, (ncol(z):1)]
    if (ispresent(colvar)) 
      colvar <- colvar[, (ncol(colvar):1)]
  }

  image   <- check.args(image)
  contour <- check.args(contour)
  if (contour$add) 
    cv <- colvar
      
 # check colvar and colors
  CC <- check.colvar.persp(colvar, z, col, inttype, clim)
  colvar <- CC$colvar; col <- CC$col
  Extend <- inttype == 2
  
  if (ispresent(colvar)) {
    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
     
    iscolkey <- is.colkey(colkey, col)
    if (iscolkey) 
      colkey <- check.colkey(colkey)
    
    if (dot$clog) {                # log transformation of color-values 
      colvar <- log(colvar)
      clim <- log(clim)
    }

  } else 
    iscolkey <- FALSE

  is.facets <- facets
  if (is.na(facets)) 
    is.facets <- TRUE

  if (is.null(plist)) {
    do.call("perspbox", c(alist(x, y, z,  
                     phi = phi, theta = theta, plot = plot, 
                     colkey = colkey, col = col), dot$persp))
    plist <- getplist()
  }
  
  if (is.function(panel.first)) 
    panel.first(plist$mat)         

 # polygon plotting
  X <- matrix(nrow = nrow(z), ncol = ncol(z), data = x)
  Y <- matrix(nrow = nrow(z), ncol = ncol(z), data = y, byrow = TRUE)
    
  Poly <- paintit (colvar, X, Y, z, plist, col, NAcol, clim, 
           border, facets, dot$points$lwd, dot$points$lty, 
           dot$shade, Extend)

 # images and contours
  if (image$add) 
    Poly <- XYimage (Poly, image, x, y, z, plist, col) 

  if (contour$add) 
    segm <- contourfunc(contour, x, y, z, plist, cv = cv, clim = clim)
  else
    segm <- NULL

  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, dot$clog) 

 # plot it
  plist <- plot.struct.3D(plist, poly = Poly, segm = segm, plot = plot)  

  setplist(plist)   
  invisible(plist$mat)
}

