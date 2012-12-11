## =============================================================================
## contours and images, x, y matrix or vector; z = matrix
## =============================================================================

image3D <- function(x = NULL, 
                  y = NULL, 
                  z, colvar = z, ..., 
                  phi = 40, theta = 40,
                  col = NULL,  NAcol = "white", 
                  border = NA, facets = TRUE,
                  colkey = list(side = 4), resfac = 1, 
                  contour = FALSE, panel.first = NULL,
                  clim = NULL, clab = NULL, bty = "b",
                  inttype = 1, add = FALSE, plot = TRUE){

  if (is.matrix(z))
    stop ("'z' should be a value for 'image3D'")

  if (length(z) > 1)
      stop("'z'  should be one value for 'image3D'") 
  if (is.null(x))
    x <- seq(0, 1, length.out = nrow(colvar))
  if (is.null(y))
    y <- seq(0, 1, length.out = ncol(colvar))
  z <- matrix(nrow = length(x), ncol = length(y), data = z)  
    
  plist <- persp3D (x = x, y = y, z = z, colvar = colvar, 
                  phi = phi, theta = theta,
                  col = col,  NAcol = NAcol, 
                  border = border, facets = facets,
                  colkey = colkey, resfac = resfac, add = add, 
                  contour = contour, panel.first = panel.first,
                  clim = clim, clab = clab, bty = bty,
                  inttype = inttype, plot = plot, ...)
  invisible (plist)
}                  

contour3D <- function(x = NULL, 
                  y = NULL, 
                  z, colvar = z, ..., 
                  phi = 40, theta = 40,
                  col = NULL,  NAcol = "white", 
                  border = NA, facets = TRUE,
                  colkey = list(side = 4), resfac = 1, 
                  panel.first = NULL,
                  clim = NULL, clab = NULL, bty = "b",
                  inttype = 1, dDepth = 1e-1, 
                  add = FALSE, plot = TRUE){

  if (add) 
    plist <- getplist()
  else
    plist <- NULL
 # check input
  if (!ispresent(colvar)) 
    stop ("'colvar' should be present for contour3D")

  if (is.null(x))
    x <- seq(0, 1, length.out = nrow(colvar))
  if (is.null(y))
    y <- seq(0, 1, length.out = ncol(colvar))

  if (! is.vector(x))
    stop("'x' should be a vector")
    
  if (! is.vector(y))
    stop("'y' should be a vector")

  dot <- splitdotpersp(list(...), bty, FALSE, x, y, z, plist = plist)

  contour <- list(args = dot$points) 
    
  if (! is.matrix(z)) {
    if (length(z) > 1)
      stop("'z'  should be a matrix or one value") 
    contour$side <- z
    z <- matrix(nrow = length(x), ncol = length(y), data = z)  
  } else {
    if (length(x) != nrow(z))
      stop("'x' should be of length = nrow(z)")
    if (length(y) != ncol(z))
      stop("'y' should be of length = ncol(z)")
    contour$side <- "z"
  }
  
  if (any(resfac != 1)) {   # change resolution
    res <- changeres(resfac, x, y, z, colvar)
    x <- res$x ; y <- res$y ; z <- res$z
    colvar <- res$colvar
  }
  
  if (is.null(col))
    col <- jet.col(100)
  contour$args$col <- col
    
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

  if (is.null(clim)) 
    clim <- range(colvar, na.rm = TRUE)
  
  iscolkey <- is.colkey(colkey, col)
  if (iscolkey) 
    colkey <- check.colkey(colkey)
    
  if (dot$clog) {                # log transformation of color-values 
    colvar <- log(colvar)
    clim <- log(clim)
  }

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
                                 
 # contours
  segm <- contourfunc(contour, x, y, colvar, plist, cv = colvar, 
    clim = clim, dDepth = dDepth)
   
  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, dot$clog) 

 # plot and update plist
  plist <- plot.struct.3D(plist, segm = segm, plot = plot)  

  setplist(plist)   
  invisible(plist$mat)
}

