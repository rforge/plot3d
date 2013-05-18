## =============================================================================
## 3-d representation using contours in x, y or z
## =============================================================================
# x, y, z vectors or arrays, colvar: array

isosurf3D <- function(x, y, z, colvar, ..., 
                      phi = 40, theta = 40, 
                      level = mean(colvar, na.rm = TRUE), isofunc = createisosurf,
                      col = "grey", border = NA, facets = TRUE, 
                      colkey = list(side = 4), panel.first = NULL,
                      clab = NULL, bty = "b", 
                      ltheta = -135, lphi = 0, shade = 0.5,
                      lighting = FALSE, add = FALSE, plot = TRUE) {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  dot <- splitdotpersp(c(list(...), ltheta = ltheta, lphi = lphi, shade = shade), 
    bty, lighting, x, y, z, plist = plist)

  if (! ispresent(colvar))
    stop("'colvar' has to be defined and be an array of dimension 3")

 # check dimensionality 
  DD <- dim(colvar)
  if (length(DD) != 3)
    stop("'colvar' has to be an array of dimension 3")
  cr <- range(colvar, na.rm = TRUE)
  na <- level[level > cr[2] | level < cr[1]]
  if (length(na) > 0) 
    stop("cannot calculate isosurfaces - change 'level': valid range ", formatC(cr[1]), 
      " - ", formatC(cr[2]))

  nlevel <- length(level)
  
  if (is.null(col)) {
    if (nlevel == 1)
      col <- "grey"
    else
      col <- jet.col(nlevel)
  } else if (length(col) != nlevel)
    stop ("number of colors in 'col' must equal number of levels")
  
  iscolkey <- is.colkey(colkey, col)    
  if (iscolkey) 
    colkey <- check.colkey(colkey)
        
  if (is.null(plist)) {
    do.call("perspbox", c(alist(x = range(x), y = range(y), 
             z = range(z, na.rm = TRUE), phi = phi, theta = theta, 
             plot = plot, colkey = colkey, col = col), 
             dot$persp))
    plist <- getplist()
  }  
  if (is.function(panel.first)) 
      panel.first(plist$mat)         

  Poly <- list()

 # calculate isosurface for all levels
  for (i in 1:nlevel) {
    Tri <- isofunc (x, y, z, colvar, level[i])
   
    Col <- rep(col[i], length.out = nrow(Tri)/3)
    lwd <- dot$points$lwd; if (is.null(lwd)) lwd <- 1
    lty <- dot$points$lty; if (is.null(lty)) lty <- 1

    X <- matrix(nrow = 3, data = Tri[ ,1])
    Y <- matrix(nrow = 3, data = Tri[ ,2])
    Z <- matrix(nrow = 3, data = Tri[ ,3])


 # depth view of the facets
    proj   <- project(colMeans(X), colMeans(Y), colMeans(Z), plist)

    Col <- rep(Col, length.out = ncol(X))
  
    if (! dot$shade$type == "none") 
      Col <- facetcols.tri (Tri, Col, dot$shade)
   
 # border and colors
    Col <- createcolors(facets, border, Col)

    Poly <- list(
       x      = cbind(Poly$x, rbind(X, NA)),
       y      = cbind(Poly$y, rbind(Y, NA)),
       z      = cbind(Poly$z, rbind(Z, NA)),
       col    = c(Poly$col, Col$facet),
       border = c(Poly$border, Col$border),
       lwd    = c(Poly$lwd, rep(lwd, length.out = ncol(X))),
       lty    = c(Poly$lty, rep(lty, length.out = ncol(X))),
       isimg  = c(Poly$isimg, rep(0, length.out = ncol(X))),
       img    = Poly$img,
       proj   = c(Poly$proj, proj))
  }
  
  class(Poly) <- "poly"
  
  if (iscolkey) {
    colkey$at <- 1:nlevel
    colkey$labels <- level
    zlim <- c(0.5, nlevel + 0.5)
    plist <- plistcolkey(plist, colkey, col, zlim, clab, FALSE, type = "isosurf3D") 
  }
  plist <- plot.struct.3D(plist, poly = Poly, plot = plot)  
  
  setplist(plist)   
  invisible(plist$mat)
}

## =============================================================================
## plotting triangles
## =============================================================================

triangle3D  <- function(tri, colvar = NULL, 
                    ..., phi = 40, theta = 40,
                    col = NULL, NAcol = "white", 
                    border = NA, facets = TRUE,
                    colkey = list(side = 4), panel.first = NULL,
                    lighting = FALSE, clim = NULL, clab = NULL,
                    bty = "b", add = FALSE, plot = TRUE)  {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  if (! is.matrix(tri))
    stop("'tri' should be a matrix with 3 columns")
  if (ncol(tri) != 3)
    stop("'tri' should be a matrix with 3 columns")
  if (nrow(tri)%%3 != 0)
    stop("'tri' should be a matrix with number of rows a multiple of 3")
    
 # split in x, y, z
  x <- matrix(nrow = 3, data = tri[, 1])
  y <- matrix(nrow = 3, data = tri[, 2])
  z <- matrix(nrow = 3, data = tri[, 3])
  len <- ncol(x) # number of triangles
  
  dot  <- splitdotpersp(list(...), bty, lighting, x, y, z, plist = plist)

  # colors
  if (ispresent(colvar)) { 
    if (length(colvar) != len)
      stop("'colvar' should have same length as number of triangles")
    
    if (is.null(col))
      col <- jet.col(100)
    
    if (length(col) == 1)
      col <- c(col, col)

    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
    
    if (dot$clog) {                       
      colvar <- log(colvar)
      clim <- log(clim)
    }

    iscolkey <- is.colkey(colkey, col) 
    if (iscolkey) 
      colkey <- check.colkey(colkey)
     
    Col <- variablecol(colvar, col, NAcol, clim) 

  } else {
    if (is.null(col))
      col <- "grey"
    Col <- rep(col, length.out = len)  
    iscolkey <- FALSE
  }   

  if (is.null(plist)) {
    do.call("perspbox", 
           c(alist(x = range(x, na.rm = TRUE), y = range(y, na.rm = TRUE), 
             z = range(z, na.rm = TRUE), 
             phi = phi, theta = theta, plot = plot, 
             colkey = colkey, col = col), dot$persp))
    plist <- getplist()
  }
  if (is.function(panel.first)) 
    panel.first(plist$mat)
  
  lwd <- dot$points$lwd ; if (is.null(lwd)) lwd <- 1
  lty <- dot$points$lty ; if (is.null(lty)) lty <- 1

  proj   <- project(colMeans(x), colMeans(y), colMeans(z), plist)

  if (! dot$shade$type == "none") 
    Col <- facetcols.tri (tri, Col, dot$shade)
   
 # border and colors
  Col <- createcolors(facets, border, Col)

  Poly <- list(x = rbind(x, NA),
               y = rbind(y, NA),
               z = rbind(z, NA), 
               col    = Col$facet,
               border = Col$border,
               lwd    = rep(lwd , length.out = len),
               lty    = rep(lty , length.out = len),
               proj = proj)

  class(Poly) <- "poly"

  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, 
      dot$clog, type = "triangle3D") 

  plist <- plot.struct.3D(plist, poly = Poly, plot = plot)  

  setplist(plist)   
  invisible(plist$mat)
}

