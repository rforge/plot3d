## =============================================================================
## 3-d representation using contours in x, y or z
## =============================================================================
# x, y, z vectors or arrays, colvar: array

isosurf3D <- function(x, y, z, colvar, ..., 
                      phi = 40, theta = 40, 
                      level = mean(colvar),
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
  
  iscolkey <- is.colkey(colkey, col)    # check if colkey is needed
  if (iscolkey) 
    colkey <- check.colkey(colkey)
        
 # call persp without doing anything
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
    Tri <- createisosurf(x, y, z, colvar, level[i])
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
       proj   = c(Poly$proj, proj))
  }
  
  class(Poly) <- "poly"
  
  if (iscolkey) {
    colkey$at <- 1:nlevel
    colkey$labels <- level
    zlim <- c(0.5, nlevel + 0.5)
    plist <- plistcolkey(plist, colkey, col, zlim, clab, FALSE) 
  }
  plist <- plot.struct.3D(plist, poly = Poly, plot = plot)  
  
  setplist(plist)   
  invisible(plist$mat)
}
