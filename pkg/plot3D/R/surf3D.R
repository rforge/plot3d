## =============================================================================
## 3-d surfaces
## =============================================================================
# x, y, z, colvar: matrices

surf3D <- function(x, y, z, colvar = z, ..., 
                   phi = 40, theta = 40,
                   col = jet.col(100), NAcol = "white", 
                   border = NA, facets = TRUE,
                   colkey = list(side = 4), 
                   panel.first = NULL,
                   clim = NULL, clab = NULL, bty = "n",
                   lighting = FALSE, inttype = 1, 
                   add = FALSE, plot = TRUE) {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  dot <- splitdotpersp(list(...), bty, lighting, x, y, z, plist = plist)
  
 # check validity, class and dimensionality
  if (! is.matrix(x))
    stop("'x' should be a matrix")
  if (! is.matrix(y))
    stop("'y' should be a matrix")
  if (! is.matrix(z))
    stop("'z' should be a matrix")
  if (ispresent(colvar))
    if (! is.matrix(colvar))
      stop("'colvar' should be a matrix or absent")

  DD <- dim(x)
  if (any (DD != dim(y)) )
    stop("dimension of 'x' not equal to dimension of 'y'")
  if (any (DD != dim(z)) )
    stop("dimension of 'x' not equal to dimension of 'z'")

  if (ispresent(colvar)) {

    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)     
  
    if (dot$clog) {     
      colvar <- log(colvar)
      clim <- log(clim) 
    }

    iscolkey <- is.colkey(colkey, col)
    if (iscolkey) 
      colkey <- check.colkey(colkey)
  
  } else 
    iscolkey <- FALSE

  CC <- check.colvar.persp (colvar, z, col, inttype, clim)
  col <- CC$col
  colvar <- CC$colvar
  
  Extend <- inttype == 2

  if (is.null(plist)) {
    do.call("perspbox", c(alist(x = range(x), y = range(y), 
             z = range(z, na.rm = TRUE),
             phi = phi, theta = theta, plot = plot, 
             colkey = colkey, col = col), dot$persp))
    plist <- getplist()
  }
  if (is.function(panel.first)) 
    panel.first(plist$mat)  
           
 # painters algorithm
  Poly <- paintit(colvar, x, y, z, plist, col, NAcol, clim, border, 
          facets, dot$points$lwd, dot$points$lty, dot$shade, Extend)

  if (iscolkey)  
    plist <- plistcolkey(plist, colkey, col, clim, clab, 
      dot$clog, type = "surf3D") 

  plist <- plot.struct.3D(plist, poly = Poly, plot = plot)  

  setplist(plist)  
  invisible(plist$mat)
}

