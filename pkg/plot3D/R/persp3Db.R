## =============================================================================
## main function, with input of colors, no colorvar
## =============================================================================

persp3Db <- function(x = seq(0, 1, length.out = nrow(col) +1), 
                     y = seq(0, 1, length.out = ncol(col) +1), 
                     z, col, ..., 
                     phi = 40, theta = 40, NAcol = "white", 
                     border = NA, facets = TRUE, panel.first = NULL,
                     bty = "b", lighting = FALSE, add = FALSE, plot = TRUE){


  if (nrow(z) != nrow(col) + 1)
    stop ("rows of 'z' should be = rows of 'col' +1")
    
  if (ncol(z) != ncol(col) + 1 )
    stop ("columns of 'z' should be = columns of 'col' + 1")

  if (add) 
    plist <- getplist()
  else
    plist <- NULL
        
  dot <- splitdotpersp(list(...), bty, lighting, x, y, z, plist = plist)

  if (is.null(plist)) {
    do.call("perspbox", c(alist(x, y, z,  
                     phi = phi, theta = theta, plot = plot, 
                     colkey = FALSE, col = col), dot$persp))
    plist <- getplist()
  }
  
  if (is.function(panel.first)) 
    panel.first(plist$mat)         

 # polygon plotting
  if (! is.matrix(x)) { 
    x <- matrix(nrow = nrow(z), ncol = ncol(z), data = x)
    y <- matrix(nrow = nrow(z), ncol = ncol(z), data = y, byrow = TRUE)
  } 
   
  lwd <- ifelse (is.null (dot$points$lwd), 1, dot$points$lwd)
  lty <- ifelse (is.null (dot$points$lty), 1, dot$points$lty)

  sl <- Sortlist(x, y, z, plist, Polar = FALSE)

  if (dot$shade$type != "none") 
    col <- facetcols (x, y, z, col, dot$shade, Extend = FALSE)

 # Draw colored polygons           
  Poly <- list()
  Poly$img <- list(list(x = x, y = y, z = z, col = col, sl = sl, 
    NAcol = NAcol, facets = facets, border = border, lwd = lwd, 
    lty = lty))

 # plot it
  plist <- plot.struct.3D(plist, poly = Poly, plot = plot)  

  setplist(plist)   
  invisible(plist$mat)
}                  

