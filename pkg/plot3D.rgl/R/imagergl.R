                                       
# ==============================================================================
# the 2D image and contour function, using rgl
# ==============================================================================

imagergl <- function(z, x = seq(0, 1, length.out = nrow(z)),
                    y = seq(0, 1, length.out = ncol(z)), ...,
                    col = jet.col(100), NAcol = "white", border = NA,
                    contour = FALSE, smooth = FALSE, 
                    clim = NULL, inttype = 1, add = FALSE)  {
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  namesextra <- c("border", "inttype")
  dots <- plot2Drgl("image3D", x, y, colvar = z,
                    col, NAcol, clim, 
                    add, namesextra = namesextra,  z = 1, 
                    border = border,
                    inttype = inttype, smooth = smooth, ...)
                    
  iscontour <- contour
  if (! is.logical(iscontour))
    iscontour <- TRUE
  else
    contour <- list()

  if (iscontour)  {
    if (is.null(contour$col))
      contour$col <- "black"
    do.call("contour3D", c(alist(z = 1.001, x = x, y = y, colvar = z, 
      add = TRUE, plot = FALSE), contour))
  }
  finishplotrgl(dots, namesextra = namesextra, add = add)  
  
}

# ==============================================================================

contourrgl <- function(z, x = seq(0, 1, length.out = nrow(z)),
                    y = seq(0, 1,  length.out = ncol(z)), ...,
                    col = NULL,   clim = NULL, add = FALSE)  {
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  namesextra <- c("lty", "lwd")
  dots <- list(...)

  dots$expand <- dots$ticktype <- dots$zlab <- NULL
  dots$box <- TRUE
  namesscat <- c(namespersp, namesextra)
  dotpersp <- dots[names(dots) %in% namesscat]

  do.call("contour3D", c(alist(x = x, y = y, z = 1.001, colkey = FALSE,
    colvar = z, col = col, clim = clim, bty = "b",
    plot = FALSE, add = add, zlab = "", ticktype = "simple"), dotpersp))

  finishplotrgl(dots, namesextra = namesextra, add = add)  
  
}



