# ==============================================================================
# the 2D arrows function, using rgl
# ==============================================================================

arrowsrgl <- function(x0, y0, x1, y1, colvar = NULL, ...,
                    col = NULL, NAcol = "white",
                    clim = NULL, type = "simple", 
                    add = FALSE)  {

# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  dots <- list(...)

  dots$expand <- dots$ticktype <- dots$zlab <- NULL
  dots$box <- TRUE
  namesextra <- c("code", "length", "angle", "lwd", "lty", "type")  
  dotpersp <- dots[names(dots) %in% c(namesextra,namespersp)]

  z <- rep(1.001, length.out = length(x0))
  do.call("arrows3D", c(alist(x0 = x0, y0 = y0, z0 = z, 
    x1 = x1, y1 = y1, z1 = z, colkey = FALSE, type = type,
    colvar = colvar, col = col, NAcol = NAcol, clim = clim, bty = "b",
    plot = FALSE, add = add, zlab = "", ticktype = "simple"), dotpersp))

  finishplotrgl(dots, namesextra, add = add)
}


# ==============================================================================
# the 2D arrows function, using rgl
# ==============================================================================

segmentsrgl <- function(x0, y0, x1, y1, colvar = NULL, ...,
                    col = NULL, NAcol = "white",
                    clim = NULL, type = "simple", 
                    add = FALSE)  {

# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  dots <- list(...)

  dots$expand <- dots$ticktype <- dots$zlab <- NULL
  dots$box <- TRUE
  namesextra <- c("lwd", "lty")  
  dotpersp <- dots[names(dots) %in% c(namesextra,namespersp)]

  z <- rep(1.001, length.out = length(x0))
  do.call("segments3D", c(alist(x0 = x0, y0 = y0, z0 = z, 
    x1 = x1, y1 = y1, z1 = z, colkey = FALSE, type = type,
    colvar = colvar, col = col, NAcol = NAcol, clim = clim, bty = "b",
    plot = FALSE, add = add, zlab = "", ticktype = "simple"), dotpersp))

  finishplotrgl(dots, namesextra, add = add)
}




