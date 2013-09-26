namespersp <- c("xlim", "ylim", "zlim", "xlab", "ylab", "zlab",
        "main", "sub", "r", "d", "scale", "box", "axes",
        "nticks", "col.ticks", "lwd.ticks",
        "cex.axis", "col.axis", "font.axis", "col.panel",
        "bty", "lwd.panel", "col.grid", "lwd.grid")

## =============================================================================

plot2Drgl <- function(func.name, x, y, colvar, col, NAcol, clim, add, 
  namesextra = NULL, z = NULL, ...) {
  
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  dots <- list(...)

  dots$expand <- dots$ticktype <- dots$zlab <- dots$bty <- dots$add <- NULL
  dots$box <- TRUE
  namesscat <- c(namespersp, namesextra)
  dotpersp <- dots[names(dots) %in% namesscat]

  if (is.null(z)) 
    z <- rep(1, length.out = length(x))
  plot3D:::refresh(FALSE)
  do.call(func.name, c(alist(x = x, y = y, z = z, colkey = FALSE,
    colvar = colvar, col = col, NAcol = NAcol, clim = clim, bty = "b",
    plot = FALSE, add = add, zlab = "", ticktype = "simple"), dotpersp))
  plot3D:::refresh(TRUE)

  dots
}

## =============================================================================

finishplotrgl <- function(dots, namesextra = NULL, add = FALSE) {
#  new <- dots $new
  plist <- getplist()
  plist$type <- "3D" 
  setplist(plist)
  
  do.call("plotrgl", c(dots[!names(dots) %in% c(namespersp, namesextra)], 
     add = add))

  mouseNULL <- is.null(dots$mouseMode)
  if (mouseNULL) 
    dots$mouseMode <- c("zoom", "zoom", "zoom")

  par3d(mouseMode = dots$mouseMode)
  if (mouseNULL) 
    pan3d(3)           # from the help of rgl.setMouseCallbacks

  view3d(phi = 0, fov = 0)
#  decorate3d(zlab = "")
  
  axis3d("x")
  axis3d("y") 
  pp <- getplist()
#  pp$type <- "2D"
  pp$dot$ticktype <- "simple"
  pp$rgl$userMatrix <- par3d("userMatrix")
  setplist(pp) 
}
                                       
