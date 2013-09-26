# ==============================================================================
# the 2D scatterplot (lines, points) function, using rgl
# ==============================================================================

scatter2Drgl <- function(x, y, colvar = NULL, ...,
                    col = NULL, NAcol = "white",
                    clim = NULL, CI = NULL, dz = 0.1, add = FALSE)  {
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  dots <- plot2Drgl("scatter3D", x, y, 
    colvar, col, NAcol, clim, add, z = rep(1 + dz, length.out = length(x)),
    namesextra = c("type", "pch", "cex", "lwd", "lty", "CI"), CI = CI, ...)

  finishplotrgl(dots, namesextra = c("type", "pch", "cex", "lwd", "lty", "CI"),
    add = add)
}


text2Drgl <- function(x, y, labels, colvar = NULL, ...,
                    col = NULL, NAcol = "white",
                    clim = NULL, CI = NULL, dz = 0.1, add = FALSE)  {
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  dots <- plot2Drgl("text3D", x, y, z = rep(1 + dz, length.out = length(x)),
    colvar, col, NAcol, clim, add, 
    namesextra = c("labels", "cex", "font"), labels = labels, ...)

  finishplotrgl(dots, namesextra = c("labels", "cex", "font"),
    add = add)
}
