# ==============================================================================
# the 2D scatterplot (lines, points) function, using rgl
# ==============================================================================

scatterrgl <- function(x, y, colvar = NULL, ...,
                    col = NULL, NAcol = "white",
                    clim = NULL, CI = NULL, add = FALSE)  {
# ------------------------------------------------------------------------------
# check input
# ------------------------------------------------------------------------------
  dots <- plot2Drgl("scatter3D", x, y, z = rep(1.001, length.out = length(x)),
    colvar, col, NAcol, clim, add,
    namesextra = c("type", "pch", "cex", "lwd", "lty", "CI"), CI = CI, ...)

  finishplotrgl(dots, namesextra = c("type", "pch", "cex", "lwd", "lty", "CI"),
    add = add)
}
