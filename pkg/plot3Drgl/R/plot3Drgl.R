namespersp <- c("xlim", "ylim", "zlim", "xlab", "ylab", "zlab",
        "main", "sub", "r", "d", "scale", "expand", "box", "axes", 
        "cex", "pch", "lwd", "lty", "type",
        "nticks", "col.ticks", "lwd.ticks", "ticktype",
        "cex.axis", "col.axis", "font.axis", "col.panel",
        "bty", "lwd.panel", "col.grid", "lwd.grid")


plot3Dfunrgl <- function(funcname, ...)  {
  form <- c(names(formals(funcname)), namespersp, "")
  dots <- list(...)
  nd <- names(dots)   
  plot <- dots$plot
  if (is.null(plot))
    plot <- TRUE
  dots$plot <- FALSE
  if (is.null(dots$add))
    dots$add <- FALSE
  irgl <- unique(c(which(!nd %in% form ), 
            which(nd %in% c("lighting", "smooth"))))
  rgldots <- c(dots[irgl], add = dots$add)
  dots[irgl] <- NULL
  if(dots$add)
    rgldots$new <- FALSE
  plot3D:::refresh(FALSE)
  do.call(funcname, dots)
  plot3D:::refresh(TRUE)
  if (plot) 
    do.call ("plotrgl", rgldots) 
}

persp3Drgl <- function(...)
  plot3Dfunrgl("persp3D", ...)
ribbon3Drgl <- function(...)
  plot3Dfunrgl("ribbon3D", ...)
hist3Drgl <- function(...)
  plot3Dfunrgl("hist3D", ...)
scatter3Drgl <- function(...)
  plot3Dfunrgl("scatter3D", ...)
points3Drgl <- function(...)
  plot3Dfunrgl("points3D", ...)
lines3Drgl <- function(...)
  plot3Dfunrgl("lines3D", ...)
slice3Drgl <- function(...)
  plot3Dfunrgl("slice3D", ...)
slicecont3Drgl <- function(...)
  plot3Dfunrgl("slicecont3D", ...)
isosurf3Drgl <- function(...)
  plot3Dfunrgl("isosurf3D", ...)
voxel3Drgl <- function(...)
  plot3Dfunrgl("voxel3D", ...)
surf3Drgl <- function(...)
  plot3Dfunrgl("surf3D", ...)
spheresurf3Drgl <- function(...)
  plot3Dfunrgl("spheresurf3D", ...)
segments3Drgl <- function(...)
  plot3Dfunrgl("segments3D", ...)
box3Drgl <- function(...)
  plot3Dfunrgl("box3D", ...)
border3Drgl <- function(...)
  plot3Dfunrgl("border3D", ...)
rect3Drgl <- function(...)
  plot3Dfunrgl("rect3D", ...)
text3Drgl <- function(...)
  plot3Dfunrgl("text3D", ...)
image3Drgl <- function(...)
  plot3Dfunrgl("image3D", ...)
contour3Drgl <- function(...)
  plot3Dfunrgl("contour3D", ...)
  