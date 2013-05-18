
## =============================================================================
##  Function to create isosurface triangles of an array
## =============================================================================

createisosurf <- function(x, y, z, colvar, level = mean(colvar, na.rm = TRUE))  {
  Tri <- computeContour3d(vol = colvar, maxvol = max(colvar, na.rm = TRUE), 
     level = level, x = x, y = y, z = z, mask = NULL)
  colnames(Tri) <- c("x","y","z")
  invisible(Tri)
}  
