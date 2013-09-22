## ====================================================================
## A local environment for non user-visible data,
## ====================================================================

.plot3D <- new.env()

.plot3D$plist <- list()

getplist <- function()
  .plot3D$plist

setplist <- function(plist)
  .plot3D$plist <- plist

initplist <- function(add) {
  if (add) 
    plist <- getplist()
  else
    plist <- NULL
# test for setting the correct plt parameters:
  if (!add) {
    plot.new()
    par(new = TRUE)
  }  
  plist
}    