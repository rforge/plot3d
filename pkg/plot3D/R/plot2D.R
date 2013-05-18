plot2Dplist <- function (plist, ...) {
  
  checkdots <-  function(pdots, dots, add) {
    if (! add) {
      if (! is.null(dots$xlim)) 
        pdots$xlim <- dots$xlim
      if (! is.null(dots$ylim)) 
        pdots$ylim <- dots$ylim
    }
    pdots
  }
  
  img2Dnr <- cont2Dnr <- scat2Dnr <- arr2Dnr <- segm2Dnr <- rect2Dnr <- poly2Dnr <- 0
  add <- FALSE
  dots <- list(...)
  p <- plist$twoD
  for (i in 1:length(p$order)) {
    plt <- p$order[i]
    if (plt  == "image") {
      img2Dnr <- img2Dnr + 1
      Dots <- checkdots(p$img2D[[img2Dnr]], dots, add)
      do.call ("image2D", c(alist(add = add), Dots))
    } else if (plt  == "contour") {
      cont2Dnr <- cont2Dnr + 1
      Dots <- checkdots(p$cont2D[[cont2Dnr]], dots, add)
      do.call ("contour2D", c(alist(add = add), Dots))
    } else if (plt == "scatter") {
      scat2Dnr <- scat2Dnr + 1
      Dots <- checkdots(p$scat2D[[scat2Dnr]], dots, add)
      do.call ("scatter2D", c(alist(add = add), Dots))
    } else if (plt == "arrows") {
      arr2Dnr <- arr2Dnr + 1
      Dots <- checkdots(p$arr2D[[arr2Dnr]], dots, add)
      do.call ("arrows2D", c(alist(add = add), Dots))
    } else if (plt == "segments") {
      segm2Dnr <- segm2Dnr + 1
      Dots <- checkdots(p$segm2D[[segm2Dnr]], dots, add)
      do.call ("segments2D", c(alist(add = add), Dots))
    } else if (plt == "rect") {
      rect2Dnr <- rect2Dnr + 1
      Dots <- checkdots(p$rect2D[[rect2Dnr]], dots, add)
      do.call ("rect2D", c(alist(add = add), Dots))
    } else if (plt == "polygon") {
      poly2Dnr <- poly2Dnr + 1
      Dots <- checkdots(p$poly2D[[poly2Dnr]], dots, add)
      do.call ("polygon2D", c(alist(add = add), Dots))
    }
    add <- TRUE
  }
  invisible(plist)
}

add2Dplist <- function(plist, method, ...) {
  if (is.null(plist))
    plist <- list(type = "2D", twoD = list(order = NULL))
  if (plist$type == "3D")
    plist$type <- "23D" 
#    stop ("cannot merge 2D and 3D plotting functions")
  p <- plist$twoD 
  p$order <- c(p$order, method)
  
  if (method == "image") {
    if (is.null(p$img2Dnr)) {
        p$img2Dnr <- 0
        p$img2D <- list()
    } 
    p$img2Dnr <- p$img2Dnr + 1
    p$img2D[[p$img2Dnr]] <- list(...)
  }
    
  else if (method == "contour") {
    if (is.null(p$cont2Dnr)) {
        p$cont2Dnr <- 0
        p$cont2D <- list()
    } 
    p$cont2Dnr <- p$cont2Dnr + 1
    p$cont2D[[p$cont2Dnr]] <- list(...)
  }

  else if (method == "scatter") {
    if (is.null(p$scat2Dnr)) {
        p$scat2Dnr <- 0
        p$scat2D <- list()
    } 
    p$scat2Dnr <- p$scat2Dnr + 1
    p$scat2D[[p$scat2Dnr]] <- list(...)
  }
    
  else if (method == "arrows") {
    if (is.null(p$arr2Dnr)) {
        p$arr2Dnr <- 0
        p$arr2D <- list()
    } 
    p$arr2Dnr <- p$arr2Dnr + 1
    p$arr2D[[p$arr2Dnr]] <- list(...)
  }
    
  else if (method == "segments") {
    if (is.null(p$segm2Dnr)) {
        p$segm2Dnr <- 0
        p$segm2D <- list()
    } 
    p$segm2Dnr <- p$segm2Dnr + 1
    p$segm2D[[p$segm2Dnr]] <- list(...)
  }

  else if (method == "rect") {
    if (is.null(p$rect2Dnr)) {
        p$rect2Dnr <- 0
        p$rect2D <- list()
    } 
    p$rect2Dnr <- p$rect2Dnr + 1
    p$rect2D[[p$rect2Dnr]] <- list(...)
  }
  else if (method == "polygon") {
    if (is.null(p$poly2Dnr)) {
        p$poly2Dnr <- 0
        p$poly2D <- list()
    } 
    p$poly2Dnr <- p$poly2Dnr + 1
    p$poly2D[[p$poly2Dnr]] <- list(...)
  }
  plist$twoD <- p
  class(plist) <- c("plist","list")
  plist
}

## =============================================================================
## plots (2-D)
## =============================================================================

# x, y, colvar: vector or matrix of same dimension

plot2D <- function(x0, y0, x1, y1, ..., colvar = NULL,
                    col = NULL, NAcol = "white",
                    colkey = list(side = 4),
                    clim = NULL, clab = NULL, add = FALSE,
                    method = "arrows") {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  plist <- add2Dplist(plist, method, x0 = x0, y0 = y0, x1 = x1, y1 = y1, 
    colvar = colvar, col = col, NAcol = NAcol, colkey = colkey, clim = clim,
    clab = clab, ...)
  setplist(plist)

  dots <- splitpardots(list(...))

 # colors
  if (! is.null(colvar)) {
    if (is.null(col))
      col <- jet.col(100)

    if (dots$clog) {
      colvar <- log(colvar)
      if (! is.null(clim)) clim <- log(clim)
    }

    iscolkey <- is.colkey(colkey, col)

    if (iscolkey) {
      colkey <- check.colkey(colkey, add)
      if (! add)
        par.ori <- par(plt = colkey$parplt)
    }

    if (length(colvar) != length(x0))
      stop ("length of 'colvar' should be equal to length of 'x0', 'x1', 'y0' and 'y1'")

    if (is.null(clim))
      clim <- range(colvar, na.rm = TRUE)

    Col <- variablecol(colvar, col, NAcol, clim)

  } else  {  # no colvar
    Col <- col
    if (is.null(Col)) Col <- "black"
    iscolkey <- FALSE
  }

  if (! add)
    dots$main <- start2Dplot(dots$main, c(x0, x1), c(y0, y1))
    
  do.call(method, c(alist(x0, y0, x1, y1, col = Col), dots$points))

  if (iscolkey) {
    drawcolkey(colkey, col, clim, clab, dots$clog)
    if (! add)
      par(plt = par.ori)
    par(mar = par("mar"))
  }

}

arrows2D <- function(x0, y0, x1 = x0, y1 = y0, ..., colvar = NULL,
                    col = NULL, NAcol = "white",
                    colkey = list(side = 4),
                    clim = NULL, clab = NULL, add = FALSE)  
  plot2D (x0, y0, x1, y1, ..., colvar = colvar,
                    col = col, NAcol = NAcol,
                    colkey = colkey,
                    clim = clim, clab = clab, add = add,
                    method = "arrows")

segments2D <- function(x0, y0, x1 = x0, y1 = y0, ..., colvar = NULL,
                    col = NULL, NAcol = "white",
                    colkey = list(side = 4),
                    clim = NULL, clab = NULL, add = FALSE) 
  plot2D (x0, y0, x1, y1, ..., colvar = colvar,
                    col = col, NAcol = NAcol,
                    colkey = colkey,
                    clim = clim, clab = clab, add = add,
                    method = "segments")

rect2D <- function(x0, y0, x1 = x0, y1 = y0, ..., colvar = NULL,
                    col = NULL, NAcol = "white",
                    colkey = list(side = 4),
                    clim = NULL, clab = NULL, add = FALSE) 
  plot2D (x0, y0, x1, y1, ..., colvar = colvar,
                    col = col, NAcol = NAcol,
                    colkey = colkey,
                    clim = clim, clab = clab, add = add,
                    method = "rect")

