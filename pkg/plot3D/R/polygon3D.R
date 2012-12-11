## =============================================================================
## 3-D polygon function
## =============================================================================

polygon3D  <- function(x, y, z, colvar = NULL, 
                    ..., phi = 40, theta = 40,
                    col = NULL, NAcol = "white", 
                    border = NA, facets = TRUE,
                    colkey = list(side = 4), panel.first = NULL,
                    clim = NULL, clab = NULL, bty = "f", 
                    add = FALSE, plot = TRUE)  {

  if (add) 
    plist <- getplist()
  else
    plist <- NULL

  dot  <- splitdotpersp(list(...), bty, NULL, x, y, z, plist = plist)

# checks
  if (length(y) != length(x))
    stop("'y' should have same length as 'x'")
  if (length(z) != length(x))
    stop("'z' should have same length as 'x'")

 # check for NAs (and number of polygons)
  len <- 1
  if (any (is.na(x) | is.na(y) | is.na(z))) {
    i1 <- which(is.na(x))
    i2 <- which(is.na(y))
    i3 <- which(is.na(z))
    ii <- unique(c(i1, i2, i3))
    if (1 %in% ii | length(x) %in% ii)
      stop ("first or last element of 'x', 'y', or 'z' cannot be 'NA'")
    di <- diff(sort(c(0, ii, length(x)+1)))-1
    if (min(di) == 1)
      stop ("two consecutive elements of 'x', 'y', or 'z' cannot be 'NA'")
    x[ii] <- NA
    y[ii] <- NA
    z[ii] <- NA
    len <- length(ii) + 1  # number of polygons!
#    xx <- x; yy <- y; zz <- z
    xx <- yy <- zz <- matrix(nrow = max(di) + 1, ncol = len, data = NA)
    ii <- c(0, ii, length(x)+ 1)
    for (i in 1 : len) {
      iseq <- (ii[i]+1): (ii[i+1]-1)
      xx[1:length(iseq), i] <- x[iseq]
      yy[1:length(iseq), i] <- y[iseq]
      zz[1:length(iseq), i] <- z[iseq]
    }
  } else {
    xx <- matrix(ncol = 1, data = c(x, NA)) 
    yy <- matrix(ncol = 1, data = c(y, NA)) 
    zz <- matrix(ncol = 1, data = c(z, NA)) 
  }
  # colors
  if (ispresent(colvar)) { 
    if (length(colvar) != len)
      stop("'colvar' should have same length as number of polygons (= 1+ number of NAs in 'x', 'y' and 'z')")
    
    if (is.null(col))
      col <- jet.col(100)
    
    if (length(col) == 1)
      col <- c(col, col)

    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
    
    if (dot$clog) {                       # log transformation of color-values 
      colvar <- log(colvar)
      clim <- log(clim)
    }

    iscolkey <- is.colkey(colkey, col) 
    if (iscolkey) 
      colkey <- check.colkey(colkey)
     
    Col <- variablecol(colvar, col, NAcol, clim) # generate color scheme

  } else {
    if (is.null(col))
      col <- "grey"
    Col <- rep(col, length.out = len)  
    iscolkey <- FALSE
  }   

# The colors of facets and border
  Col <- createcolors(facets, border, Col)  

  if (is.null(plist)) {
    do.call("perspbox", 
           c(alist(x = range(x, na.rm = TRUE), y = range(y, na.rm = TRUE), 
             z = range(z, na.rm = TRUE), 
             phi = phi, theta = theta, plot = plot, 
             colkey = colkey, col = col), dot$persp))
    plist <- getplist()
  }
  if (is.function(panel.first)) 
    panel.first(plist$mat)
  
  lwd <- dot$points$lwd ; if (is.null(lwd)) lwd <- 1
  lty <- dot$points$lty ; if (is.null(lty)) lty <- 1


  Poly <- list(x = xx, 
               y = yy, 
               z = zz, 
               col    = Col$facet,
               border = Col$border,
               lwd    = rep(lwd , length.out = len),
               lty    = rep(lty , length.out = len))

 # sort points according to view
  Poly$proj   <- project(colMeans(xx, na.rm = TRUE), colMeans(yy, na.rm = TRUE), 
    colMeans(zz, na.rm = TRUE), plist)

  class(Poly) <- "poly"

  if (iscolkey) 
    plist <- plistcolkey(plist, colkey, col, clim, clab, dot$clog) 

 # plot it
  plist <- plot.struct.3D(plist, poly = Poly, plot = plot)  

  setplist(plist)   
  invisible(plist$mat)
}

