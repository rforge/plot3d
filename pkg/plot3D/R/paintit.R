
## =============================================================================
## Add image polygons, sorted, inputs are all matrices
## =============================================================================

paintit  <- function (colvar, x, y, z, plist, col, NAcol, clim,
                     border, facets, lwd, lty, dotshade, 
                     Extend = FALSE, Polar = FALSE) {        

 # Check the plotting arguments x and y
  if (! is.matrix(x))
    stop("'x' should be a matrix")
  if (! is.matrix(y))
    stop("'y' should be a matrix")

 # adapt color palette and range for values = NA
  if (!ispresent(colvar)) {
    if (ispresent(col))
      Col <- col[1]  # take first color
    else
      Col <- "grey"
    if (Extend)
      Col <- rep(Col, length(x))  
    else  
      Col <- rep(Col, length(x[-1,-1]))  

  } else if (any (is.na(colvar)) & ! is.null(NAcol) ) {
    CC <- checkcolors(colvar, col, NAcol, clim)
    col <- CC$col; colvar <- CC$colvar; clim <- CC$lim  
  }

  cmin   <- clim[1]
  crange <- diff(clim)
  N      <- length(col) -1

  sl <- Sortlist(x, y, z, plist, Extend, Polar)

  # the colors, 1.0000 1 to avoid that trunc(1) = 0  
  if (ispresent(colvar)) 
    Col <- col[1 + trunc((colvar - cmin)/crange*1.00000000001*N)]
  
  if (dotshade$type != "none") 
    Col <- facetcols (x, y, z, Col, dotshade, Extend)

 # Draw colored polygons           
  poly <- polyfill(x, y, z, Col[sl$list], NAcol, facets, border, sl$ix, sl$iy,        
            lwd, lty, Extend, sl$Proj[sl$list])                                   

  invisible(poly)
}

## =============================================================================
## sort facets to draw according to view
## =============================================================================

sortlistvec <- function (x, y, z, plist, ignorez = TRUE) {                      
 
  Proj    <- project(x, y, z, plist, ignorez)
  sortlist <- sort.int(Proj, index.return = TRUE)$ix
  list(list = sortlist, Proj = Proj)
}

Sortlist <- function (x, y, z, plist, Extend, Polar = FALSE) {

  if (Polar & Extend)
    zz <- z
  else if (Polar & !Extend)
    zz <- meangrid(z)
  else 
    zz <- 0

  if (Extend) {
    xx <- x
    yy <- y

  } else  {    # map to centre of grid
    xx <- meangrid(x)
    yy <- meangrid(y)
  }  
  sl <- sortlistvec(as.vector(xx), as.vector(yy), as.vector(zz), plist, !Polar)

  if (Extend)
    ind <- expand.sort(sl$list, dim(x)) # now pointing to row/col
  else
    ind <- expand.sort(sl$list, dim(x)-1) 

  ix <- ind$x; iy <- ind$y

  if (Polar) {
    NN <- length(ix) * 0.5
    ix <- ix[- (1:NN)]
    iy <- iy[- (1:NN)]
    sl$list <- sl$list[- (1:NN)]
    maxProj <- sl$Proj[sl$list[length(sl$list)]]
    sl$Proj <- sl$Proj[sl$Proj <= maxProj] #sl$Proj[sl$list]
  }
  list(ix = ix, iy = iy, list = sl$list, 
    Proj = sl$Proj)
}


## =============================================================================
## Create polygons
## =============================================================================

createpoly <- function (x, y, z, ix, iy, Extend = TRUE) {
    
  if (Extend) {
    xx <- extend(x)
    yy <- extend(y)
    zz <- extend(z)
  } else {
    xx <- x
    yy <- y
    zz <- z
  }
 # the polygons
  PolyX <- rbind(xx[cbind(ix,     iy    )],
                 xx[cbind(ix + 1, iy    )],
                 xx[cbind(ix + 1, iy + 1)],
                 xx[cbind(ix,     iy + 1)], NA)
  PolyY <- rbind(yy[cbind(ix,     iy    )],
                 yy[cbind(ix + 1, iy    )],
                 yy[cbind(ix + 1, iy + 1)],
                 yy[cbind(ix,     iy + 1)], NA)
  PolyZ <- rbind(zz[cbind(ix,     iy    )],
                 zz[cbind(ix + 1, iy    )],
                 zz[cbind(ix + 1, iy + 1)],
                 zz[cbind(ix,     iy + 1)], NA)

  list(X = PolyX, Y = PolyY, Z = PolyZ)

}

## =============================================================================
## Draw polygons
## =============================================================================

polyfill <- function(x, y, z, Col, NAcol, facets, border, ix, iy, 
                     lwd, lty, Extend = FALSE, proj = NULL) {

  Poly <- createpoly(x, y, z, ix, iy, Extend) 

  if (any (is.na(x) | is.na(y) | is.na(z))) {
    i1 <- which(is.na(Poly$X[-5, ]))
    i2 <- which(is.na(Poly$Y[-5, ]))
    i3 <- which(is.na(Poly$Z[-5, ]))
    ii <- unique(c(i1, i2, i3))
    Poly$Y[-5, ][ii] <- NA
    Poly$X[-5, ][ii] <- NA
    Poly$Z[-5, ][ii] <- NA

    ina <- apply (Poly$X[-5, ], MARGIN = 2, FUN = function(x) 
      any(is.na(x)) & !all(is.na(x)))
      
    for (i in (1:ncol(Poly$X)) [ina]){
      ii <- which(!is.na(Poly$X[1:4, i]))
      Poly$X[,i] <- c(Poly$X[ii,i], rep(NA, 5-length(ii)))
      Poly$Y[,i] <- c(Poly$Y[ii,i], rep(NA, 5-length(ii)))
      Poly$Z[,i] <- c(Poly$Z[ii,i], rep(NA, 5-length(ii)))
    }  

 # remove columns with only NAs or with all but one NA
    notNA  <- ! (is.na(Poly$X[2,]))
    Poly$X <- Poly$X[, notNA]
    Poly$Y <- Poly$Y[, notNA]
    Poly$Z <- Poly$Z[, notNA]
    
    if (length(Col) == length(notNA))
      Col <- Col[notNA]
    if (length(border) == length(notNA))
      border <- border[notNA]
    if (length(lwd) == length(notNA))
      lwd <- lwd[notNA]
    if (length(lty) == length(notNA))
      lty <- lty[notNA]
    proj <- proj[notNA]  
  } 

    
# The colors
  Col <- createcolors(facets, border, Col)  

  if (is.null(lwd))
    lwd <- 1
  if (is.null(lty))
    lty <- 1
  
 # update and return polygons.
  poly <- list(
       x      = Poly$X,
       y      = Poly$Y,
       z      = Poly$Z,                                  
       col    = Col$facet,
       border = Col$border,
       lwd    = rep(lwd , length.out = ncol(Poly$X)),
       lty    = rep(lty , length.out = ncol(Poly$X)),
       proj   = proj)
  class(poly) <- "poly"
  return(poly)
    
}
