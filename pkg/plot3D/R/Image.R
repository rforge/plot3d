
## =============================================================================
## =============================================================================
## Image S3 functions   - this code can be improved.
## =============================================================================
## =============================================================================

Image <- function(z, ...) UseMethod ("Image")
Image.default <- function (z, ...) Image.matrix(z, ...)

## =============================================================================
## Image function, input is a matrix
## =============================================================================

Image.matrix <- function (z, x = seq(0, 1, length.out = nrow(z)), 
                   y = seq(0, 1, length.out = ncol(z)), ..., 
                   col = jet.col(100), NAcol = "white", 
                   border = NA, facets = TRUE, 
                   contour = FALSE, colkey = list(side = 4), resfac = 1, 
                   clab = NULL, theta = 0) {

  # check colors  
  if (length(col) == 1)
    if (is.na(col)) col <- NULL
  
 # The plotting arguments
  dots <- splitpardots(list(...))
  dotimage <- dots$main
  dotother <- dots$points

  add <- dots[["add"]]
  if (is.null(add)) add <- FALSE

  iscolkey <- is.colkey(colkey, col)        # check if color key is needed
  if (iscolkey) {
    colkey <- check.colkey(colkey)
    if (!add)
      plt.or <- par(plt = colkey$parplt)
  }  
  
 # check contours
  iscontour <- ! is.null(contour)
  if (length(contour) == 0) 
    iscontour <- FALSE
  else if (is.logical(contour[[1]][1]))
    if (contour[[1]][1] == FALSE) 
    iscontour <- FALSE
  else if (! is.list(contour)) 
    contour <- list()   

 # x- and y-values
  if (length(dim(x)) > 2 | length(dim(y)) > 2)
    stop("'x' or 'y' cannot be an array")
    
  if (is.null (x)) 
    x <- seq(0, 1, length.out = nrow(z))

  if (is.null (y)) 
    y <- seq(0, 1, length.out = ncol(z))

  if (is.matrix(x)) 
    if (! is.matrix(y)) 
      y <- matrix(nrow = nrow(x), ncol = ncol(x), data = y, byrow = TRUE)
     
  if (is.matrix(y)) 
    if (! is.matrix(x)) 
      x <- matrix(nrow = nrow(y), ncol = ncol(y), data = x)

 # change resolution
  if (any(resfac != 1)) {   
    res <- changeres(resfac, x, y, z)
    x <- res$x
    y <- res$y
    z <- res$z
  }
 
  if (iscontour) {
      if (is.matrix(x))
        stop ("cannot add contour if 'x' or 'y' is a matrix")
      contour$x <- x
      contour$y <- y
  }
 
 # rotate 
  rotate <- FALSE
  if (theta != 0) {        
    if (is.vector(x)) {
      rotate <- TRUE
      x <- matrix (nrow = nrow(z), ncol = ncol(z), data = x)
      y <- matrix (nrow = nrow(z), ncol = ncol(z), data = y, byrow = TRUE)
    }

    th <- theta*pi/180
    Mat <- matrix(nrow = 2, data = c(cos(th), sin(th), -sin(th), cos(th)))
    XY <- cbind(as.vector(x), as.vector(y))%*%Mat
    x <- matrix(nrow = nrow(z), ncol = ncol(z), data = XY[, 1])
    y <- matrix(nrow = nrow(z), ncol = ncol(z), data = XY[, 2])
  }

  useimage <- TRUE     # default it to use the image function
  if (is.matrix(x)) {  # ..but not if x and y are matrix!
     if (any (dim(x) - dim(y) != 0))
       stop("matrices 'x' and 'y' not of same dimension") 
     if (any (dim(x) - dim(z) != 0))                             
       stop("matrices 'x' or 'y' and 'z' not of same dimension") 
     useimage <- FALSE
  } 
  
 # Check for decreasing values of x and y    
   if (! is.matrix(x) & all(diff(x) < 0)) {    # swap
        if (is.null(dotimage$xlim)) dotimage$xlim <- rev(range(x))
        x <- rev(x)
        z <- z[nrow(z):1, ]
   }
  
   if (! is.matrix(y) & all(diff(y) < 0)) {    # swap
        if (is.null(dotimage$ylim)) dotimage$ylim <- rev(range(y))
        y <- rev(y)
        z <- z[, (ncol(z):1)]
   }
  
 # log transformation of z-values (can be specified with log = "c", or log = "z"
  zlog <- FALSE 
  if (! is.null(dotimage$log)) {
    if (length(grep("z", dotimage[["log"]])) > 0) {
      dotimage[["log"]] <- gsub("z", "", dotimage[["log"]])
      zlog <- TRUE
    }
    if (length(grep("c", dotimage[["log"]])) > 0) {
      dotimage[["log"]] <- gsub("c", "", dotimage[["log"]])
      zlog <- TRUE
    }

  }
  if (zlog) z <- log(z)

 # labels
  if (is.null(dotimage[["xlab"]])) dotimage[["xlab"]] <- "x"
  if (is.null(dotimage[["ylab"]])) dotimage[["ylab"]] <- "y"

 # z ranges
  zlim <- dotimage[["zlim"]]
  if (is.null(zlim)) 
    zlim <- dotother[["clim"]]
  else if (!is.null(dotother[["clim"]])) 
    stop ("only one of 'zlim' and 'clim' can be specified")
    
  dotimage[["zlim"]] <- dotother[["clim"]] <- NULL
  
  if (is.null(zlim)) {
  
     if (length(which(!is.na(z))) == 0)
        zlim <- c(0, 1)
     else zlim <- range(z, na.rm = TRUE)
  } else 
    if (zlog) zlim  <- log(zlim )
                                
  colkeyZlim <- zlim
  colkeyCol  <- col
  
 # Colors for values = NA 
  if (any (is.na(z)) & ! is.null(NAcol) ) {
      CC <- checkcolors(z, col, NAcol, zlim)
      zlim  <- CC$lim
      col <- CC$col
      z <- CC$colvar
  }

  if (! facets | is.na(facets)) {
    useimage <- FALSE
    if (! is.matrix(x)) {
      xy <- mesh(x,y)
      x <- xy$x
      y <- xy$y  
    }
  }

  if (! useimage) {  # use colored polygons if x- and y are matrix

    # create colors
    zmin   <- zlim[1]
    zrange <- diff(zlim)
    N      <- length(col) -1
    Col    <- matrix(nrow = nrow(z), data = col[1 + trunc((z - zmin)/zrange*N)])

    # polygons are painted in the order in which they were defined
    ix <- rep(1:nrow(x), ncol(x))
    iy <- as.vector(matrix(nrow = nrow(x), ncol = ncol(x), 
                    data = 1:ncol(x), byrow =TRUE))

 # empty plot
    dotimage$type <- "n"
    if (!add) {
      do.call("plot", c(alist(x = x, y = y), dotimage))
    
      plotrect <- !is.null(NAcol)
      if (plotrect) 
        if (NAcol != "white") {
          usr <- par("usr") 
          rect(usr[1], usr[3], usr[2], usr[4], col = NAcol)
        }    
    }

    # function to draw polygon
    poly <- polyfill(x, y, z, Col, NAcol, facets, border, ix, iy, 
      dots$lwd, dots$lty, Extend = TRUE)

    dots$lwd <- NULL
    do.call("polygon", c(alist(poly$x, poly$y, lwd = poly$lwd, 
      border = poly$border, col = poly$col), 
                       dotother))
 
  } else {
    do.call("image", c(alist(z = z, x = x, y = y, col = col, add = add, zlim = zlim), dotimage))
    if (!is.na(border)){
      do.call("abline", c(alist(h = 0.5*(y[-1]+y[-length(y)]), col = border), dotother))
      do.call("abline", c(alist(v = 0.5*(x[-1]+x[-length(x)]), col = border), dotother))
    }
  }
  if (is.null(dots$add)) box()
  
  # contours
  if (iscontour) {
    if (zlog) 
      if (!is.null(contour$levels)) contour$levels <- log(contour$levels)
      
    if (! rotate)
      do.call("contour", c(list(z = z, add = TRUE), contour))
    else {    # first calculate contours on unrotated values, then transform
      line.list <- do.call("contourLines", c(alist(z), contour))
      templines <- function(clines) 
         lines(cbind(clines[[2]], clines[[3]])%*%Mat)              
        invisible(lapply(line.list, templines))
    }
  }  
  
  if (iscolkey)  {
    drawcolkey(colkey, colkeyCol, colkeyZlim, clab, zlog) 
    if (!add)
      par(plt = plt.or)  
    par(mar = par("mar")) 
  }               
   
}

## =============================================================================
## Image function, input is an array
## =============================================================================

Image.array <- function (z, margin = c(1, 2), subset, ask = NULL, ...) {
  
  DD <- dim(z)
  if (length(DD) != 3)
    stop ("Can only make image of 3-D array, 'z' has dimension ", length(DD))

  if (length(margin) != 2)
    stop ("'margin' should contain two numbers, the x, y subscripts of which to make images")
   
  if ( max(margin) > 3 | min (margin) < 1)
    stop ("indexes in 'margin' should be inbetween 1 and 3")

  index <- (1:3) [- margin]

  if (index > 3 || index <1)
    stop ("'index' to loop over should be inbetween 1 and 3")
  
  x <- 1:DD[index]
  
  if (!missing(subset)){
     if (is.numeric(subset)) { 
       isub <- subset
     } else {
      e <- substitute(subset)
      r <- eval(e, as.data.frame(z), parent.frame())
      if (!is.logical(r))
          stop("'subset' must evaluate to logical")
      isub <- r & !is.na(r)
      isub <- which(isub)
      if (length(isub) == 0)
        stop("cannot continue: nothing selected - check 'subset'")
     }   
  } else isub <- x

  np     <- length(isub)
  ldots  <- list(...)
  
  ## Set par mfrow and ask
  ask <- setplotpar(ldots, np, ask)
  if (ask) {
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
  }

  if (is.null(ldots$main)) {
    title <- names(z)[index][isub]
    if (is.null(title)) title <- isub
  } else 
    title <- rep(ldots$main, length.out = length(isub))

  # outer margin text
  Mtext <- ldots$mtext
  ldots$mtext <- NULL

  i1 <- 1
  for (i in isub) {
    if (index == 1) 
      zz <- z[i, , ] 
    else if (index == 2)
      zz <- z[ ,i , ] 
    else
      zz <- z[ ,, i ]
    if (margin[2] < margin[1])
      zz <- t(zz)

    LL <- c(list(z = zz), ldots)
    LL$main <- title[i1]
    i1 <- i1+1
    do.call(Image, LL)

  }
  if (! is.null(Mtext))
     mtext(text = Mtext, side = 3, outer = TRUE, line = par("oma")[3]-1 )
  
}

## =============================================================================
## Image of a list of matrices or arrays
## =============================================================================

Image.list <- function (z, ...) {
  
# check z: list with similar matrices or arrays of dimension at most 3
  if ( all(c("x", "y", "z") %in% names(z)))  {
    Image.matrix(z = z$z, x = z$x, y = z$y, ...)
  } else {
  nz     <- length(z)
  classz <- class(z[[1]])

  if (! classz %in% c("matrix", "array"))
    stop ("'z' should be a list with either matrices or arrays")
    
  DD <- dim(z[[1]])
  if (length(DD) > 3 | length(DD) < 2)
      stop ("Can only make image of 2-D or 3-D array, 'z' has dimension ", length(DD))

  for (i in 2 : nz)
    if (any(dim(z[[i]]) - DD != 0))
      stop("elements of 'z' should have the same dimension, check element", i)
  
# Set the mfrow argument - different from the usual
  if ("matrix" %in% classz)  {
     nc <- min(ceiling(sqrt(nz)), 3)
     nr <- min(ceiling(nz/nc), 3)
  } else { # differs from default in that it is not limited to 3
     nc <- ceiling(sqrt(nz))
     nr <- ceiling(nz/nc)
  }
  mfrow <- c(nr, nc)
  par(mfrow = mfrow)

# Plotting arguments
  Ldots <- list(...) 
  Ldots$mfrow <- mfrow
   
  if (!is.null(Ldots$main)) {
     main <- rep(Ldots$main, length.out = nz)
     Ldots$main <- NULL
  } else {
     main <- names(z)
     if (is.null(main)) main <- 1:nz
  }
  ask <- Ldots$ask
  if (is.null(ask)) ask <- TRUE
  Ldots$ask <- NULL

  # ylim and xlim can be lists and are at least two values
  yylim  <- expanddotslist(Ldots$ylim, nz)
  xxlim  <- expanddotslist(Ldots$xlim, nz)
  zzlim  <- expanddotslist(Ldots$zlim, nz)
  zzlab  <- expanddotslist(Ldots$clab, nz)
   
  if (ask) {
     oask <- devAskNewPage(TRUE)
     on.exit(devAskNewPage(oask))
  }

 # Display the images
  if ("matrix" %in% classz)  {
     # outer margin text
     Mtext <- Ldots$mtext
     Ldots$mtext <- NULL

     for (i in 1:nz) {
      Ldots$main <- main[i]
      Ldots$xlim <- xxlim[[i]]
      Ldots$ylim <- yylim[[i]]
      Ldots$zlim <- zzlim[[i]]
      Ldots$clab <- zzlab[[i]]
      
      LL <- c(list(z = z[[i]]), Ldots)
      do.call(Image, LL)
     }
  
     if (! is.null(Mtext))
       mtext(text = Mtext, side = 3, outer = TRUE, line = par("oma")[3]-1 )
   
  } else {  # array
     margin <- Ldots$margin
     Ldots$margin <- NULL
     
     if (is.null(margin)) margin <- 1:2
     if (length(margin) != 2)
       stop ("'margin' should contain two numbers, the x, y subscripts with which to make images")
     if ( max(margin) > 3 | min (margin) < 1)
       stop ("indexes in 'margin' should be inbetween 1 and 3")
     index <- (1:3) [- margin]

     subset <- Ldots$subset
     Ldots$subset <- NULL
     if (!is.null(subset)){
      if (is.numeric(subset)) { 
        isub <- subset
      } else {        e <- substitute(subset)
        r <- eval(e, as.data.frame(z), parent.frame())
        if (!is.logical(r))
          stop("'subset' must evaluate to logical")
        isub <- r & !is.na(r)
        isub <- which(isub)
        if (length(isub) == 0)
          stop("cannot continue: nothing selected - check 'subset'")
      } 
     } else isub <- 1:DD[index]
     
     nisub     <- length(isub)

     # number of empty plots 
     noplot <- prod(mfrow) - nz
     if (noplot == 0) noplot <- NULL else noplot <- 1:noplot

     # outer margin text
     Mtext <- Ldots$mtext
     Ldots$mtext <- NULL
     
     if (! is.null(Mtext)) 
       Mtext <- rep(Mtext, length.out = nisub)
     else
       Mtext <- isub
     pline <- par("oma")[3]-1  
     # loop first over margin, then over data sets
     for (jj in 1:nisub) {
       j <- isub[jj]      
       for (i in 1:nz) {
        if (index == 1) 
         zz <- z[[i]][j, , ] 
        else if (index == 2)
         zz <- z[[i]][ ,j , ] 
        else
         zz <- z[[i]][ ,, j ]
        if (margin[2] < margin[1])
         zz <- t(zz)
       
        Ldots$main <- main[i]
        Ldots$xlim <- xxlim[[i]]
        Ldots$ylim <- yylim[[i]]
        Ldots$zlim <- zzlim[[i]]
        Ldots$clab <- zzlab[[i]]
        LL <- c(list(z = zz), Ldots)
        do.call(Image, LL)
       }
       # to make sure all figures are drawn
       for (i in noplot) 
         plot(0, type = "n", xlab = "", ylab = "", axes = FALSE, 
              frame.plot = FALSE)
       mtext(text = Mtext[jj], side = 3, outer = TRUE, line = pline)
     }  
    } 
  }
}

## =============================================================================
## Checking and expanding arguments in dots (...) with default
## =============================================================================

expanddots <- function (dots, default, n) {
  dots <- if (is.null(dots)) default else dots
  rep(dots, length.out = n)
}

# lists: e.g. xlim and ylim....
expanddotslist <- function (dots, n) {
  if (is.null(dots)) return(dots)
  dd <- if (!is.list(dots )) list(dots) else dots
  rep(dd, length.out = n)
}

## allow multiple titles for color key legend
##  if (is.null(Ldots$key.title)) {
##    key.title <- ""
##  } else {
##    key.title <- Ldots$key.title
##  }
##  if (np %% (length(key.title)) != 0) 
##   warning("length of key.title is not a multiple of elements in 'z'")
  
## make key.title same length as nz
##  key.title <- rep(key.title, length.out = np)




