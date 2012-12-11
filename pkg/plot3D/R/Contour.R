Contour <- function (z, x = seq(0, 1, length.out = nrow(z)),
                   y = seq(0, 1, length.out = ncol(z)), ...,
                   col = NULL,
                   colkey = list(side = 4), resfac = 1,
                   clab = NULL) {

  # check colors
  if (length(col) == 1)
    if (is.na(col)) col <- NULL

 # The plotting arguments
  dots <- list(...)

 # z ranges
  zlim <- dots[["zlim"]]
  if (is.null(zlim))
    zlim <- dots[["clim"]]
  else if (!is.null(dots[["clim"]]))
    stop ("only one of 'zlim' and 'clim' can be specified")

  dots[["zlim"]] <- dots[["clim"]] <- NULL

  if (is.null(zlim)) {

     if (length(which(!is.na(z))) == 0)
        zlim <- c(0, 1)
     else zlim <- range(z, na.rm = TRUE)
  } else
    if (zlog) zlim  <- log(zlim )

  levels <- dots$levels; dots$levels <- NULL
  
  if (is.null(levels)) {
    nlevs <- dots$nlevels

    if (is.null(nlevs))
      nlevs <- 10

    levels <- pretty(zlim, nlevs)
  }
  nlevs <- length(levels)

  if (is.null(col))
    col <- jet.col(nlevs)
    
  if (nlevs > 1) {
   # for colors: 
    dz <- c(-diff(levels[1:2]), diff(levels[(nlevs-1):nlevs])) * 0.5
    zlim <- range(levels) + dz
  }
  
  add <- dots[["add"]]
  if (is.null(add)) add <- FALSE

  iscolkey <- is.colkey(colkey, col)        # check if color key is needed

  if (iscolkey) {
    colkey <- check.colkey(colkey)
    if (!add)
      plt.or <- par(plt = colkey$parplt)
  }

 # x- and y-values
  if (! is.vector(x) | ! is.vector(y))
    stop("'x' and 'y' must be a vector")

  if (is.null (x))
    x <- seq(0, 1, length.out = nrow(z))

  if (is.null (y))
    y <- seq(0, 1, length.out = ncol(z))

 # change resolution
  if (any(resfac != 1)) {
    res <- changeres(resfac, x, y, z)
    x <- res$x
    y <- res$y
    z <- res$z
  }

 # Check for decreasing values of x and y
   if (! is.matrix(x) & all(diff(x) < 0)) {    # swap
     if (is.null(dots$xlim))
       dots$xlim <- rev(range(x))
      x <- rev(x)
      z <- z[nrow(z):1, ]
   }

   if (! is.matrix(y) & all(diff(y) < 0)) {    # swap
     if (is.null(dots$ylim))
       dots$ylim <- rev(range(y))
     y <- rev(y)
     z <- z[, (ncol(z):1)]
   }

 # log transformation of z-values (can be specified with log = "c", or log = "z"
  zlog <- FALSE
  if (! is.null(dots$log)) {
    if (length(grep("z", dots[["log"]])) > 0) {
      dots[["log"]] <- gsub("z", "", dots[["log"]])
      zlog <- TRUE
    }
    if (length(grep("c", dots[["log"]])) > 0) {
      dots[["log"]] <- gsub("c", "", dots[["log"]])
      zlog <- TRUE
    }
  }
  if (zlog) 
    z <- log(z)

 # labels
  if (is.null(dots[["xlab"]])) dots[["xlab"]] <- "x"
  if (is.null(dots[["ylab"]])) dots[["ylab"]] <- "y"

  colkeyCol  <- col

  # contours
  if (zlog)
    if (!is.null(dots$levels))
      dots$levels <- log(dots$levels)

  do.call("contour", c(list(z = z, x = x, y = y, col = col, levels = levels), dots))

  if (iscolkey)  {
    colkey$at <- levels
    drawcolkey(colkey, colkeyCol, zlim, clab, zlog)
    if (!add)
      par(plt = plt.or)
    par(mar = par("mar"))
  }

}
