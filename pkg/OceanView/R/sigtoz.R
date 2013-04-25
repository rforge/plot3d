## =============================================================================
## =============================================================================
## Maps or extracts from variables defined in sigma coordinates 
## =============================================================================
## =============================================================================

mapsigma <- function(var, ...) UseMethod ("mapsigma")

## =============================================================================

mapsigma.matrix <- function (var = NULL, 
                             sigma,      
                             x = NULL,
                             depth = NULL,
                             numdepth = NULL, 
                             xto = NULL,
                             resfac = 1, ...) {
  if (is.null(var)) 
    var <- matrix(nrow = nrow(sigma), ncol = ncol(sigma), data = 1)

  if (any (dim(var) - dim(sigma) != 0))
    stop ("'sigma' and 'var' not of same dimension")
    
  if (is.null(x))
    x <- seq(0, 1, length.out = nrow(var))
  
  yto <- y <-   z <-  seq(0, 1, length.out = ncol(var) )
  resfac <- abs(rep(resfac, length.out = 2))
  changeres <- FALSE
  if (any(resfac > 1)) {
    if (is.null(xto))
      xto <- changeresvec(x, resfac[1])
    yto <- changeresvec(y, resfac[2])
    changeres <- TRUE
  } else 
    if (! is.null(xto) )
      changeres <- TRUE

  if (changeres) {
    var <- map(var, x = x, y = y, xto = xto, yto = yto)$var
    sigma <- map(sigma, x = x, y = y, xto = xto, yto = yto)$var
  }
  
  Nr <- nrow(var)
  Nc <- ncol(var)
    
  if (is.null(depth))
    depth <- seq(min(sigma, na.rm = TRUE), max(sigma, na.rm = TRUE), 
               length.out = ifelse(is.null(numdepth),ncol(sigma),numdepth))

  if (nrow(sigma) != Nr | ncol(sigma) != Nc)
    stop ("'sigma' should be of same dimension as matrix 'var'")
 
  Mnew <- matrix(nrow = Nr, ncol = length(depth), data =NA)
  
  for (i in 1:Nr) 
    if (any(! is.na(sigma[i, ])))
      Mnew[i,] <- Approx(x = sigma[i,], y = var[i,], xout = depth)$y 
     
  list (var = Mnew, depth = depth, x = xto)
}

## =============================================================================

mapsigma.array <- function (var = NULL, 
                            sigma,
                            x = NULL,
                            y = NULL, 
                            depth = NULL,
                            numdepth = NULL, 
                            xto = NULL,
                            yto = NULL,
                            resfac = 1, ...) {
  if (is.null(var)) 
    var <- array(dim = dim(sigma), data = 1)

  DD <- dim(var)
  Ds <- dim(sigma)
    
  if (any (DD - Ds != 0))
    stop ("'sigma' should be of same dimension as 'var'")
    
  lenD <- length(DD)
  if (lenD > 3)
    stop ("dimension of 'sigma' or 'var' can not be > 3")
    
  if (is.null(x))
    x <- seq(0, 1, length.out = DD[1])

  if (is.null(y))
    y <- seq(0, 1, length.out = DD[2])

  zto <- z <- seq(0, 1, length.out = DD[3])

  
  resfac <- abs(rep(resfac, length.out = 3))
  changeres <- FALSE
  if (any(resfac > 1)) {
    if (is.null(xto))
      xto <- changeresvec(x, resfac[1])
    if (is.null(yto))
      yto <- changeresvec(y, resfac[2])
    zto <- changeresvec(z, resfac[3])
    changeres <- TRUE
  } else 
    if (! is.null(xto) | ! is.null(yto) )
      changeres <- TRUE

  if (changeres) {
    var <- map(var, x = x, y = y, z = z, xto = xto, yto = yto, zto = zto)$var
    sigma <- map(sigma, x = x, y = y, z = z, xto = xto, yto = yto, zto = zto)$var
  }

  if (is.null(depth))
    depth <- seq(min(sigma, na.rm = TRUE), max(sigma, na.rm = TRUE), 
                length.out = ifelse(is.null(numdepth), dim(sigma)[lenD], numdepth))

  Mnew <- array(dim = c( DD[1:2], length(depth)), data = NA)
  
  for (i in 1:DD[1])
    for (j in 1:DD[2]) {
      if (any(! is.na(sigma[i,j,])))
        Mnew[i,j,] <- Approx(x = sigma[i,j,], y = var[i,j,], xout = depth)$y 
    }
  list (var = Mnew, depth = depth)
} 
## =============================================================================
## Transect in sigma coordinates
## =============================================================================

transectsigma  <- function (var = NULL, 
                            sigma,
                            x = NULL,
                            y = NULL,
                            to, 
                            depth = NULL,
                            numdepth = NULL,
                            resfac = 1, ...) {

  if (is.null(var)) 
    var <- array(dim = dim(sigma), data = 1)

  DD <- dim(var)
  Ds <- dim(sigma)
    
  if (any (DD - Ds != 0))
    stop ("'sigma' should be of same dimension as 'var'")
    
  lenD <- length(DD)
  if (lenD != 3)
    stop ("dimension of 'sigma' or 'var' should be 3")
    
  z <-  seq(0, 1, length.out = DD[3])


  tranVar <- transect(var = var, x = as.vector(x),
     y = as.vector(y), z = z, to = to)

  tranSigma <- transect(var = sigma, x = as.vector(x),
    y = as.vector(y), z = z, to = to)

  VarSigma <- mapsigma(tranVar$var, x = to[,1], y = to[,2],
     sigma = tranSigma$var, depth = depth, numdepth = numdepth, resfac = resfac, ...)
  
  return(VarSigma)
  
}
