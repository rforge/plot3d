## =============================================================================
## Scatterplots (2-D)
## =============================================================================
# x, y, colvar: vector or matrix of same dimension

scatter <- function(x, y, colvar = NULL, ..., 
                    col = NULL, NAcol = "white", 
                    colkey = list(side = 4), 
                    clim = NULL, clab = NULL, CI = NULL, add = FALSE) {

  dots <- list(...)

  isCI <- is.list(CI)
  if (isCI) 
    CI <- check.CI(CI, length(x), 2)
  
  clog <- FALSE 
  if (! is.null(dots$log)) {
    if (length(grep("c", dots[["log"]])) > 0) {
      dots[["log"]] <- gsub("c", "", dots[["log"]])
      if (dots[["log"]] == "")
        dots[["log"]] <- NULL
      clog <- TRUE
    }
  }

 # colors
  if (! is.null(colvar)) {
    if (is.null(col))
      col <- jet.col(100)

    if (clog) {
      colvar <- log(colvar)
      if (! is.null(clim)) clim <- log(clim) 
    }
    
    iscolkey <- is.colkey(colkey, col)        

    if (iscolkey) {
      colkey <- check.colkey(colkey, add)
      if (! add)       
        par.ori <- par(plt = colkey$parplt)  
    }  

    if (length(colvar) != length(x)) 
      stop ("length of 'colvar' should be equal to length of 'x' and 'y'")

    if (is.null(clim)) 
      clim <- range(colvar, na.rm = TRUE)
    
    Col <- variablecol(colvar, col, NAcol, clim) 

  } else  {  # no colvar
    Col <- col
    if (is.null(Col)) Col <- "black"
    iscolkey <- FALSE
  }   

  useSegments <- FALSE
  
  if (! is.null(dots$type))
    if (dots$type %in% c("b", "l", "o"))
      if (length(Col) > 1  )
        useSegments <- TRUE

 # function that makes a box type 
  startplot<- function(){
    dd <- dots
    dd$type <- "n"
    
    bty <- dots$bty  
    dots$bty <<- NULL
    
    if (is.null(bty)) 
      bty <- "o"

    if (bty %in% c("b2", "g", "bl")) {
      dd$bty <- NULL
    }     
    do.call("plot", c(alist(x, y, col = Col), dd)) 

    if (bty == "b2")
      grid(col = "grey", lty = 1, lwd = 2)
    else if (bty == "g") {
      pu <- par("usr")
      rect(pu[1], pu[3], pu[2], pu[4], 
        col = grey(0.925), border = grey(0.925))
      grid(col = "white", lty = 1, lwd = 2)
    } else if (bty %in% c("bl","bl2")) {
      pu <- par("usr")
      rect(pu[1], pu[3], pu[2], pu[4], col = "black")
      if (bty == "bl2") 
        grid(col = "grey", lty = 1, lwd = 2)
    }
  }
  
  if (useSegments) {
    Type <- dots$type
    len <- length(x)
    if (Type %in% c("b", "o"))   # no distinction is made..
      dots$type <- "p" 
    else     
      dots$type <- "n" 

   # mean of point colors for line colors
    LCol <- cbind(Col[-1], Col[-len])
    LCol <- apply(LCol, MARGIN = 1, FUN = MeanColors)

    if (! add) 
      startplot()
    add <- TRUE
    if (isCI) {plot.CI.2d(CI, x, y, Col) ; isCI <- FALSE}  # do this first
    do.call("points", c(alist(x, y, col = Col), dots)) 
    dots$type <- NULL
    do.call("segments", c(alist(x[-len], y[-len], x[-1], y[-1], 
                  col = LCol), dots))
  }
  
  else if (! add) {
    startplot()
    if (isCI) {
      plot.CI.2d(CI, x, y, Col)   
      isCI <- FALSE
    }
    do.call("points", c(alist(x, y, col = Col), dots))
  }
  else  {
    if (isCI) {
      plot.CI.2d(CI, x, y, Col) 
      isCI <- FALSE
    }
    do.call("points", c(alist(x, y, col = Col), dots))
  }
    
  if (iscolkey) {
    drawcolkey(colkey, col, clim, clab, clog) 
    if (! add)       
      par(plt = par.ori)  
    par(mar = par("mar"))
  }    

}
## =============================================================================
## Confidence interval check for scatters (2D and 3D)
## =============================================================================

check.CI <- function(CI, len, dim) {
  
    if (dim == 2 & !is.null(CI$z))
      stop("'CI' should not contain confidence intervals in the 'z' direction for 2-D plot")

    if (dim == 2 & is.null(CI$x) & is.null(CI$y))
      stop("'CI' should contain confidence intervals the 'x' and/or 'y' direction")
    
    if (dim == 3 & (is.null(CI$x) & is.null(CI$y) & is.null(CI$z)))
        stop("'CI' should contain confidence intervals in 'x', 'y', or 'z' direction")

    if (!is.null(CI$x)) {
      if (! is.matrix(CI$x))
        stop("'CI$x' should be a matrix")
      if (ncol(CI$x) != 2)
        stop("'CI$x' should be a matrix with two columns, with lower and upper value")
      if (nrow(CI$x) != len)
        stop("number of rows of matrix 'CI$x' should be equal to number of points")
    }      
    if (!is.null(CI$y)) {
      if (! is.matrix(CI$y))
        stop("'CI$y' should be a matrix")
      if (ncol(CI$y) != 2)
        stop("'CI$y' should be a matrix with two columns, with lower and upper value")
      if (nrow(CI$y) != len)
        stop("number of rows of matrix 'CI$y' should be equal to number of points")
    }      
    if (!is.null(CI$z)) {
      if (! is.matrix(CI$z))
        stop("'CI$z' should be a matrix")
      if (ncol(CI$z) != 2)
        stop("'CI$z' should be a matrix with two columns, with lower and upper value")
      if (nrow(CI$z) != len)
        stop("number of rows of matrix 'CI$z' should be equal to number of points")
    }      
  parameter <- list(alen = 0.01, lty = par("lty"), lwd = par("lwd"), col = NULL)
  CIpar <- CI; CIpar$x <- CIpar$y <- CIpar$z <- NULL                           
  CIpar <- overrulepar(parameter, CIpar)
  CIpar$x <- CI$x; CIpar$y <- CI$y; CIpar$z <- CI$z
  CIpar
}


## =============================================================================
## CI in 2-d
## =============================================================================

plot.CI.2d <- function(CI, x, y, Col) {         # very-very simple

  CIpar <- CI[c("lty", "lwd", "col")]

  if (is.null(CIpar$col))
    CIpar$col <- Col
  
  if (! is.null(CI$x)) { # CI in x direction
     len <-  par("fin")[1] * CI$alen
     do.call("arrows", c(alist(x, y, x-CI$x[, 1], y, angle = 90, length = len), CIpar))
     do.call("arrows", c(alist(x, y, x+CI$x[, 2], y, angle = 90, length = len), CIpar))
  }  
  if (! is.null(CI$y)) { 
     len <-  par("fin")[2] * CI$alen
     do.call("arrows", c(alist(x, y, x, y-CI$y[, 1], angle = 90, length = len), CIpar))
     do.call("arrows", c(alist(x, y, x, y+CI$y[, 2], angle = 90, length = len), CIpar))
  }  
}

