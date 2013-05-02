## =============================================================================
## plots (2-D)
## =============================================================================

# x, y, colvar: vector or matrix of same dimension

plot2D <- function(x0, y0, x1, y1, ..., colvar = NULL,
                    col = NULL, NAcol = "white",
                    colkey = list(side = 4),
                    clim = NULL, clab = NULL, add = FALSE,
                    method = "arrows") {

  dots <- splitpardots(list(...))

  clog <- FALSE
  if (! is.null(dots$main$log)) {
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
    drawcolkey(colkey, col, clim, clab, clog)
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

