
## =============================================================================
## =============================================================================
## Color key functions
## =============================================================================
## =============================================================================

## =============================================================================
## Is it necessary to draw a color key?
## =============================================================================

is.colkey <- function(colkey, col) {
  
  if (is.logical(colkey))
    return(colkey)
    
  iscolkey <- ! is.null(colkey)
 
  iscol <- ispresent(col) 
  
  if (iscol) {
    if ( length(col) == 1) 
      iscol <- FALSE
    else if (length(col) == 2 & col[1] == col[2]) 
      iscol <- FALSE
  }
  
  if (!iscol) 
    return(FALSE)
 
  else if (length(colkey) == 0) 
    iscolkey <- FALSE
  else if (! is.logical(colkey)) 
    iscolkey <- TRUE
  else if (colkey[[1]][1] == FALSE) 
    iscolkey <- FALSE
  
  return(iscolkey)
}

## =============================================================================
## function to extract default parameter values if not overruled
## =============================================================================

overrulepar <- function(main, subset) {
  nmsC <- names(main)
  main[(namc <- names(subset))] <- subset
  if (length(noNms <- namc[!namc %in% nmsC]) > 0) 
     warning("unknown names in colkey parameter subset: ", paste(noNms, 
            collapse = ", "))
  return(main)
}

## =============================================================================
## color key parameter check
## =============================================================================

check.colkey <- function(colkeypar, add = FALSE) {    
   
  if (!is.list(colkeypar))
    colkeypar <- list()
      
  parameter <- list(side = 4, 
      length = 1, width = 1, dist = 0, shift = 0,
      col.clab = NULL, cex.clab = par("cex.axis"),
      at = NULL, labels = TRUE, tick = TRUE, line = NA, 
      pos = NA, outer = FALSE, font = NA, lty = 1, lwd = 1, 
      lwd.ticks = 1, col.box = NULL, col.axis = NULL, 
      col.ticks = NULL, hadj = NA, padj = NA, 
      cex.axis = par("cex.axis"), mgp = NULL, tck = NULL, tcl = NULL, las = NULL)
                             
  colkeypar$parleg <- colkeypar$parplt <- NULL
  colkey <- overrulepar(parameter, colkeypar)
  if (is.numeric(colkey$labels))
    colkey$labels <- as.logical(colkey$labels)
  if (is.null(colkey$side)) 
    colkey$side <- 4

 # plt parameters of legend
  colkey <- key.parleg(colkey, add)
  return(colkey)
}

## =============================================================================
   
key.parleg <- function(colkey, add) {   # the plotting parameters                                                       

  dw     <- 0.03*colkey$width
  rp     <- colkey$shift
   
  parplt <- par("plt")
    
  if (colkey$side == 1) {
    if (! add) 
      parplt <- par("plt") + c(0, 0, 0.15, 0)
    dd     <- 0.15 + colkey$dist
    dp     <- (parplt[2] - parplt[1]) * (1-colkey$length)/2 
    parleg <- c( parplt[1] + dp+rp, parplt[2] - dp+rp, 
                 parplt[3] - dd, parplt[3] - dd + dw)
  
  } else if (colkey$side == 2) {
    if (! add) 
      parplt <- par("plt") + c(0.1, 0, 0, 0)
    dd <- 0.1  + colkey$dist
    dp <- (parplt[4] - parplt[3]) * (1-colkey$length)/2 
    parleg <- c( parplt[1] - dd, parplt[1] - dd + dw, 
                 parplt[3] + dp+rp, parplt[4] - dp+rp)
  
  } else if (colkey$side == 3) {
    if (! add) 
      parplt <- par("plt") - c(0, 0, 0, 0.08)
    dd <- 0.02 + colkey$dist
    dp <- (parplt[2] - parplt[1]) * (1-colkey$length)/2 
    parleg <- c( parplt[1] + dp+rp,  parplt[2] - dp+rp, 
                 parplt[4] + dd, parplt[4] + dd + dw)
  
  } else if (colkey$side == 4) {
    if (! add) 
      parplt <- par("plt") - c(0, 0.08, 0, 0)
    dd <- 0.02 + colkey$dist
    dp <- (parplt[4] - parplt[3]) * (1-colkey$length)/2
    parleg <- c(parplt[2] + dd, parplt[2] + dd + dw, 
                parplt[3] + dp+rp, parplt[4] - dp+rp)
  }
   
  colkey$parleg <- check.plt(parleg)
  colkey$parplt <- check.plt(parplt)
  return(colkey)
}

## =============================================================================
## function to save the color key settings
## =============================================================================

plistcolkey <- function (plist, colkeypar, col, zlim, zlab = NULL, 
                         zlog = FALSE, New = TRUE)  {
  if (is.null(plist$colkey))  {
    plist$colkey <- list()
    plist$numkeys <- 1
  } else
    plist$numkeys <- plist$numkeys + 1

  plist$colkey[[plist$numkeys]] <- list(par = colkeypar, col = col, 
    zlim = zlim, zlab = zlab, zlog = zlog, New = New)
  plist
} 
                              
## =============================================================================
## functions to draw the color key
## =============================================================================

drawallcols <- function(plist) {
  for (colkey in plist$colkey) 
    drawcolkey(colkey$par, colkey$col, colkey$zlim, colkey$zlab, 
                        colkey$zlog, colkey$New)
}     

## =============================================================================

drawcolkey <- function (colkeypar, col, zlim, zlab = NULL, 
                        zlog = FALSE, New = TRUE) {     

  parleg <- check.plt(colkeypar$parleg)
  Plt <- par(plt = parleg)   
  PP  <- par()
  if (New) 
    par(new = TRUE)
  
  usr <- par("usr")
  col.zlab <- colkeypar$col.clab
  cex.zlab <- colkeypar$cex.clab
  
  ix <- 1
  minz <- zlim[1]
  maxz <- zlim[2]
  binwidth <- (maxz - minz)/64
  iy <- IY <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
  if (zlog) {
    iy <- exp(iy)
    if (colkeypar$side %in% c(2, 4))  {
      Log <- "y"
    } else {
      Log <- "x"
    }  
  } else Log <- ""
  iz <- matrix(IY, nrow = 1, ncol = length(iy))
  
  if (! is.numeric(cex.zlab)) 
    cex.zlab <- 1.
  
  # the parameters for the axis
  axispar <- colkeypar
  
  # remove arguments not in axis function
  axispar$side <- NULL; axispar$length <- NULL ; axispar$width <- NULL
  axispar$parleg <- NULL; axispar$parplt <- NULL; axispar$dist <- NULL
  axispar$shift <- NULL; axispar$col.box <- NULL  
  axispar$col.clab <- NULL; axispar$cex.clab <- NULL

  if (colkeypar$side %in% c(2, 4)) {
  
    image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", log = Log,
        ylab = "", col = col, main = zlab, cex.main = cex.zlab, 
        col.main = col.zlab)
    do.call("axis", c(list(side = colkeypar$side, mgp = c(3, 1, 0), las = 2), 
            axispar))
                                                    
  } else {
  
    image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", log = Log,
        ylab = "", col = col, main = "")
    mtext(side = colkeypar$side, text = zlab, line = 2, cex = cex.zlab, 
        col = col.zlab)
    do.call("axis", c(list(side = colkeypar$side, mgp = c(3, 1, 0), las = 1), 
         axispar))
  }
  
  box(col = colkeypar$col.box) 

  par(plt = Plt)
  par(usr = usr)
  par(xlog = PP$xlog)
  par(ylog = PP$ylog)
  
  if (New) 
    par(new = FALSE)  
}

## =============================================================================
## =============================================================================
## R-function to draw a color key
## =============================================================================
## =============================================================================


colkey <- function(col = NULL, clim, clab = NULL, clog = FALSE, add = FALSE, 
                   cex.clab = NULL, col.clab = NULL, 
                   side = 4, length = 1, width = 1, 
                   dist = 0, shift = 0, at = NULL, labels = TRUE, 
                   tick = TRUE, line = NA, pos = NA, outer = FALSE,  
                   font = NA, lty = 1, lwd = 1, lwd.ticks = 1, 
                   col.axis = NULL, col.ticks = NULL, col.box = NULL,
                   hadj = NA, padj = NA, cex.axis = par("cex.axis"),
                   mgp = NULL, tck = NULL, tcl = NULL, las = NULL) {

  if (is.null(col)) 
    col <- jet.col(100)
  
  colkey <- list(side = side, length = length, width = width, 
      dist = dist, shift = shift, cex.clab = cex.clab, col.clab = col.clab,
      at = at, labels = labels, tick = tick, line = line, 
      pos = pos, outer = outer, font = font, lty = lty, lwd = lwd, 
      lwd.ticks = lwd.ticks, col.box = col.box, col.axis = col.axis, 
      col.ticks = col.ticks, hadj = hadj, 
      padj = padj, cex.axis = cex.axis,
      mgp = mgp, tck = tck, tcl = tcl, las =las)

  if (is.numeric(colkey$labels))
    colkey$labels <- as.logical(colkey$labels)
  if (is.null(colkey$side)) colkey$side <- 4
                                                          
  dw     <- 0.03*colkey$width

  parplt <- par("plt") 
  
  if (! add & colkey$side %in% c(1, 3)) {
      py <- 0.5*(parplt[3] + parplt[4])
      dp     <- (parplt[2] - parplt[1]) * (1-colkey$length)/2
      colkey$parleg <- c( parplt[1]+dp, parplt[2]-dp, py - dw/2, py+dw/2)

  } else if (! add & colkey$side %in% c(2, 4)) {
      px <- 0.5*(parplt[1] + parplt[2])
      dp <- (parplt[4] - parplt[3])*(1-colkey$length)/2
      colkey$parleg <- c( px - dw/2, px +dw/2, parplt[3]+dp, parplt[4]-dp)

  } else colkey <- key.parleg(colkey, add = TRUE)
  
  colkey$parplt <- parplt
  
  if (clog) 
    clim <- log(clim) 
  
  drawcolkey (colkey, col, zlim = clim, zlab = clab, 
                        zlog = clog,New = add)

  par(mar = par("mar")) # TO PREVENT R FROM SETTING DEFAULTPLOT = FALSE

}

## =============================================================================
## checks the validity of the plotting arguments "plt"
## =============================================================================

check.plt <- function(plt) {
  if (!(plt[1] < plt[2] & plt[3] < plt[4]))
    stop("figure margins too large")
  eps <- 1e-10
  if (!(plt[1] > -eps & plt[2] < 1+eps & plt[3] > -eps & plt[4] < 1+eps))
    stop("plot region too large")
  return(plt)  
}
