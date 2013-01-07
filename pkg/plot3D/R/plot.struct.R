## =============================================================================
## Plotting the information in the plotting list
## =============================================================================

plotdev <- function(...)  {
  x <- plot.plist(getplist(), ...)
  invisible(x)    
}

## =============================================================================

plot.plist <- function(x, ...)  {
  if (length(x) == 0)
    stop("nothing to draw")
  dot <- list(...)

  if (length(dot) != 0) {
    projectnames <- c("theta", "phi", "xlim", "ylim", "zlim", 
                      "scale", "expand", "r", "d")
    if (sum( projectnames %in% names(dot)) > 0)
      x <- proj3D (x, dot[projectnames])
  
    shadenames <- c("ltheta", "lphi", "shade", "lighting")
    if (sum( shadenames %in% names(dot)) > 0)
      if (! is.null(x$poly))
        x$poly <- color3D(x$poly, x$scalefac, dot[shadenames], dot$lighting)
  
    if ("alpha" %in% names(dot))
      x <- alpha3D(x, dot[["alpha"]])
  }     
  
  x$persp$box <- FALSE
  x <- plot.struct.3D (x, plot = TRUE)
  invisible(x)  
}

## =============================================================================

plot.struct.3D <- function(plist, pt = NULL, CIpt = NULL, poly = NULL, 
  segm = NULL, labels = NULL, arr = NULL, quiv = NULL, plot = TRUE) {       

  if (plot) {
    if (is.null(plist$plt)) 
      plist$plt$ori   <- par("plt")

    if (!is.null(plist)) 
      par(plt = plist$plt$main)

    if (!plist$persp$box) 
      plist <- plotbox(plist)
  }
  
  plist <- update.3D(plist, pt, CIpt, poly, segm, labels, arr, quiv)

  if (plot) {
    plotlist3D(plist)
    if (plist$persp$bty == "f")
      drawfullbox(plist)
    if (! is.null(plist$colkey))
      drawallcols(plist)

    par(plt = plist$plt$ori)
    par(mar = par("mar"))
  }
  invisible(plist)  
}

## =============================================================================

alpha3D <- function(plist, alpha) { # makes colors transparant

  if (alpha == 1 | alpha == 0) 
    return (plist)
  
  setalpha <- function(Col) {
    if (is.null(Col))
      return(Col)
    ii <- which (! is.na(Col))
    if (length(ii) > 0) {   
      pcol <- alpha.col (Col, alpha)
      Col[ii] <- pcol[ii] 
    }    
    Col
  }

  plist$poly$col       <- setalpha(plist$poly$col)
  plist$poly$border    <- setalpha(plist$poly$border)
  plist$pt$col         <- setalpha(plist$pt$col)
  plist$pt$bg          <- setalpha(plist$pt$bg)
  plist$CIpt$col       <- setalpha(plist$CIpt$col)
  plist$CIpt$bg        <- setalpha(plist$CIpt$bg)
  plist$CIpt$CIpar$col <- setalpha(plist$CIpt$CIpar$col)
  plist$labels$col     <- setalpha(plist$labels$col)
  plist$segm$col       <- setalpha(plist$segm$col)
  plist$arr$col        <- setalpha(plist$arr$col)
  
  if (! is.null(plist$numkeys))
    for (i in 1:plist$numkeys)
      plist$colkey[[i]]$col  <- setalpha(plist$colkey[[i]]$col)
      
  return(plist)
}

## =============================================================================

color3D <- function(poly, scalefac, shade, lighting) {  # lighting and shading

  shade$lighting <- NULL
  
  shade <- check.shade(shade, lighting)

  light   <- setuplight(shade$lphi, shade$ltheta) [1:3]              
  px <- poly$x * scalefac$x
  py <- poly$y * scalefac$y
  pz <- poly$z * scalefac$z
  Normals <- normal.points(rbind(px[1,], py[1,], pz[1,]) , 
                           rbind(px[2,], py[2,], pz[2,]) , 
                           rbind(px[3,], py[3,], pz[3,]) )
    
  ii <- which (! is.na(poly$col) & poly$col != "transparent" )
  if (length(ii) > 0) {   
    if (shade$type == "shade") 
      pcol <- facetcols.shade(light, Normals, poly$col, shade$shade)
    else if (shade$type == "light") 
      pcol <-  facetcols.light(light, Normals, poly$col, shade)
    poly$col[ii] <- pcol[ii] 
  }    

  ii <- which (! is.na(poly$border))
  if (length(ii) > 0) {   
    if (shade$type == "shade") 
      pcol <- facetcols.shade(light, Normals, poly$border, shade$shade)
    else if (shade$type == "light") 
      pcol <-  facetcols.light(light, Normals, poly$border, shade)
    poly$border[ii] <- pcol[ii] 
  }    
  poly
}

## =============================================================================

proj3D <- function(plist, dot) {

  if (!is.null(dot$theta))
    plist$persp$theta <- dot$theta

  if (!is.null(dot$phi))
    plist$persp$phi <- dot$phi

  if (!is.null(dot$xlim))
    plist$xlim <- dot$xlim

  if (!is.null(dot$ylim))
    plist$ylim <- dot$ylim

  if (!is.null(dot$zlim))
    plist$zlim <- dot$zlim

  if (!is.null(dot$scale))
    plist$dot$scale <- dot$scale

  if (!is.null(dot$expand))
    plist$dot$expand <- dot$expand

  lim <- setlim (plist$xlim, plist$ylim, plist$zlim, 
    plist$dot$scale, plist$dot$expand) 

  plist$scalefac <- lim

  if (!is.null(dot$r))
    plist$dot$r <- dot$r

  if (!is.null(dot$d))
    plist$dot$d <- dot$d

  plist$mat <- transmat (plist$persp$phi, plist$persp$theta, plist$scalefac, 
    plist$dot$r, plist$dot$d)

 # update projections
  if (!is.null(plist$pt)) 
    plist$pt$proj <- project(plist$pt$x.mid,  plist$pt$y.mid,  plist$pt$z.mid,  plist)

  if (!is.null(plist$CIpt))
    plist$CIpt$proj <- project(plist$CIpt$x.mid,  plist$CIpt$y.mid,  plist$CIpt$z.mid,  plist)

  if (!is.null(plist$labels))
    plist$labels$proj <- project(plist$labels$x, plist$labels$y, plist$labels$z, plist)

  if (!is.null(plist$poly))
    plist$poly$proj <- project(colMeans(plist$poly$x, na.rm = TRUE),
                               colMeans(plist$poly$y, na.rm = TRUE), 
                               colMeans(plist$poly$z, na.rm = TRUE), plist)

  if (!is.null(plist$segm)) 
    plist$segm$proj <- project(0.5*(plist$segm$x.from + plist$segm$x.to), 
                               0.5*(plist$segm$y.from + plist$segm$y.to), 
                               0.5*(plist$segm$z.from + plist$segm$z.to), plist)

  if (!is.null(plist$arr))
    plist$arr$proj <- project(0.5*(plist$arr$x.from + plist$arr$x.to), 
                              0.5*(plist$arr$y.from + plist$arr$y.to), 
                              0.5*(plist$arr$z.from + plist$arr$z.to), plist)
  return(plist)
} 

## =============================================================================
## updates plist
## =============================================================================

update.3D <- function(plist, pt = NULL, CIpt = NULL, poly = NULL, 
       segm = NULL, labels = NULL, arr = NULL, quiv = NULL) {       
# ------------------------------------------------------------------------------
# update plist with new elements     
# ------------------------------------------------------------------------------

  if (is.null(plist$pt))
    plist$pt <- pt

  else if (! is.null(pt)) {
    plist$pt$x.mid  <- c(plist$pt$x.mid,  pt$x.mid)
    plist$pt$y.mid  <- c(plist$pt$y.mid,  pt$y.mid)
    plist$pt$z.mid  <- c(plist$pt$z.mid,  pt$z.mid)
    plist$pt$col    <- c(plist$pt$col,    pt$col)
    plist$pt$pch    <- c(plist$pt$pch,    pt$pch)   
    plist$pt$bg     <- c(plist$pt$bg,     pt$bg)
    plist$pt$cex    <- c(plist$pt$cex,    pt$cex)   
    plist$pt$proj   <- c(plist$pt$proj,   pt$proj)
  }

  if (is.null(plist$CIpt))
    plist$CIpt <- CIpt
  else if (! is.null(CIpt)) {
    plist$CIpt$x.to   <- rbind(plist$CIpt$x.to,  CIpt$x.to)
    plist$CIpt$y.to   <- rbind(plist$CIpt$y.to,  CIpt$y.to)
    plist$CIpt$z.to   <- rbind(plist$CIpt$z.to,  CIpt$z.to)
    plist$CIpt$x.from <- rbind(plist$CIpt$x.from,CIpt$x.from)
    plist$CIpt$y.from <- rbind(plist$CIpt$y.from,CIpt$y.from)
    plist$CIpt$z.from <- rbind(plist$CIpt$z.from,CIpt$z.from)
    plist$CIpt$nCI    <- c(plist$CIpt$nCI,   CIpt$nCI)
    plist$CIpt$x.mid  <- c(plist$CIpt$x.mid, CIpt$x.mid)
    plist$CIpt$y.mid  <- c(plist$CIpt$y.mid, CIpt$y.mid)
    plist$CIpt$z.mid  <- c(plist$CIpt$z.mid, CIpt$z.mid)
    plist$CIpt$len    <- c(plist$CIpt$len,   CIpt$len)
    plist$CIpt$lty    <- c(plist$CIpt$lty,   CIpt$lty)
    plist$CIpt$lwd    <- c(plist$CIpt$lwd,   CIpt$lwd)
    plist$CIpt$col    <- c(plist$CIpt$col,   CIpt$col)
    plist$CIpt$pch    <- c(plist$CIpt$pch,   CIpt$pch)   
    plist$CIpt$bg     <- c(plist$CIpt$bg,    CIpt$bg)
    plist$CIpt$cex    <- c(plist$CIpt$cex,   CIpt$cex)   
    plist$CIpt$proj   <- c(plist$CIpt$proj,  CIpt$proj)

    plist$CIpt$CIpar$col  <- c(plist$CIpt$CIpar$col, CIpt$CIpar$col)
    plist$CIpt$CIpar$lwd  <- c(plist$CIpt$CIpar$lwd, CIpt$CIpar$lwd)
    plist$CIpt$CIpar$lty  <- c(plist$CIpt$CIpar$lty, CIpt$CIpar$lty)
    plist$CIpt$CIpar$alen <- c(plist$CIpt$CIpar$alen, CIpt$CIpar$alen)
  }

  if (is.null(plist$labels))
    plist$labels <- labels
  else if (! is.null(labels)) {
    plist$labels$x      <- c(plist$labels$x,  labels$x)
    plist$labels$y      <- c(plist$labels$y,  labels$y)
    plist$labels$z      <- c(plist$labels$z,  labels$z)
    plist$labels$labels <- c(plist$labels$labels, labels$labels)
    plist$labels$adj    <- c(plist$labels$adj,  labels$adj)
    plist$labels$cex    <- c(plist$labels$cex,  labels$cex)
    plist$labels$col    <- c(plist$labels$col,  labels$col)
    plist$labels$font   <- c(plist$labels$font, labels$font)
    plist$labels$proj   <- c(plist$labels$proj, labels$proj)
  }
    
  if (is.null(plist$poly))
    plist$poly <- poly      
  else if (! is.null(poly)) {
    nR1 <- nrow(poly$x) 
    nR2 <- nrow(plist$poly$x)
    if (nR1 > nR2) {
      nR <- matrix(nrow = nR1 - nR2, ncol = ncol(plist$poly$x), data = NA)
      plist$poly$x <- rbind(plist$poly$x, nR)
      plist$poly$y <- rbind(plist$poly$y, nR)
      plist$poly$z <- rbind(plist$poly$z, nR)
    } else if (nR2 > nR1) {
      nR <- matrix(nrow = nR2 - nR1, ncol = ncol(poly$x), data = NA)
      poly$x <- rbind(poly$x, nR)
      poly$y <- rbind(poly$y, nR)
      poly$z <- rbind(poly$z, nR)
    }
    plist$poly$x      <- cbind(plist$poly$x,  poly$x)
    plist$poly$y      <- cbind(plist$poly$y,  poly$y)
    plist$poly$z      <- cbind(plist$poly$z,  poly$z)
    plist$poly$proj   <- c(plist$poly$proj,   poly$proj)
    plist$poly$lwd    <- c(plist$poly$lwd,    poly$lwd)
    plist$poly$lty    <- c(plist$poly$lty,    poly$lty)
    plist$poly$border <- c(plist$poly$border, poly$border)
    plist$poly$col    <- c(plist$poly$col,    poly$col)
  }
  
  if (is.null(plist$segm))
    plist$segm <- segm
  else if (! is.null(segm)) {
    plist$segm$x.from <- c(plist$segm$x.from, segm$x.from)
    plist$segm$y.from <- c(plist$segm$y.from, segm$y.from)
    plist$segm$z.from <- c(plist$segm$z.from, segm$z.from)
    plist$segm$x.to   <- c(plist$segm$x.to,   segm$x.to)
    plist$segm$y.to   <- c(plist$segm$y.to,   segm$y.to)
    plist$segm$z.to   <- c(plist$segm$z.to,   segm$z.to)
    plist$segm$proj   <- c(plist$segm$proj,   segm$proj)
    plist$segm$lwd    <- c(plist$segm$lwd,    segm$lwd)
    plist$segm$lty    <- c(plist$segm$lty,    segm$lty)
    plist$segm$col    <- c(plist$segm$col,    segm$col)
  }

  if (is.null(plist$arr))
    plist$arr <- arr
  else if (! is.null(arr)) {
    plist$arr$x.from <- c(plist$arr$x.from, arr$x.from)
    plist$arr$y.from <- c(plist$arr$y.from, arr$y.from)
    plist$arr$z.from <- c(plist$arr$z.from, arr$z.from)
    plist$arr$x.to   <- c(plist$arr$x.to,   arr$x.to)
    plist$arr$y.to   <- c(plist$arr$y.to,   arr$y.to)
    plist$arr$z.to   <- c(plist$arr$z.to,   arr$z.to)
    plist$arr$proj   <- c(plist$arr$proj,   arr$proj)
    plist$arr$code   <- c(plist$arr$code,   arr$code)
    plist$arr$angle  <- c(plist$arr$angle,  arr$angle)
    plist$arr$length <- c(plist$arr$length, arr$length)
    plist$arr$lwd    <- c(plist$arr$lwd,    arr$lwd)
    plist$arr$lty    <- c(plist$arr$lty,    arr$lty)
    plist$arr$col    <- c(plist$arr$col,    arr$col)
  }                                                                     
  return (plist)
}

## =============================================================================
## plots points, polygons, segments, labels, arrows
## =============================================================================

plotlist3D <- function(plist) {       

    pt     <- plist$pt
    CIpt   <- plist$CIpt    
    poly   <- plist$poly
    segm   <- plist$segm
    labels <- plist$labels
    arr    <- plist$arr
    quiv   <- plist$quiv

# ------------------------------------------------------------------------------
# project (x, y, z) to plane (x, y) and count number of different structures
# ------------------------------------------------------------------------------
    numStruct <- 0  
  
    if (! is.null(poly)) {     
      pol <- trans3D(x = poly$x, y = poly$y, z = poly$z, pmat = plist$mat)
      numStruct <- numStruct + 1          
    }
    if (! is.null(quiv))  {    
      quiv.to   <- trans3D(x = quiv$x.to,   y = quiv$y.to,   z = quiv$z.to,   pmat = plist$mat)
      quiv.from <- trans3D(x = quiv$x.from, y = quiv$y.from, z = quiv$z.from, pmat = plist$mat)
      numStruct <- numStruct + 1
    }
    if (! is.null(CIpt$x.from)) {   
      CI.to   <- trans3D(x = CIpt$x.to,   y = CIpt$y.to,   z = CIpt$z.to,   pmat = plist$mat)
      CI.from <- trans3D(x = CIpt$x.from, y = CIpt$y.from, z = CIpt$z.from, pmat = plist$mat)
      CI.mid  <- trans3D(x = CIpt$x.mid , y = CIpt$y.mid , z = CIpt$z.mid , pmat = plist$mat)
      numStruct <- numStruct + 1
    }
    if (! is.null(pt)) {
      pt.mid  <- trans3D(x = pt$x.mid , y = pt$y.mid , z = pt$z.mid , pmat = plist$mat)
      numStruct <- numStruct + 1
    }
    if (! is.null(segm)) {   
      segm.to   <- trans3D(x = segm$x.to,   y = segm$y.to,   z = segm$z.to,   pmat = plist$mat)
      segm.from <- trans3D(x = segm$x.from, y = segm$y.from, z = segm$z.from, pmat = plist$mat)
      numStruct <- numStruct + 1
    }
    if (! is.null(arr)) {   
      arr.to   <- trans3D(x = arr$x.to,   y = arr$y.to,   z = arr$z.to,   pmat = plist$mat)
      arr.from <- trans3D(x = arr$x.from, y = arr$y.from, z = arr$z.from, pmat = plist$mat)
      numStruct <- numStruct + 1
    }
    if (! is.null(labels)) {   
      lab <- trans3D(x = labels$x, y = labels$y, z = labels$z, pmat = plist$mat)
      numStruct <- numStruct + 1
    }    

    sortlist <- sort.int(c(pt$proj, CIpt$proj, poly$proj, segm$proj, 
                           labels$proj, arr$proj, quiv$proj), 
                         index.return = TRUE)$ix

# ------------------------------------------------------------------------------
# check if there is only one types and plot if true
# ------------------------------------------------------------------------------
    if (numStruct == 1) {

      if (! is.null(pt)) {  
        points(pt.mid$x[sortlist], pt.mid$y[sortlist], 
               col = pt$col[sortlist], 
               pch = pt$pch[sortlist],
               cex = pt$cex[sortlist], 
               bg  = pt$bg[sortlist])

      } else if (!is.null(poly)) {  # only polygons
        polygon(pol$x[ ,sortlist], pol$y[ ,sortlist], 
                lwd = poly$lwd[sortlist],
                lty = poly$lty[sortlist], 
                border = poly$border[sortlist], 
                col = poly$col [sortlist])

      } else if (!is.null(segm)) {  # only segments
        segments(segm.from$x[sortlist], segm.from$y[sortlist],  
                 segm.to$x  [sortlist], segm.to$y[sortlist], 
                 col = segm$col[sortlist],
                 lwd = segm$lwd[sortlist],
                 lty = segm$lty[sortlist])

      } else if (!is.null(labels)) {    # only labels
        text(x = lab$x[sortlist], y = lab$y[sortlist], 
             labels = labels$labels[sortlist], 
             col = labels$col[sortlist], 
             adj = labels$adj[sortlist], 
             cex = labels$cex[sortlist],
             font = labels$font[sortlist] )

      } else if (!is.null(arr)) {  # only segments
        arrows(arr.from$x[sortlist], arr.from$y[sortlist],  
               arr.to$x[sortlist], arr.to$y[sortlist], 
               length = arr$length[sortlist],
               angle = arr$angle[sortlist],
               code = arr$code[sortlist],
               col = arr$col[sortlist],
               lwd = arr$lwd[sortlist],
               lty = arr$lty[sortlist])

      } else if (!is.null(quiv)) { # only quivers - not in plot3D package

          do.call("Arrows", c(alist(quiv.from$x, quiv.from$y, quiv.to$x, quiv.to$y, 
             col = quiv$col),  quiv$dot))

      }
      if (is.null(CIpt))
        return()    
    }
 
   # it is a mix of types  
    Lpt     <- length(pt$proj)
    LCIpt   <- length(CIpt$proj)
    Lpoly   <- length(poly$proj)
    Lsegm   <- length(segm$proj)
    Llabels <- length(labels$proj)
    Larr    <- length(arr$proj)
    Lquiv   <- length(quiv$proj)
    LCI     <- Lpt + LCIpt
    LCP     <- LCI + Lpoly
    LCPS    <- LCP + Lsegm
    LCPSL   <- LCPS + Llabels
    LCPSLA  <- LCPSL + Larr
    Ltot    <- LCPSLA + Lquiv

    type <- rep(0, Ltot)                                   # 0 = points
    type[sortlist > Lpt   & sortlist <= LCI]    <- 1       # points + CI
    type[sortlist > LCI   & sortlist <= LCP]    <- 2       # polygons
    type[sortlist > LCP   & sortlist <= LCPS]   <- 3       # segments
    type[sortlist > LCPS  & sortlist <= LCPSL]  <- 4       # labels
    type[sortlist > LCPSL & sortlist <= LCPSLA] <- 5       # arrows
    type[sortlist > LCPSLA                    ] <- 6       # quivers

    plotit <- function(ii) {
      i <- sortlist[ii]  
    
      if (type[ii] == 0) { # points   
        points(pt.mid$x[i], pt.mid$y[i], 
               col = pt$col[i], pch = pt$pch[i],
               cex = pt$cex[i], bg = pt$bg[i])
   
     } else if (type[ii] == 1) { # points + CI   
        io  <- i - Lpt
        nCI <- CIpt$nCI[io]
        for(j in 1:nCI)  
          arrows(CI.from$x[io, j], CI.from$y[io, j], 
                 CI.to$x[io, j], CI.to$y[io, j], 
                 angle = 90, length = CIpt$CIpar$alen[io], code = 3, 
                 col = CIpt$CIpar$col[io], 
                 lty = CIpt$CIpar$lty[io], 
                 lwd = CIpt$CIpar$lwd[io])

        if (CIpt$dopoints) 
          points(CI.mid$x[io], CI.mid$y[io], 
                 col = CIpt$col[io], pch = CIpt$pch[io],
                 cex = CIpt$cex[io], bg = CIpt$bg[io])
                                                                
    } else if (type[ii] == 2) {
       io <- i - LCI 
       polygon(pol$x[, io], pol$y[, io], 
               lwd = poly$lwd[io],
               lty = poly$lty[io], 
               border = poly$border[io], 
               col = poly$col[io])

    } else if (type[ii] == 3) {
       io <- i - LCP
       segments(segm.from$x[io], segm.from$y[io],  
                segm.to$x[io], segm.to$y[io], 
                col = segm$col[io], 
                lwd = segm$lwd[io], 
                lty = segm$lty[io])

    } else if (type[ii] == 4) {
       io <- i - LCPS
       text(x = lab$x[io], y = lab$y[io], 
            labels = labels$labels[io], 
            col = labels$col[io],
            adj = labels$adj[io], 
            cex = labels$cex[io],
            font = labels$font[io])

    } else if (type[ii] == 5) {
       io <- i - LCPSL
       arrows(arr.from$x[io], arr.from$y[io],  
              arr.to$x[io], arr.to$y[io], 
              length = arr$length[io], 
              angle = arr$angle[io], 
              code = arr$code[io], 
              col = arr$col[io], 
              lwd = arr$lwd[io], 
              lty = arr$lty[io])

   } else if (type[ii] == 6) {
      io <- i - LCPSLA
      dp <- extractdots(quiv$dot, i)
      do.call("Arrows", c(alist(quiv$x.from[io], quiv$y.from[io], 
         quiv$x.to[io], quiv$y.to[io], col = quiv$col[io]), dp))
      }
    
    }
  
  sapply(FUN = plotit, 1:length(sortlist))
#    for (i in 1:length(sortlist)) plotit(i)

}