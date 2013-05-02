cutrgl <- function(...) {
  SS <- select3d()
  plist <- selectplist(getplist(), SS)
  rgl.clear()
  dots <- list(...)
  light <- dots$lighting
  if (is.null(light))
    light <- getplist()$rgl$lighting
  smooth <- dots$smooth
  if (is.null(smooth))
    smooth <- getplist()$rgl$smooth
  dots$lighting <- dots$new <- dots$smooth <- NULL
  do.call ("plotrglplist", c(alist(plist, lighting = light, new = FALSE, 
    update = FALSE, scale = FALSE, smooth = smooth), dots))

  if (plist$type == "2D") 
    aspect3d(1,1,1)

  decorate3d(axes = FALSE, box = TRUE, zlab = "")

 # zoom and centering restored
  par3d(zoom = 1)
  uM <- par3d("userMatrix")
  uM[ ,4] <- c(0,0,0,1)
  par3d(userMatrix = uM)

  if (plist$type == "2D") {
    axis3d("x")
    axis3d("y") 
  }
}

uncutrgl <- function(...) {
  rgl.clear()
  dots <- list(...)
  light <- dots$lighting
  if (is.null(light))
    light <- getplist()$rgl$lighting
  if (is.null(light)) 
    light <- FALSE
  smooth <- dots$smooth
  if (is.null(smooth))
    smooth <- getplist()$rgl$smooth
  dots$lighting <- dots$new <- dots$smooth <- NULL

  do.call ("plotrgl", c(alist(lighting = light, new = FALSE, smooth = smooth), dots))
  par3d(zoom = 1)

  plist <- getplist()
#  par3d(userMatrix = plist$rgl$userMatrix)
  if (! is.null(plist$type))
    if (getplist()$type == "2D") {
      axis3d("x")
      axis3d("y") 
    }
}



selectplist <- function (plist, SS) {
 #
 
  if (! is.null(plist$pt)) {
    pt  <- plist$pt                  
    ipt <- with (pt, SS(x.mid, y.mid, z.mid))
    if (sum(ipt) > 0)
      plist$pt <- with(pt, list(x.mid = x.mid[ipt], y.mid = y.mid[ipt],
        z.mid = z.mid[ipt], col = col[ipt], pch = pch[ipt],
        cex = cex[ipt], bg = bg[ipt], proj = proj[ipt]))
    else
      plist$pt <- NULL
  }
 #
  if (!is.null(plist$CIpt)) {
    CIpt <- plist$CIpt
    ipt <- with (CIpt, SS(x.mid, y.mid, z.mid))
    if (sum(ipt) > 0)
      plist$CIpt <- with (CIpt, list(x.to = x.to[ipt,], y.to = y.to[ipt,],
       z.to = z.to[ipt,], x.from = x.from [ipt,], y.from = y.from[ipt,],
       z.from = z.from[ipt,], nCI = nCI[ipt], x.mid = x.mid[ipt],
       y.mid = y.mid[ipt], z.mid = z.mid[ipt], length = CIpt$length[ipt],
       col= col[ipt], pch = pch[ipt],
       bg = bg[ipt], cex = cex[ipt], proj = proj[ipt],
       CIpar = list (col = CIpar$col[ipt], lwd = CIpar$lwd[ipt],
        lty = CIpar$lty[ipt], alen = CIpar$alen[ipt])))
    else
      plist$CIpt <- NULL
  }

 #
  if (! is.null(plist$poly)) {
    poly  <- plist$poly
    ip <- with (poly, SS(colMeans(x, na.rm = TRUE),
      colMeans(y, na.rm = TRUE), colMeans(z, na.rm = TRUE)))
    if (sum(ip) > 0)
      plist$poly <- with (poly, list(x = x[,ip], y = y[,ip], z = z[,ip],
        col = col[ip], border = border[ip], lwd = lwd[ip], lty = lty[ip], 
        isimg = isimg[ip], proj = proj[ip]))
    else
      plist$poly <- NULL
  }

  if (!is.null(plist$img)) {
    for (i in plist$imgnr:1) {
      img <- plist$img[[i]]
      
      # expand all values of x and y
      if (is.vector(img$x)) {
        XY <- mesh(img$x, img$y)
        img$x <- XY$x; img$y <- XY$y
      }
      Nx <- nrow(img$z)
      Ny <- ncol(img$z)
      ipt <- SS(img$x, img$y, img$z)
      if (sum(ipt) > 0) {
        Select <- img$z; Select[] <- ipt
        Noselect <- which(ipt == 0)
#        if (! all(is.na(img$col)))
#           img$col[Noselect] <- "transparent"
        arrsel <- which (Select == 1, arr.ind = TRUE)
        xr <- range(arrsel[,1])
        yr <- range(arrsel[,2])
  
        xsel <- xr[1] : xr[2]
        ysel <- yr[1] : yr[2]
        if (is.vector(plist$img[[i]]$x)) 
          plist$img[[i]]$x <- plist$img[[i]]$x[xsel]
        else
          plist$img[[i]]$x <- img$x[xsel, ysel]

        if (is.vector(plist$img[[i]]$x)) 
          plist$img[[i]]$y <- plist$img[[i]]$y[ysel]
        else
          plist$img[[i]]$y <- img$y[xsel, ysel]
        
        plist$img[[i]]$z <- img$z[xsel, ysel]
        plist$img[[i]]$col <- img$col[xsel, ysel]
          
      } else {
        plist$img[[i]] <- NULL    
        plist$imgnr <- plist$imgnr - 1
      }  
    }
  }
  
  if (!is.null(plist$labels)) {
    labels <- plist$labels
    il <- with (labels, SS(x, y, z))
    if (sum(il) > 0)
      plist$labels <- with (labels, list(x = x[il], y = y[il], z = z[il],
      labels = labels[il], adj = adj[il], cex = cex[il],
      col = col[il], font = font[il], proj = proj[il]))
    else
      plist$labels <- NULL
  }

  if (!is.null(plist$segm)) {
    segm <- plist$segm
    is <- with (segm, SS(colMeans(rbind(as.vector(x.from), as.vector(x.to))),
      colMeans(rbind(as.vector(y.from), as.vector(y.to))), colMeans(rbind(as.vector(z.from), as.vector(z.to)))))
    if (sum(is) > 0)
      plist$segm <- with (segm, list(x.from = as.vector(x.from)[is], 
        y.from = as.vector(y.from)[is], z.from = as.vector(z.from)[is], 
        x.to = as.vector(x.to)[is], y.to = as.vector(y.to)[is], 
        z.to = as.vector(z.to)[is],
        proj = proj[is], lwd  = lwd[is], lty  = lty[is], col  = col[is]))
    else
      plist$segm <- NULL
  }

  if (!is.null(plist$arr)) {
    arr <- plist$arr
    is <- with (arr, SS(colMeans(rbind(as.vector(x.from), as.vector(x.to))),
      colMeans(rbind(as.vector(y.from), as.vector(y.to))), colMeans(rbind(as.vector(z.from), as.vector(z.to)))))
    if (sum(is) > 0)
      plist$arr <- with (arr, list(x.from = as.vector(x.from)[is], 
        y.from = as.vector(y.from)[is], z.from = as.vector(z.from)[is], 
        x.to = as.vector(x.to)[is], y.to = as.vector(y.to)[is], 
        z.to = as.vector(z.to)[is],
        proj = proj[is], lwd  = lwd[is], lty  = lty[is], col  = col[is],
        code = code[is], angle = angle[is], length = length[is], type = type[is]))
    else
      plist$arr <- NULL
  }

  # new ranges   
   plist$xlim <- newlim(c(plist$pt$x.mid, plist$CIpt$x.to, plist$CIpt$x.from,
      plist$poly$x, plist$labels$x, plist$segm$x.from, plist$segm$x.to,
      plist$arr$x.from, plist$arr$x.to), TRUE)
   plist$ylim <- newlim(c(plist$pt$y.mid, plist$CIpt$y.to, plist$CIpt$y.from,
      plist$poly$y, plist$labels$y, plist$segm$y.from, plist$segm$y.to,
      plist$arr$y.from, plist$arr$y.to), FALSE)
   plist$zlim <- newlim(c(plist$pt$z.mid, plist$CIpt$z.to, plist$CIpt$z.from,
      plist$poly$z, plist$labels$z, plist$segm$z.from, plist$segm$z.to,
      plist$arr$z.from, plist$arr$z.to), FALSE)
  return (plist)
}

newlim <- function(xx, verbose) {
  if (length(xx) == 0) {
    if (verbose) 
      warning("nothing selected")
    return(c(-0.1,0.1))
  }
  lim <- range(xx, na.rm = TRUE)

  if (any(is.infinite(lim))) return(c(-0.1,0.1))
  if (diff(lim) == 0)
    lim <- lim * c(0.8, 1.2)
  if (diff(lim) == 0)
    lim <- lim + c(-0.1, 0.1)
  return(lim)
}
