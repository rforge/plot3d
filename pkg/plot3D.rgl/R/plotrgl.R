
## =============================================================================
## RGL versions of the functions in package plot3D
## =============================================================================


plotrgl <- function(lighting = FALSE, new = TRUE, add = FALSE, ...) {

  plist <- getplist()
  plotrglplist (plist, lighting, new = new, add = add, update = TRUE, scale = TRUE, ...) 
}

## =============================================================================
## Plot all items of a plist
## =============================================================================

plotrglplist <- function(plist, lighting = FALSE, new = TRUE, 
  add = FALSE, update = TRUE, scale = TRUE, ...) {

  dots <- list(...)
  materialnames <- names(formals(rgl.material))
  material <- dots[names(dots) %in% materialnames] 
  material$lit <- lighting
  if (new) 
    do.call("open3d", dots[!names(dots) %in% materialnames])
  else {
    if (! add)
      rgl.clear()
    do.call("material3d", dots[!names(dots) %in% materialnames])
  }
  if (! is.null(material))
    do.call("material3d", material)

  if (length(plist) == 0)
    stop("nothing to draw")

  if (scale) 
    aspect3d(plist$scalefac$x, plist$scalefac$y, plist$scalefac$z)
    
  meanscale <- plist$scalefac$expand     # this seems to work ...
#  meanscale <- sqrt(sum(par3d("scale")^2)/3)*material3d("size")

 # two types of polygons
  poly <- plist$poly
  if (! is.null(poly)) {

    if (nrow(poly$x) > 5)
      stop ("cannot handle polygons with more than 4 nodes in plotrgl")

    ifill <- which(! is.na(poly$col))
    if (length(ifill) > 0)
      rglpoly(poly, ifill, "fill")
    iline <- which(is.na(poly$col))
    if (length(iline) > 0)
      rglpoly(poly, iline, "line")
    
  }
 # line segments
  segm <- plist$segm
  if (length(segm$x.from) > 0)
    segments3d(x = rbind(as.vector(segm$x.from), as.vector(segm$x.to)), 
               y = rbind(as.vector(segm$y.from), as.vector(segm$y.to)),
               z = rbind(as.vector(segm$z.from), as.vector(segm$z.to)),
               color = matrix (nrow = 2, byrow = TRUE, data = rep(segm$col, 2)),
               lwd = segm$lwd[1], lty = segm$lty[1])   

 # arrows ... not supported - only segments and a "text"
  arr <- plist$arr
  if (length(arr$x.from) > 0) {
    segments3d(x = rbind(as.vector(arr$x.from), as.vector(arr$x.to)), 
               y = rbind(as.vector(arr$y.from), as.vector(arr$y.to)),
               z = rbind(as.vector(arr$z.from), as.vector(arr$z.to)),
               color = matrix (nrow = 2, byrow = TRUE, data = rep(arr$col, 2)),
               lwd = arr$lwd[1], lty = arr$lty[1])   
    ii <- which(arr$code %in% c(1, 3))
    if (length (ii) > 0) 
      text3d(x = as.vector(arr$x.from[ii]), y = as.vector(arr$y.from[ii]), 
        z = as.vector(arr$z.from[ii]), text = "^", color = arr$col[ii])    

    ii <- which(arr$code %in% c(2, 3))
    if (length (ii) > 0) 
      text3d(x = as.vector(arr$x.to[ii]), y = as.vector(arr$y.to[ii]), 
        z = as.vector(arr$z.to[ii]), text = "^", color = arr$col[ii])    
  }

  pts <- plist$pt 
  if (length(pts$x.mid) > 0) {
    ii <- which(pts$pch == ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
                 radius = 0.005 *pts$cex[ii]*meanscale,
                 color = pts$col[ii])
  
    ii <- which(pts$pch != ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
                 radius = 0.0166667 *pts$cex[ii]*meanscale,
                 color = pts$col[ii])
  }
  
  pts <- plist$CIpt
  if (length(pts$x.mid) > 0) {
    ii <- which(pts$pch == ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
               color = pts$col[ii], 
               radius = 0.005 *pts$cex[ii]*meanscale) 
    ii <- which(pts$pch != ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
               color = pts$col[ii], 
               radius = 0.0166667 *pts$cex[ii]*meanscale) 
    nCImax <- max(pts$nCI)
    
    for (i in 1: nCImax) {             
      ii <- which(i <= pts$nCI)
      segments3d(x = rbind(pts$x.from[ii,i], pts$x.to[ii,i]), 
                 y = rbind(pts$y.from[ii,i], pts$y.to[ii,i]),
                 z = rbind(pts$z.from[ii,i], pts$z.to[ii,i]),
                 color = matrix (nrow = 2, byrow = TRUE, data = rep(pts$CIpar$col, 2)),
                 lwd = pts$CIpar$lwd[1], lty = pts$CIpar$lty[1])   
    }           
  }
  
  labs <- plist$labels
  if (length(labs) > 0)
    text3d(x = labs$x, y = labs$y, z = labs$z,
               color = labs$col, texts = labs$labels, font = labs$font,
               cex = labs$cex, adj = labs$adj[1]) 

  if (!scale)
    aspect3d(1,1,1)

  D <- NULL
  if (plist$persp$drawbox) {
    axes <- FALSE
    
    if (! is.null(plist$dot$ticktype))
      if (plist$dot$ticktype == "detailed")
        axes <- TRUE

    D <- decorate3d(xlim = plist$xlim, ylim = plist$ylim, zlim = plist$zlim, 
	     xlab = plist$dot$xlab, ylab = plist$dot$ylab, zlab =  plist$dot$zlab, 
	     main = plist$dot$main, sub = plist$dot$sub, axes = axes)
  } else 
    D <- title3d(xlab = plist$dot$xlab, ylab = plist$dot$ylab, zlab = plist$dot$zlab, 
	     main = plist$dot$main, sub = plist$dot$sub)	

	if (! is.null(D) & update) {
    plist$rgl$D <- as.list(D)
    setplist(plist)
  }
}

  
rglpoly <- function(poly, il, front) { 
   
  if (is.null(il)) {    # choose all
    i.Tri  <- which(is.na (poly$x[4, ]))
    i.Quad <- which(!is.na(poly$x[4, ]))
  
  } else {
    i.Tri  <- il[which(is.na (poly$x[4, il]))]
    i.Quad <- il[which(!is.na(poly$x[4, il]))]
  }

    
  plotpoly <- function(ipol, func, ir) {
    if (front == "fill")  {
      it <- ipol[poly$col[ipol] != "transparent" ]
      func(x = poly$x[1:ir, it], y = poly$y[1:ir, it], z = poly$z[1:ir, it], 
        col = matrix (nrow = ir, byrow = TRUE, data = rep(poly$col[it], ir)), 
        front = front, lwd = poly$lwd[it[1]]) 
      ii <- which(poly$border[ipol] != poly$col[ipol])
      if (length(ii) > 0) {
        is <- ipol[ii]
        irr <- c(1:ir, 1, ir+1)
        lines3d(x = poly$x[irr, is], y = poly$y[irr, is], z = poly$z[irr, is], 
          col = matrix (nrow = ir+2, byrow = TRUE, data = rep(poly$border[is], ir+2)), 
          lty = poly$lty[is[1]], lwd = poly$lwd[is[1]]) 
      }
    } else  
       func(x = poly$x[1:ir, ipol], y = poly$y[1:ir, ipol], z = poly$z[1:ir, ipol], 
          col = matrix (nrow = ir, byrow = TRUE, data = rep(poly$border[ipol], ir)), 
          front = front, back = front, lwd = poly$lwd[ipol[1]]) 
  }
  if (length(i.Tri) > 0)
    plotpoly(i.Tri, triangles3d, 3)
  if (length(i.Quad) > 0) 
    plotpoly(i.Quad, quads3d, 4)
}   

  
  