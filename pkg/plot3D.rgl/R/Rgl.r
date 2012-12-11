
## =============================================================================
## RGL versions of the functions
## =============================================================================

  rglpoly <- function(poly, il, front) { 
   
    if (is.null(il)) {    # choose all
      i.Tri  <- which(is.na (poly$x[4, ]))
      i.Quad <- which(!is.na(poly$x[4, ]))
  
    } else {
      i.Tri  <- il[which(is.na (poly$x[4, il]))]
      i.Quad <- il[which(!is.na(poly$x[4, il]))]
    }

    if (length(i.Tri > 0) )  {
      if (front == "fill")  {
        it <- i.Tri[poly$col[i.Tri] != "transparent" ]
        triangles3d(x = poly$x[1:3, it], y = poly$y[1:3, it], z = poly$z[1:3, it], 
          col = matrix (nrow = 3, byrow = TRUE, data = rep(poly$col[it], 3)), 
          front = front, lwd = poly$lwd[it[1]]) 
        ii <- which(poly$border[i.Tri] != poly$col[i.Tri])
        if (length(ii) > 0) {
          is <- i.Tri[ii]
          ir <- c(1:3, 1, 4)
          lines3d(x = poly$x[ir, is], y = poly$y[ir, is], z = poly$z[ir, is], 
            col = matrix (nrow = 4, byrow = TRUE, data = rep(poly$border[is], 4)), 
          lty = poly$lty[is], lwd = poly$lwd[is[1]]) 
        }
      } else  
         triangles3d(x = poly$x[1:3, i.Tri], y = poly$y[1:3, i.Tri], z = poly$z[1:3, i.Tri], 
          col = matrix (nrow = 3, byrow = TRUE, data = rep(poly$border[i.Tri], 3)), 
          front = front, back = front, lwd = poly$lwd[i.Tri[1]]) 
   }
    if (length(i.Quad > 0) )
      if (front == "fill")  {
        iq <- i.Quad[poly$col[i.Quad] != "transparent" ]
        ir <- c(1:4, 1, 5)
        quads3d(x = poly$x[1:4, iq], y = poly$y[1:4, iq], z = poly$z[1:4, iq], 
          col = matrix (nrow = 4, byrow = TRUE, data = rep(poly$col[iq], 4)), 
          front = front, lwd = poly$lwd[iq[1]]) 
        ii <- which(poly$border[i.Quad] != poly$col[i.Quad])
        if (length(ii) > 0) {
          is <- i.Quad[ii]
          ir <- c(1:4, 1, 5)
          lines3d(x = poly$x[ir, is], y = poly$y[ir, is], z = poly$z[ir, is], 
          col = matrix (nrow = 5, byrow = TRUE, data = rep(poly$border[is], 5)), 
          lty = poly$lty[is], lwd = poly$lwd[is[1]]) 
        }
      } else
        quads3d(x = poly$x[1:4, i.Quad], y = poly$y[1:4, i.Quad], z = poly$z[1:4, i.Quad], 
          col = matrix (nrow = 4, byrow = TRUE, data = rep(poly$border[i.Quad], 4)), 
          front = front, back = front, lwd = poly$lwd[i.Quad[1]]) 
   }
   
## =============================================================================

plotrgl <- function(new = TRUE, lighting = FALSE, ...) {

  dots <- list(...)
  materialnames <- names(formals(rgl.material))
  material <- dots[names(dots) %in% materialnames] 
  material$lit <- lighting
  if (new) 
    do.call("open3d", dots[!names(dots) %in% materialnames])
  else
    rgl.clear()

#  if (! is.null(par3d))
#    do.call("par3d", par3d)
    
  if (! is.null(material))
    do.call("material3d", material)
    

  plist <- getplist()
  if (length(plist) == 0)
    stop("nothing to draw")

#  if (new)
  aspect3d(plist$scalefac$x, plist$scalefac$y, plist$scalefac$z)

 # two types of polygons
  poly <- plist$poly
  if (! is.null(poly)) {

    if (nrow(poly$x) > 5)
      stop ("cannot handle true polygons in rgl")

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
                 radius = 0.005 *pts$cex[ii],
                 color = pts$col[ii])
  
    ii <- which(pts$pch != ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
                 radius = 0.0175 *pts$cex[ii],
                 color = pts$col[ii])
  }
  
  pts <- plist$CIpt
  if (length(pts$x.mid) > 0) {
    ii <- which(pts$pch == ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
               color = pts$col[ii], 
               radius = 0.005 *pts$cex[ii]) 
    ii <- which(pts$pch != ".") 
    if (length (ii) > 0) 
      spheres3d(x = pts$x.mid[ii], y = pts$y.mid[ii], z = pts$z.mid[ii],
               color = pts$col[ii], 
               radius = 0.0175 *pts$cex[ii]) 
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

  if (plist$persp$drawbox) {
    axes <- FALSE
    
    if (! is.null(plist$dot$ticktype))
      if (plist$dot$ticktype == "detailed")
        axes <- TRUE

    decorate3d(xlim = plist$xlim, ylim = plist$ylim, zlim = plist$zlim, 
	     xlab = plist$dot$xlab, ylab = plist$dot$ylab, zlab =  plist$dot$zlab, 
	     main = plist$dot$main, sub = plist$dot$sub, axes = axes)
  }
	
}

  