
## =============================================================================
## RGL versions of the functions in package plot3D
## =============================================================================
par3dpars <- c("antialias","cex","family","font","useFreeType","fontname",
  "FOV","ignoreExtent","modelMatrix", "mouseMode", "projMatrix","bg",
  "scale","skipRedraw","userMatrix","viewport","zoom","bbox", "windowRect")

plotrgl <- function(lighting = FALSE, new = TRUE, add = FALSE, smooth = FALSE, ...) {

  plist <- getplist() 
  if (plist$type =="2D") {
    plotrgl2D(plist, new = new, add = add, smooth = smooth, plot = TRUE, ...)
    pp <- getplist()
    pp$twoD <- plist$twoD
    pp$converted <- TRUE
    plist <- pp
  } else
    plist <- plotrglplist (plist, lighting, new = new, add = add, smooth = smooth, 
         update = TRUE, scale = TRUE, ...) 

  plist$rgl$userMatrix <- par3d("userMatrix")
  setplist(plist)
  dots <- list(...)
  if (is.null(dots$mouseMode)) 
    pan3d(3)           # from the help of rgl.setMouseCallbacks
}

## =============================================================================
## Plot all items of a plist
## =============================================================================

plotrglplist <- function(plist, lighting = FALSE, new = TRUE, smooth = FALSE,
  add = FALSE, update = TRUE, scale = TRUE, ...) {

  if (add)
    new <- FALSE
  dots <- list(...)
  materialnames <- names(formals(rgl.material))
  material <- dots[names(dots) %in% materialnames] 
  material$lit <- lighting
  if (new) 
    do.call("open3d", dots[names(dots) %in% par3dpars]) #[!names(dots) %in% materialnames])
  else {
    if (! add)
      rgl.clear()
    do.call("material3d", dots[names(dots) %in% par3dpars]) #dots[!names(dots) %in% materialnames])
  }
  if (! is.null(material))
    do.call("material3d", material)

  if (length(plist) == 0)
    stop("nothing to draw")

  if (scale & !add) 
    aspect3d(plist$scalefac$x, plist$scalefac$y, plist$scalefac$z)

  # images
  Poly <- plist$poly

 # make col the correct size for persp3d
  if (! is.null(plist$imgnr)) {
    changedimg <- FALSE
    for (i in 1:plist$imgnr) {
      img <- plist$img[[i]]
        if (any(dim(img$col) - dim(img$x)) != 0) {
          i1 <- cbind(img$col, img$col[,ncol(img$col)])
          CC <- rbind(i1[1,], i1)
          plist$img[[i]]$col <- CC 
          changedimg <- TRUE
       } 
    }
    if (changedimg) setplist(plist)
  }
  
  if (smooth & ! is.null(plist$imgnr)) {
  
    for (i in 1:plist$imgnr) {
      img <- plist$img[[i]]
       if (!all(is.na(img$col))) 
        persp3d(img$x, img$y, img$z, col = img$col, add = TRUE, aspect = FALSE)
    }
    
   # the borders?      
    im <- which(Poly$isimg & ! is.na(Poly$border))
    rglpoly(Poly, im, "line")

   # polygons that are not images
    p <- !Poly$isimg
    poly <- list(x = Poly$x[,p], y = Poly$y[,p], z = Poly$z[,p], 
        col = Poly$col[p], border = Poly$border[p],
        lwd = Poly$lwd[p], lty = Poly$lty[p])
        
  } else poly <- Poly
  
 # two types of polygons
  if (length(poly$x) > 0) {
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
  if (length(segm$x.from) > 0) {
    ilwd <- unique(segm$lwd) 
    for (i in 1:length(ilwd)) {
      ii <- which(segm$lwd == ilwd[i])
      segments3d(x = rbind(as.vector(segm$x.from)[ii], as.vector(segm$x.to)[ii]), 
                 y = rbind(as.vector(segm$y.from)[ii], as.vector(segm$y.to)[ii]),
                 z = rbind(as.vector(segm$z.from)[ii], as.vector(segm$z.to)[ii]),
                 color = matrix (nrow = 2, byrow = TRUE, data = rep(segm$col[ii], 2)),
                 lwd = ilwd[i], lty = segm$lty[ii[1]])   
    }           
  }
 # arrows 
  arr <- plist$arr
  if (length(arr$x.from) > 0) {
    ilwd <- unique(arr$lwd) 
    for (i in 1:length(ilwd)) {
      ii <- which(arr$lwd == ilwd[i])
      
      arrfun(as.vector(arr$x.from)[ii], as.vector(arr$y.from)[ii], as.vector(arr$z.from)[ii], 
              as.vector(arr$x.to)[ii], as.vector(arr$y.to)[ii], as.vector(arr$z.to)[ii],
              code = arr$code[ii], col = arr$col[ii], lty = arr$lty[ii], 
              lwd = ilwd[i], length = arr$length[ii], angle = arr$angle[ii],
              type = arr$type[ii], sx = 1/plist$scale$x, sy = 1/plist$scale$y, sz = 1/plist$scale$z)      
    }
  }
  pts <- plist$pt 
  if (length(pts$x.mid) > 0) {
    ii <- which(pts$pch == ".") 
    if (length (ii) > 0) {
      unsize <- unique(pts$cex[ii])
      for (j in unsize) {
        ij <- ii[which(pts$cex[ii] == j)]
        plot3d(x = pts$x.mid[ij], y = pts$y.mid[ij], z = pts$z.mid[ij],
               size = 1 *j, col = pts$col[ij], add = TRUE)
      }
    }
    ii <- which(pts$pch != ".") 
    
    if (length (ii) > 0) {
      unsize <- unique(pts$cex[ii])
      for (j in unsize) {
        ij <- ii[which(pts$cex[ii] == j)]
        plot3d(x = pts$x.mid[ij], y = pts$y.mid[ij], z = pts$z.mid[ij],
               size = 6 *j, col = pts$col[ij], add = TRUE)
      }
    } 
  }
  
  pts <- plist$CIpt
  if (length(pts$x.mid) > 0) {
    ii <- which(pts$pch == ".") 
    if (length (ii) > 0) {
      unsize <- unique(pts$cex[ii])
      for (j in unsize) {
        ij <- ii[which(pts$cex[ii] == j)]
        plot3d(x = pts$x.mid[ij], y = pts$y.mid[ij], z = pts$z.mid[ij],
              size = 1 *j, col = pts$col[ij], add = TRUE)
      }
    }
    ii <- which(pts$pch != ".") 
    if (length (ii) > 0) {
      unsize <- unique(pts$cex[ii])
      for (j in unsize) {
        ij <- ii[which(pts$cex[ii] == j)]
        plot3d(x = pts$x.mid[ij], y = pts$y.mid[ij], z = pts$z.mid[ij],
             size = 6 *j, col = pts$col[ij], add = TRUE)
      }
    }
    nCImax <- max(pts$nCI)
    
    for (i in 1: nCImax) {             
      ii <- which(i <= pts$nCI)
      segments3d(x = rbind(pts$x.from[ii,i], pts$x.to[ii,i]), 
                 y = rbind(pts$y.from[ii,i], pts$y.to[ii,i]),
                 z = rbind(pts$z.from[ii,i], pts$z.to[ii,i]),
                 color = matrix (nrow = 2, byrow = TRUE, data = rep(pts$CIpar$col[ii], 2)),
                 lwd = pts$CIpar$lwd[1], lty = pts$CIpar$lty[1])   
    }           
  }
  
  labs <- plist$labels
  if (length(labs) > 0)
    text3d(x = labs$x, y = labs$y, z = labs$z,
               color = labs$col, texts = labs$labels, font = labs$font,
               cex = labs$cex, adj = labs$adj[1]) 

  D <- NULL
  if (plist$persp$drawbox & !add) {
    axes <- FALSE
    
    if (! is.null(plist$dot$ticktype))
      if (plist$dot$ticktype == "detailed")
        axes <- TRUE

    D <- decorate3d(xlim = plist$xlim, ylim = plist$ylim, zlim = plist$zlim, 
	     xlab = plist$dot$xlab, ylab = plist$dot$ylab, zlab =  plist$dot$zlab, 
	     main = plist$dot$main, sub = plist$dot$sub, axes = axes)
  } else if (!add)
    D <- title3d(xlab = plist$dot$xlab, ylab = plist$dot$ylab, zlab = plist$dot$zlab, 
	     main = plist$dot$main, sub = plist$dot$sub)	

  pp <- getplist()
  pp$rgl$lighting <- lighting 
  pp$rgl$smooth <- smooth
  pp$rgl$alpha <- material3d()$alpha
	if (! is.null(D)) 
    pp$rgl$D <- as.list(D)
  setplist(pp)
  invisible(pp)
}

## =============================================================================
## Plot polygons of a plist
## =============================================================================
  
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

## =============================================================================
## An arrow function, 
## =============================================================================
# TO BE IMPROVED...
  
hypoth <- function(a, b) sqrt(a^2 + b^2)

arrfun <- function (x.from, y.from, z.from, x.to, y.to, z.to, code, 
    col, lty, lwd, ..., length, angle = 20, type = "simple",
    sx, sy, sz)  # scales       
{
#  if (is.null(angle)) angle <- 30
  length <- length/3
  if (any(length < 0 | ! is.finite(length   )))
    stop ("invalid arrow head length")

  if (any(!is.finite(angle)))
    stop ("invalid arrow head angle")

  if (any(is.na(code) | code < 0  | code > 3))
    stop ("invalid arrow head specification")

  N <- length(x.from)
  Code   <- rep(code, length.out = N)
  Length <- rep(length, length.out = N)
  Col    <- rep(col, length.out = N)
  Angle  <- rep(angle*pi/180, length.out = N)
  Type   <- rep(type, length.out = N)

  color <- matrix (nrow = 2, byrow = TRUE, data = rep(Col, 2))
  segments3d(x = rbind(x.from, x.to), y = rbind(y.from, y.to),
             z = rbind(z.from, z.to), color = color,
             lwd = lwd[1], lty = lty[1])   

## ? something with scale???
  
  eps <- 1e-8
  dd <- hypoth(x.from - x.to, y.from - y.to)
    
  arrhead <- function (code) {
  
    i <- which (Code == code)
    if (length(i) == 0) return()
    length <- Length[i]
    col    <- rbind(Col[i], Col[i], Col[i], Col[i])
    angle  <- Angle[i]
    type   <- Type[i]

    if (code %in% c(1, 3)) {
    	x1 <- x.from[i]
    	y1 <- y.from[i]
  	  z1 <- z.from[i]
  	  x0 <- x.to[i]
  	  y0 <- y.to[i]
  	  z0 <- z.to[i]
    } else {
    	x1 <- x.to[i]
    	y1 <- y.to[i]
    	z1 <- z.to[i]
  	  x0 <- x.from[i]
  	  y0 <- y.from[i]
  	  z0 <- z.from[i]
    }
  	xc <- x0 - x1
   	yc <- y0 - y1
    zc <- z0 - z1

    r <- sqrt((yc/sy)^2 + (xc/sx)^2 + (zc/sz)^2)
    rot <- atan2(yc/sy, xc/sx) 
    rot2 <- acos((zc/sz)/r)  

    is <- which (type == "cone")
    if (length(is) > 0) {
      N <- 30
      pseq <- seq(0, 2*pi, length.out = N+1)
# the long version, with a loop      
#      for (i in 1: N) {
#        a.xy.1 <- angle[is]*cos(pseq[i])
#        a.xy.2 <- angle[is]*cos(pseq[i+1])
#        a.z.1 <- angle[is]*sin(pseq[i])
#        a.z.2 <- angle[is]*sin(pseq[i+1])
       
#       	x <- rbind (x1[is] + length[is] * sx * cos(rot[is] + a.xy.1)*sin(rot2[is] + a.z.1), x1[is], 
#                    x1[is] + length[is] * sx * cos(rot[is] + a.xy.2)*sin(rot2[is] + a.z.2))
#       	y <- rbind (y1[is] + length[is] * sy * sin(rot[is] + a.xy.1)*sin(rot2[is] + a.z.1), y1[is],  
#                    y1[is] + length[is] * sy * sin(rot[is] + a.xy.2)*sin(rot2[is] + a.z.2))
#     	  z <- rbind (z1[is] + length[is] * sz * cos(rot2[is] + a.z.1), z1[is], 
#     	              z1[is] + length[is] * sz * cos(rot2[is] + a.z.2))
#       }

      Nx <- length(is)
      x <- as.vector(outer (x1[is], 1:N, FUN = function (x,i)
             x + length[is] * sx * cos(rot[is] + angle[is]*cos(pseq[i]))*
                                   sin(rot2[is] + angle[is]*sin(pseq[i]))))
      x <- rbind(x, rep(x.to[is], times = N), c(x[-(1:Nx)], x[1:Nx]))

      y <- as.vector(outer (y1[is], 1:N, FUN = function (y,i)
             y + length[is] * sy * sin(rot[is] + angle[is]*cos(pseq[i]))*
                                   sin(rot2[is] + angle[is]*sin(pseq[i]))))
      y <- rbind(y, rep(y1[is], times = N), c(y[-(1:Nx)], y[1:Nx]))

      z <- as.vector(outer (z1[is], 1:N, FUN = function (z,i)
             z + length[is] * sz * cos(rot2[is] + angle[is]*sin(pseq[i]))))
      z <- rbind(z, rep(z1[is], times = N), c(z[-(1:Nx)], z[1:Nx]))

      triangles3d(x = x, y = y, z = z, col = col[1:3, is], lty = lty[is[1]], 
           front = "fill", lwd = lwd[1])                    
    }
    ii <- which (type != "cone") 
    if (length(ii) == 0) return()

   	x <- rbind (x1 + length * sx * cos(rot+angle)*sin(rot2), x1, 
                x1 + length * sx * cos(rot-angle)*sin(rot2), NA)
   	y <- rbind (y1 + length * sy * sin(rot+angle)*sin(rot2), y1,  
                y1 + length * sy * sin(rot-angle)*sin(rot2), NA)
   	z <- rbind (z1 + length * sz * cos(rot2), z1, 
     	          z1 + length * sz * cos(rot2), NA)
    is <- which (type == "simple")
    if (length(is) > 0)  	        
     lines3d(x[,is], y[,is], z[,is], col = col[,is], lwd = lwd[1], lty = lty[is[1]])                    

    is <- which (type == "triangle")
    if (length(is) > 0)  	        
      triangles3d(x = x[1:3,is], y = y[1:3,is], z = z[1:3,is], col = col[1:3,is], lty = lty[is[1]], 
        front = "fill", lwd = lwd[1])                    
  }
  arrhead(1)
  arrhead(2)
  arrhead(3)
}






plotrgl2D <- function(plist, new, add, smooth, plot = FALSE, ...) {
  
  checkdots <-  function(pdots, dots, add) {
    if (! add) {
      if (! is.null(dots$xlim)) 
        pdots$xlim <- dots$xlim
      if (! is.null(dots$ylim)) 
        pdots$ylim <- dots$ylim
    }
    pdots$colkey <- pdots$clab <- pdots$facets <- pdots$resfac <- pdots$theta <- NULL 
    pdots$rasterImage <- NULL 
    pdots$new <- new
    pdots
  }
  
  img2Dnr <- cont2Dnr <- scat2Dnr <- arr2Dnr <- segm2Dnr <- rect2Dnr <- poly2Dnr <- 0
  dots <- list(...)
#  if (add)
#    new <- FALSE
#  if (new) 
#    do.call("open3d", dots[names(dots) %in% par3dpars]) #[!names(dots) %in% materialnames]) 
#  else if (! add)
#      rgl.clear()
#  add <- TRUE
  p <- plist$twoD
  for (i in 1:length(p$order)) {
    plt <- p$order[i]
    if (plt  == "image") {
      img2Dnr <- img2Dnr + 1
      Dots <- checkdots(p$img2D[[img2Dnr]], dots, add)
      do.call ("imagergl", c(alist(add = add, smooth = smooth), Dots))
    } else if (plt  == "contour") {
      cont2Dnr <- cont2Dnr + 1
      Dots <- checkdots(p$cont2D[[cont2Dnr]], dots, add)
      do.call ("contourrgl", c(alist(add = add), Dots))
    } else if (plt == "scatter") {
      scat2Dnr <- scat2Dnr + 1
      Dots <- checkdots(p$scat2D[[scat2Dnr]], dots, add)
      do.call ("scatterrgl", c(alist(add = add), Dots))
    } else if (plt == "arrows") {
      arr2Dnr <- arr2Dnr + 1
      Dots <- checkdots(p$arr2D[[arr2Dnr]], dots, add)
      do.call ("arrowsrgl", c(alist(add = add), Dots))
    } else if (plt == "segments") {
      segm2Dnr <- segm2Dnr + 1
      Dots <- checkdots(p$segm2D[[segm2Dnr]], dots, add)
      do.call ("segmentsrgl", c(alist(add = add), Dots))
    } else if (plt == "rect") {
      stop("rect not supported in rgl")
    } else if (plt == "polygon") {
      stop("polygons not supported in rgl")
    }
    add <- TRUE
    new <- FALSE
  }
  
  invisible(plist)
 
}

