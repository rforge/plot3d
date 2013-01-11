

zoomrgl <- function(...) {
  SS <- select3d()
  plist <- selectplist(getplist(), SS)
  rgl.clear()
  dots <- list(...)
  light <- dots$lighting
  if (is.null(light))
    light <- FALSE
  dots$lighting <- dots$new <- NULL
  do.call ("plotrglplist", c(alist(plist, lighting = light, new = FALSE, update = FALSE, scale = FALSE), dots))
}

unzoomrgl <- function(...) {
  rgl.clear()
  dots <- list(...)
  light <- dots$lighting
  if (is.null(light))
    light <- FALSE
  dots$lighting <- dots$new <- NULL
  do.call ("plotrgl", c(alist(lighting = light, new = FALSE), dots))
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
        col = col[ip], border = border[ip], lwd = lwd[ip], lty = lty[ip], proj = proj[ip]))
    else
      plist$poly <- NULL
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
    is <- with (segm, SS(colMeans(rbind(x.from, x.to)),
      colMeans(rbind(y.from, y.to)), colMeans(rbind(z.from, z.to))))
    if (sum(is) > 0)
      plist$segm <- with (segm, list(x.from = x.from[is], y.from = y.from[is],
        z.from = z.from[is], x.to = x.to[is], y.to = y.to[is], z.to = z.to[is],
        proj = proj[is], lwd  = lwd[is], lty  = lty[is], col  = col[is]))
    else
      plist$segm <- NULL
  }

  if (!is.null(plist$arr)) {
    arr <- plist$arr
    is <- with (arr, SS(colMeans(rbind(x.from, x.to)),
      colMeans(rbind(y.from, y.to)), colMeans(rbind(z.from, z.to))))
    if (sum(is) > 0)
      plist$arr <- with (arr, list(x.from = x.from[is], y.from = y.from[is],
        z.from = z.from[is], x.to = x.to[is], y.to = y.to[is], z.to = z.to[is],
        proj = proj[is], lwd  = lwd[is], lty  = lty[is], col  = col[is],
        code = code[is], angle = angle[is], length = length[is]))
    else
      plist$arr <- NULL
  }

  plist$xlim <- plist$ylim <- plist$zlim <- NULL
  return (plist)
}

