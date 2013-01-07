#??contour3D along x, y
 require(plot3D.rgl)
persp3D(z = volcano, facets = FALSE, lighting = TRUE, curtain = TRUE, 
 lphi = 45, ltheta = 45, phi = 0)
