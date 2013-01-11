\name{Zooming in on rgl plots}
\alias{zoomrgl}
\alias{unzoomrgl}
\title{
  Zooming in on an rgl plot. 
}
\description{
  \code{zoomrgl} zooms in on a selected region of the plot. It overwrites the current plot.
  Selection is done by dragging over the plot, with the left mousekey clicked.

  \code{unzoomrgl} restores the original plot (but keeps the current orientation).
}
\usage{
zoomrgl (...) 
unzoomrgl (...) 
}

\arguments{
  \item{... }{Any argument that can be passed to the \link{plotrgl} function, 
    e.g arguments \code{lighting}, from \link{par3d}, \link{open3d} or \link{material3d}.
    Exceptions are \code{new} and \code{add} (which are always \code{FALSE}).
    }
}

\seealso{
  \link{plotrgl}, 
  \link{material3d}, \link{par3d} for \code{rgl} arguments that can be passed to 
  the function.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\note{
  Both functions will not work when the active rgl window has been closed.
  In that case, \code{zoomrgl} will freeze R, and the escape key should be used.
}
\examples{
\dontrun{
 ribbon3D(z = volcano, zlim= c(-100, 200), image = TRUE)
 plotrgl(new = TRUE) # new window
 zoomrgl()           # select region with left mouse
 zoomrgl()           # second selection
 unzoomrgl()         # original restored
}

}
\keyword{ hplot }

