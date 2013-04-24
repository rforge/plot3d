\name{cutting in rgl plots}
\alias{cutrgl}
\alias{uncutrgl}
\title{
  Cutting a rectangular region from an rgl plot. 
}
\description{
  \code{cutrgl} zooms in on a selected region of the plot. It overwrites the current plot.
  Selection is done by dragging over the plot, with the left mousekey clicked.

  \code{uncutrgl} restores the original plot, but keeps the current orientation.
}
\usage{
cutrgl (...) 
uncutrgl (...) 
}

\arguments{
  \item{... }{Any argument that can be passed to the function \link{plotrgl}, 
    e.g argument \code{lighting}, or to the \code{rgl} functions 
    \link{par3d}, \link{open3d} or \link{material3d}.
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
  Both functions will not work when another active rgl window has been opened.
  In that case, \code{cutrgl} will freeze R, and the escape key should be used.
}
\examples{
\dontrun{
 ribbon3D(z = volcano, zlim= c(-100, 200), image = TRUE, plot = FALSE)
 plotrgl(new = TRUE) # new window
 cutrgl()           # select region with left mouse
 cutrgl()           # second selection
 uncutrgl()         # original restored
}

}
\keyword{ hplot }

