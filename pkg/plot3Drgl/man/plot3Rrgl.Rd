\name{plot3Drgl-package}
\alias{plot3Drgl-package}
\alias{plot3Drgl}
\docType{package}
\title{
  Functions to plot multi-dimensional data using \code{rgl}.
}
\description{
  Package \code{plot3Drgl} provides an interface from package
  \code{plot3D} to package \code{rgl}.

  It will plot most (but not all) features from plots generated with \code{plot3D}, 
  except for the color keys and polygons. 
  
  It also also includes rgl implementations of 2-D functions 
  (arrows, points, contours, images), 
  which can be zoomed, moved, and sections selected.
  
}
\author{
  Karline Soetaert
}
%\references{
%% to be filled
%}
\seealso{
Functions from R-package \code{plot3D}.

Functions from R-package \code{rgl}.

\itemize{
 \item For 3-D graphs:
  \itemize{
   \item \link{plotrgl} the main function that translates plot3D graphs to rgl.
  }
 \item For 2-D graphs:
 \itemize{
   \item \link{arrows2Drgl} creates 2-D arrow plots.
   \item \link{scatter2Drgl} for point and line plots. 
   \item \link{image2Drgl}, \link{contour2Drgl} an rgl implementation of the image  and contour functions.
 }
}
  Apart from the usual zooming, it is also possible to move the figure in the rgl window
  (based on an example in the rgl package).

  Rectangular areas can be selected from rgl plots, using \link{cutrgl} while
  \link{uncutrgl} will restore the original plot.
}


\keyword{ package }
