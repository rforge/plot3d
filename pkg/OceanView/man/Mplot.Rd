\name{Matrix plotting}
\alias{Mplot}
\alias{Msplit}
\alias{Mcommon}
\title{
 Functions for plotting matrices
}
\description{
  \code{Mplot} plots data from (a list of) matrices. 
  
  \code{Msplit} splits a matrix in a list according to factors.
  
  \code{Mcommon} creates a list of matrices that have only common variables.
}
\usage{
  Mplot (M, ..., x = 1, select = NULL, which = select, 
         subset = NULL, ask = NULL, legend = list(x = "top")) 

  Msplit (M, split = 1, subset = NULL) 

  Mcommon (M, ..., verbose = FALSE)      

}

\arguments{
  \item{M }{Matrix or data.frame to be plotted. For \code{Mplot}, \code{M} can be
    a list with matrices or data.frames. 
    }
  \item{x }{Name or number of the column to be used as the x-values. 
    }
  \item{select }{Which variable/columns to be selected.  This is added for
    consistency with the R-function \code{subset}.
    }
  \item{which }{The name(s) or the index to the variables that should be
    plotted or selected.  Default = all variables, except \code{time}. 
    }
  \item{subset }{Logical expression indicating elements or rows to keep in 
    \code{select}: missing values are taken as \code{FALSE}
    }
  \item{ask }{Logical; if \code{TRUE}, the user is \emph{ask}ed before
    each plot, if \code{NULL} the user is only asked if more than one
    page of plots is necessary and the current graphics device is set
    interactive, see \code{\link{par}(ask)} and
    \code{\link{dev.interactive}}.
    }
  \item{legend }{A \code{list} with parameters for the legend 
    to be added.
    } 
  \item{split }{The name or number of the column with the factor 
    according to which the matrix will be split. 
    } 
  \item{verbose }{If \code{TRUE} will write output to the screen. 
    } 
  \item{\dots}{Additional arguments passed to the methods. For \code{Mplot}:
    can also be extra matrices to plot. 
    The arguments after \dots must be matched exactly.
    }
}

\value{
  Function \code{Msplit} returns a list with the matrices, split according to 
  the factors; the names of the elements is set by the factor's name.

  Function \code{Mcommon} returns a list with the matrices, which only have
  the common variables.
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Create three dummy matrices
## =======================================================================

 M1 <- matrix(nrow = 10, ncol = 5, data = 1:50)
 colnames(M1) <- LETTERS[1:5]

 M2 <- M1[, c(1, 3, 4, 5, 2)] 
 M2[ ,-1] <- M2[,-1] /2
 colnames(M2)[3] <- "CC"  # Different name

 M3 <- matrix(nrow = 5, ncol = 4, data = runif(20)*10) 
 M3[,1] <- sort(M3[,1])
 colnames(M3) <- colnames(M1)[-3]

 head(M1); head(M2); head(M3)

# plot results -  will change mfrow 
 Mplot(M1, M2, M3, lwd = 2, mtext = "All variables versus 1st column",
       legend = list(x = "top", legend = c("M1", "M2", "M3")))

## =======================================================================
## Plot a selection or only common elements
## =======================================================================

 Mplot(M1, M2, M3, x = "B", select = c("A", "E"), pch = c(NA, 16, 1), 
       type = c("l", "p", "b"), col = c("black", "red", "blue"),  
       legend = list(x = "right", legend = c("M1", "M2", "M3")))

 Mplot(Mcommon(M1, M2, M3), lwd = 2, mtext = "common variables", 
       legend = list(x = "top", legend = c("M1", "M2", "M3")))

## =======================================================================
## The iris and Orange data set
## =======================================================================

# Split the matrix according to the species
 Irislist <- Msplit(iris, split = "Species")
 names(Irislist)

 Mplot(Irislist, type = "p", 
       legend = list(x = "bottomright", title = "species"))

 Mplot(Msplit(Orange,1), lwd = 2,
       legend = list(x = "topleft", title = "tree nr"))

# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

