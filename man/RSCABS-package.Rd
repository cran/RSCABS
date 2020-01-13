\name{RSCABS-package}
\alias{RSCABS-package}
\alias{RSCABS}
\docType{package}
\title{
Runs the Rao-Scott adjusted Cochran-Armitage trend test by slices (RSCABS) analysis  
}
\description{
This package contains all of the functions necessary to run the RSCABS analysis through the GUI or command line on histopathological data.            
}
\details{
\itemize{
\item{\bold{Package:}}{RSCABS}
\item{\bold{Type:}}{  Package}
\item{\bold{Version:}}{  0.01}
\item{\bold{Date:}}{   2016-04-21}
\item{\bold{License:}}{   CC0 }
}
}
\author{
Joe Swintek <swintek.joe@epa.gov> as author and Kevin Flynn as a tester. 



Maintainer: Joe Swintek <swintek.joe@epa.gov>
}


\note{
The command line version uses \code{\link{runRSCABS}} (see example below). Use the command \code{\link{Histopath}} to call the GUI version.
}


\references{
	Green, John W. and Springer, Timothy A. and Saulnier, Amy N. and Swintek, Joe, (2014) Statistical analysis of histopathological endpoints. 
	Environmental Toxicology and Chemistry, 33(5), 1108-1116
}

\keyword{RSCABS}

\examples{
## Not run: 
#Take the subset corresponding to F0-females of 16 weeks of age
\dontrun{
data(exampleHistData)
exampleHistData.sub<-exampleHistData[which(exampleHistData$Generation=='F2' & 
	exampleHistData$Genotypic_Sex=='Female' & exampleHistData$Age=='16_wk' ),  ]
#Run RSCABS	
exampleResults<-runRSCABS(exampleHistData.sub,'Treatment','Replicate',test.type='RS')
	}
	}





