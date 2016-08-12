\name{otherPath}
\alias{otherPath}
\title{
	Other analyses for histopathological endpoints
}
\description{
	This window is used to calculate the Cochran-Armitage trend test by slices(SCABS), the chi-squared test for homogeneity, and a frequency table for each end point.  
}
\usage{
	otherPath()
}


\details{
\itemize{ 
	\item{\bold{Run SCABS Button:}}{Run SCABS on all end points.}
	\item{\bold{Get Details on a Response Button:}}{Attains values for; the chi-squared test for homogeneity, frequency tables, and a RSCABS analysis for a single endpoint.}
	\item{\bold{Get Details on all Responses Button:}}{Attains values for; the chi-squared test for homogeneity, frequency tables, and a RSCABS analysis for all endpoints. This will prompt the creation of a new directory and fill that directly with html files containing tables for each endpoint. }
	\item{\bold{Save Button:}}{Saves the results from the last analysis ran. SCABS analyses are saved as csv files, while detailed on a Responses are saved as html files. }
}

}


\author{
	Joe Swintek
}

\seealso{
	\code{\link{runRSCABS}}, \code{\link{addHistoSpec}}, \code{\link{Histopath}}
}

\keyword{RSCABS}