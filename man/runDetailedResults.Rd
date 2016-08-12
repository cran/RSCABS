\name{runDetailedResults}
\alias{runDetailedResults}

\title{
	Attain detailed results on an endpoint
}
\description{
	An internal function called by OtherPath to attain detailed results on an endpoint.  This function; performs a chi-squared test for homogeneity, 
	calculates a frequency table, and recalculates RSCABS.	
}
\usage{
runDetailedResults(Data, GroupVar = "", ReplicateVar = "", effect = "")
}
\arguments{
  \item{Data}{
	Tall formatted data from Histopath.
}
  \item{GroupVar}{
	The name of the treatment variable.
}
  \item{ReplicateVar}{
	The name of the replicate variable.
}
  \item{effect}{
	The effect being tested.
}
}


\value{
	Returns a list containing: 
	 \item{Numeric}{The top level of the list is a numeric key associated with the severity score.}
    \item{ChiResults}{Chi square results.}
    \item{Step$Row}{The results from the RSCABS analysis.}
	\item{Step$FreqTable}{The frequency table associated with each treatment level.}
}

\author{
	Joe Swintek
}

\seealso{
	\code{\link{detailedResults2HTML}}, \code{\link{detailedResults2Output}}
}

