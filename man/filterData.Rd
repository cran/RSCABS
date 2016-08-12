\name{filterData}
\alias{filterData}
\title{
	Filters problematic data 
}
\description{
	An internal function called by OtherPath to filter out problematic endpoints for calculating detailed responses. 
}
\usage{
filterData(effect, Data)
}

\arguments{
  \item{effect}{
	The endpoint under examination 
}
  \item{Data}{
	Data from Histopath
}
}
\details{
	Filter out potential endpoints that do not have a score above 0 or contain values above 20.  This will also exclude anything flagged as an identification variable.   
}
\value{

\item{Bool}{Returns a TRUE or FALSE}


}
\author{
	Joe Swintek
}