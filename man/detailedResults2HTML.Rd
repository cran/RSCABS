\name{detailedResults2HTML}
\alias{detailedResults2HTML}


\title{
	Saves results as an HTML file
}
\description{
	An internal function that prints results from \code{\link{runDetailedResults}} to  an HTML file.   
}
\usage{
detailedResults2HTML(Results, k, Dir, Effect,Inverse=FALSE)
}

\arguments{
  \item{Results}{
	A Results data structure from  \code{\link{runDetailedResults}}.
}
  \item{k}{
	The severity score being displayed.
}
  \item{Dir}{
	The name of the new directory to be created.
}
  \item{Effect}{
	The name of the endpoint to be saved.
}
  \item{Inverse}{
	Bool indicating an inverse scale endpoint.
}

}



\author{
	Joe Swintek
}
\note{
	This function will end up creating a new folder.
}

\seealso{
	\code{\link{runDetailedResults}}, \code{\link{detailedResults2Output}}
}
