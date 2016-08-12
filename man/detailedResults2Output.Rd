\name{detailedResults2Output}
\alias{detailedResults2Output}

\title{
	Print results to the GUI window  
}
\description{
	An internal function that prints results from \code{\link{runDetailedResults}} to the window.   
}
\usage{
detailedResults2Output(Effect, Results, k, ResultsBox, BoolNotebook, Notebook = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{Effect}{
		The tested endpoint. 
}

  \item{Results}{
	A Results data structure from  \code{\link{runDetailedResults}.}
}
  \item{k}{
	The severity score being displayed.
}
  \item{ResultsBox}{
	The name of the box the results are displayed in.
}
  \item{BoolNotebook}{
	A bool indicating to add another tab to the current notebook.
}
  \item{Notebook}{
	The current notebook.
}
}


\author{
	Joe Swintek
}

\seealso{
	\code{\link{runDetailedResults}}, \code{\link{detailedResults2HTML}}
}
