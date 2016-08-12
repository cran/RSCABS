\name{plotRSCABS}
\alias{plotRSCABS}
	
\title{
	Plots histopathological data.
}
\description{
	Plots histopathological data used in the RSCABS analysis as stacked bar graphs.  
}
\usage{
plotRSCABS(Data, Effect, Treatment, Metric = "Percent", Lowest = "Remove", 
	PlotParms = NULL, Format = NULL, File = NULL,...)
}

\arguments{
  \item{Data}{
	Tall formatted data used in \code{\link{Histopath}} and \code{\link{runRSCABS}}.
}
  \item{Effect}{
	The name of endpoint being plotted.
}
  \item{Treatment}{
	The name of the treatment variable. 
}
  \item{Metric}{
	The value being plotted, either "Total" or as a "Percent".
}
  \item{Lowest}{
	Determines whether to "Include" or "Remove" the lowest response. The lowest response should correspond to no effect.    
}

  \item{PlotParms}{
	A list of plotting parameters, see \code{\link{setPlotParms}}.
}
  \item{Format}{
	The name (as a string) of the function that saves the plot to a file.  Use NULL to print the plot to the screen. 
}
  \item{File}{
	The name of the file the plot is saved to.
}
  \item{...}{
	Other arguments to be passed to \code{\link{barplot}}. 
}

}

\author{
	Joe Swintek
}

\keyword{Plotting}