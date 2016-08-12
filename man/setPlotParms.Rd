\name{setPlotParms}
\alias{setPlotParms}
	
\title{
	Sets the default plotting parameters for RSCABS
}
\description{
	An internal function for setting the plotting parameters not defined by the prior to function call. This is called by \code{\link{plotRSCABS}}. 
}
\usage{
setPlotParms(PlotParms, Out, Effect, Metric, Lowest,...)
}

\arguments{
  \item{PlotParms}{
	A list that contains values for the plotting parameters for \code{\link{barplot}}.
}
  \item{Out}{
	A matrix of values that will be plotted.
}
  \item{Effect}{
	The name of the endpoint being plotted.
}
  \item{Metric}{
	The value being plotted and be "Total" or "Percent".
}
  \item{Lowest}{
	Determines whether to "Include" or "Remove" the lowest response. The lowest response should correspond to no effect.    
}
  \item{...}{
	Other arguments to be passed to \code{\link{barplot}}. 
}
}

\value{
The values of PlotParms have the same name as the plotting parameters of \code{\link{barplot}}.
	\item{xlab}{The label for the x axis, defaults to 'Treatment Group'.}
	\item{ylab}{The label for the y axis, defaults to the value of \code{Metric}. }
	\item{main}{The title.}
	\item{legend.text}{The legend text defaults to name of the rows of Out.}
	\item{args.legend}{Extra parameters to be passed to \code{\link{legend}}. Defaults to placing the legend in the right margin.}
	\item{ColorFunction}{The code pallet used to color the bar plots.  Defaults to using \code{\link{heat.colors}}.}
	\item{Colors}{The colors used in the bar plot.  Defaults to using the pallet determined by \code{ColorFunction}. }
}

\author{
	Joe Swintek
}
\note{
	This function only changes item in PlotParms that have a NULL value.
}



\seealso{
	\code{\link{plotRSCABS}}
}
\keyword{Plotting}