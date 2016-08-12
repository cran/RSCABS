\name{Histopath}
\alias{Histopath}
\title{
	GUI for analysis of histopathological data
}
\description{
	This is the function call to start the GUI interface for StatCharrms
}
\usage{
	Histopath()
}

\details{
\itemize{ 
	\item{\bold{Load Data Button:}}{
		A standard data set in the "tall" format.  Every row indicates an organism. The data set must contain columns for the treatment level and every tested
		histological endpoint.  The data must be in .csv to load it into Histopath. 	
	}
	\item{\bold{Specify Data Button:}}{
		This is used to tell Histopath the names of the columns used to specify the data.  Pushing the button bring up a new tab that needs to be navigated to. 
		See \code{\link{addHistoSpec}} for details.	
	}
	\item{\bold{Run RSCABS Button:}}{
		Pressing this will perform the RSCABS analysis.  The results will replace the current data in the display box.      	
	}
	\item{\bold{Run Other Analyses Button:}}{
		Pressing this will perform the bring up a new window with the ability to perform other relevant analyses.  See \code{\link{otherPath}} for details.   	
	}
	\item{\bold{Save Result Button:}}{
		Used to save the results of the RSCABS analysis. 	
	}	
	}
}


\value{
	Returns a list with the following values:
	\item{Response}{
		The endpoint that is being tested.
	}
	\item{Treatment}{
		The treatment level.
	}
	\item{R-Score}{
		The severity score from the histology.
	}
	\item{Statistic}{
		The test statistic corresponding that row's endpoint treatment level, and RSCORE.  
	}
	\item{P-Value}{
		The corresponding p-value.
	}
	\item{Signif}{
		The significance flag where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.
	}
}




\references{
	Green, John W., Springer, Timothy A., Saulnier, Amy N., and Swintek, Joe (2014) Statistical analysis of histopathological endpoints. 
	Environmental Toxicology and Chemistry, 33(5), 1108-1116
}
\author{
	Joe Swintek
}
\note{
	See the vignette for a full tutorial on how to use this function. 	
}
\seealso{
	\code{\link{runRSCABS}}, \code{\link{otherPath}}, \code{\link{addHistoSpec}}
}
\keyword{RSCABS}
