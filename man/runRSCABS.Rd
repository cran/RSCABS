\name{runRSCABS}
\alias{runRSCABS}

\title{
	Runs RSCABS  
}
\description{
	Runs the Rao-Scott adjusted Cochran-Armitage trend test by slices (RSCABS) analysis.    
}
\usage{
	runRSCABS(Data, Treatment, Replicate='', Effects = '', test.type = "RS")
}

\arguments{
  \item{Data}{
	A standard data set in the tall format.  Every row indicates an organism. The data set must contain columns for the treatment level and every tested
	histological endpoint.	
}
  \item{Treatment}{
	The name of the column that contains the information about the treatment level.  Increasing values indicate higher treatments.   
}
  \item{Replicate}{
	The name of the column that contains the information about the replicate structure.  If the  replicate is not specified this will default to running "CA" as the test type.   
}
  \item{Effects}{
	The endpoint to be tested.  Defaults to all columns that have integers less then 20.  The analysis assumes that higher scores indicate a worse outcome. 
}
  \item{test.type}{
	Indicate the type of analysis to be performed.  Use "RS" to select the Rao-Scott adjustment to the Cochran-Armitage test and "CA" to ignore the adjustment.  
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
		The test statistic corresponding to that row's endpoint treatment level, and R-Score.  
	}
	\item{P-Value}{
		The corresponding p-value
	}
	\item{Signif}{
		The significance flag where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.
	}
}

\references{
	Green, John W. and Springer, Timothy A. and Saulnier, Amy N. and Swintek, Joe, (2014) Statistical analysis of histopathological endpoints. 
	Environmental Toxicology and Chemistry, 33(5), 1108-1116
}

\author{
	Joe Swintek
}
\note{
	The outputted data frame will not show results for endpoints that did not meet selection criteria as determined by \code{\link{checkSelction}}. Also note that RSCABS is a step down procedure so analysis     
 for an effect at an RSCORE will stop when a non-significant result is found. 	
}


\examples{
	\dontrun{
## Not run: 
#Take the subset corresponding to F0-females of 16 weeks of age
data(exampleHistData)
exampleHistData.sub<-exampleHistData[which(exampleHistData$Generation=='F2' & 
	exampleHistData$Genotypic_Sex=='Female' & exampleHistData$Age=='16_wk' ),  ]
#Run RSCABS	
	exampleResults<-runRSCABS(exampleHistData.sub,'Treatment',
		'Replicate',test.type='RS')
	}
}
\keyword{RSCABS}





