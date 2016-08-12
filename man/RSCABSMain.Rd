\name{RSCABSMain}
\alias{RSCABSMain}
\title{
	Cleans data and runs RSCABS when called from the GUI interface  
}
\description{
	This is an internal function called though the GUI to prepare data for input into \code{\link{runRSCABS}}.
}
\usage{
RSCABSMain(Data, GroupVar = "", ReplicateVar = "", BoScabs = FALSE)
}

\arguments{
  \item{Data}{
	A standard data set in the tall format.  Every row indicates an organism. The data set must contain columns for the treatment level and every tested
	histological endpoint.	
}
  \item{GroupVar}{
	The name of the column that contain the information about the treatment level.  Increasing values indicate higher treatments.    
}
  \item{ReplicateVar}{
	The name of the column that contain the information about the replicate structure. This is optional, if this is not declared at function call, BoScabs will be set to FALSE. 
}
  \item{BoScabs}{
	Indicate the type of analysis to be performed.  Use FALSE to select the Rao-Scott adjustment to the Cochran-Armitage test and TRUE to ignore the adjustment.  
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
		The test statistic corresponding that row's endpoint treatment level, and R-Score.  
	}
	\item{P-Value}{
		The corresponding p-value
	}
	\item{Signif}{
		The significance flag with "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.
	}
}


\author{
	Joe Swintek 
}
