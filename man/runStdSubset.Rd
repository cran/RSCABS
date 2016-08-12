\name{runStdSubset}
\alias{runStdSubset}
\title{
	Subset the data used in Histopath 
}
\description{
	An internal function that that will subset data for Histopath based on the global variables; GenderVar,  GenderVal, GenerationVar, GenerationVal, AgeVar, and AgeVal. 
}
\usage{
runStdSubset(Data)
}

\arguments{
  \item{Data}{
    Data that has been formatted for use in Histopath.  
 }
}

\details{
	This function relies on the global variables; GenderVar,  GenderVal, GenerationVar, GenerationVal, AgeVar, and AgeVal.  
	All of these values are controlled through GUI.
}
\value{
	\item{UseData }{Sub-setted data that will be used in the RSCABS calculation. }
}

\author{
	Joe Swintek
}

