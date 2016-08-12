\name{stepKRSCABS}
\alias{stepKRSCABS}

\title{
	Steps through the severity score for a given effect
}
\description{
	An internal function for stepping through each severity score of an endpoint. 
}
\usage{
	stepKRSCABS(Effect, Data.Prep, Treatment, Replicate, test.type)
}

\arguments{
  \item{Effect}{
	Endpoint being tested.
}
  \item{Data.Prep}{
		Data prepared by prep.data.RSCABS.
}
  \item{Treatment}{
	Name of the treatment variable. 
}
  \item{Replicate}{
	Name of the replicate variable. 
}
  \item{test.type}{
	Indicate the type of analysis to be performed.  Use "RS" to select the Rao-Scott adjustment to the Cochran-Armitage test and "CA" to ignore the adjustment.
}
}

\value{
 \item{Results.Effect}{ An intermediary step for results. }
}

\author{
	Joe Swintek
}
