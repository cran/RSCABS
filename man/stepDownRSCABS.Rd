\name{stepDownRSCABS}
\alias{stepDownRSCABS}

\title{
	Performs the step down aspect of RSCABS
}
\description{
	and internal function that performs the step down aspect of RSCABS.
}
\usage{
	stepDownRSCABS(TestK, x.i.j, n.i.j, m.i, Effect, test.type)
}

\arguments{
  \item{TestK}{
		The severity score being tested 
}
  \item{x.i.j}{
	Matrix containing the number of observed "successes" for replicate i on treatment j. 
}
  \item{n.i.j}{
	Matrix containing the number of observations for replicate i on treatment j. 
}
  \item{m.i}{
	Matrix of number units in each treatment/replicate combination.
}
  \item{Effect}{
	The the end point to be tested.  
}
  \item{test.type}{
	Indicate the type  of analysis to be performed.  Use "RS" to select the Rao-Scott adjustment to the Cochran-Armitage test and "CA" to ignore the adjustment.  
}
}

\value{
 \item{Result.K}{An intermediary result.}
}

\author{
	Joe Swintek
}
