\name{RSCABK}
\alias{RSCABK}

\title{
	Runs the kth slice of RSCABS 
}
\description{
	An internal function that calculates the RSCABS or SCABS test statistic with the associated p-value. 
}
\usage{
RSCABK(x.i.j, n.i.j, m.i, TestK, test.type)
}
\arguments{
  \item{x.i.j}{
	matrix containing the number of observed "successes" for replicate i on treatment j. 
}
  \item{n.i.j}{
	matrix containing the number of observations for replicate i on treatment j. 
}
  \item{m.i}{
	matrix of number units in each treatment/replicate combination.
}
  \item{TestK}{
	The kth severity score being tested.
}
  \item{test.type}{
	Indicate the type  of analysis to be performed.  Use "RS" to select the Rao-Scott adjustment to the Cochran-Armitage test and "CA" to ignore the adjustment.
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
		The test statistic corresponding to that row's endpoint treatment level and R-Score.  
	}
	\item{P-Value}{
		The corresponding p-value
	}
	\item{Signif}{
		The significance flag where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.
	}
}

\author{
	Joe Swintek
}

