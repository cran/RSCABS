\name{convert2Score}
\alias{convert2Score}
\title{
	Converts any object that is not zero or a positive number to NA 
}
\description{
	An internal function for converting any non-whole number in a vector into NA which is excluded from the analysis. 
}
\usage{
convert2Score(Dvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dvec}{
	A vector to be converted. 
}
}

\value{
\item{Dvec}{
	A vector after conversion. 
}
}

\author{
	Joe Swintek
}
