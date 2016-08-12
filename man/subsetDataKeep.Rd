\name{subsetDataKeep}
\alias{subsetDataKeep}

\title{
	Subsets data
}
\description{
	An internal function used to subset data and sanitize inputs. 
}
\usage{
subsetDataKeep(Data, Var, Val)
}

\arguments{
  \item{Data}{
	Tall formatted data used in Histopath.
}
  \item{Var}{
	The variable used for sub-setting.
}
  \item{Val}{
	The value of the  variable being sub-setted on.
}
}


\value{
	\item{Subset}{Post subset data}
}
