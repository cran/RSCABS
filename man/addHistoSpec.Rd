\name{addHistoSpec}
\alias{addHistoSpec}
\title{
	Specify data parameters
}
\description{
	Calls a tab where you can specify the names of the variables used in Histopath. 
}
\usage{
addHistoSpec(Notebook)
}

\arguments{
  \item{Notebook}{
		The notebook the tab will be added to.
	}
}

\details{
	\itemize{
  \item{\bold{Select Gender Variable:}}{
		Button used to specify the name of the gender variable.
	}
  \item{\bold{Select Gender Value:}}{
		Button used to specify the value of the gender variable.
	}
  \item{\bold{Select Generation Variable:}}{
	  Button used to specify the name of the generation variable.
	}
  \item{\bold{Select Generation Value:}}{
	Button used to specify the value of the generation variable.
	}
  \item{\bold{Select Age Variable:}}{
	  Button used to specify the name of the age variable.
	}
  \item{\bold{Select Age Value:}}{
	 Button used to specify the value of the age variable.
	}
  \item{\bold{Select Treatment Variable:}}{
		Button used to specify the name of the treatment variable.
	}
  \item{\bold{Select Replicate Variable:}}{
	Button used to specify the name of the replicate variable.
	}
  \item{\bold{Confirm Selected Values and Variables:}}{
	Press this button when all the values in the data specification form have been selected.  Only Treatment must be selected to proceed with the analysis.  
	If all values are selected a pop up message will appear directing you to the main tab. 
	}
	}
}

\author{
	Joe Swintek
}



\seealso{
	\code{\link{runRSCABS}}, \code{\link{otherPath}}, \code{\link{Histopath}}
}


\keyword{RSCABS}
