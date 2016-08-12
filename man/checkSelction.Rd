\name{checkSelction}
\alias{checkSelction}
\title{
	Checks user inputs
}
\description{
	An internal function that checks to see if a variables necessary for Histopath to run has been defined 
}
\usage{
checkSelction()
}

\details{
	This function relies on the global variables; GenderVar,  GenderVal, GenerationVar, GenerationVal, AgeVar, AgeVal, and TreatmentVar.  
	All of these values are controlled by the GUI.

}
\value{

 \item{msg}{A message to the user displaying all the values and variables that still need to be declared.}


}

\author{
	Joe Swintek
}