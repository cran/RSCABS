\name{selectPara}
\alias{selectPara}

\title{
	Window to declare the names of variables and values
}
\description{
	This internal function brings up a window to declare a variable or value.  The function then assigns the names in global variables 
	and updates the corresponding labels on the OtherPath window.   
}
\usage{
selectPara(VarName,LabelName=NULL,Enviro,What=NULL,Mult=FALSE,Display=NULL)
}
\arguments{
  \item{VarName}{
	The name (as a string) of the variable or value to be specified in the data set.
}
  \item{LabelName}{
	The name (as a string) of the label that will be updated.  The default is to not update a label.
}
  \item{What}{
	A vector that can can be selected from.
}
  \item{Enviro}{
	The name (as a string) of the environment that containing the variables and labels. 
}
  \item{Mult}{
	Bool for determining if multiple values can be selected. 
}
  \item{Display}{
	Indicates what is displayed in the window title.
}


}


\author{
	Joe Swintek
}
