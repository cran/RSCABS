\name{exampleHistData}
\alias{exampleHistData}
\docType{data}
\title{
	Example histology data
}
\description{
	Example Histology data included in StatCharrms and RSCABS.  
}
\usage{data(exampleHistData)}
\format{
  A test data set for StatCharrms and RSCABS that contains all the necessary identifiers along with 51 histopathological endpoints. 
  \describe{
    \item{\code{Generation}}{The generation, with three levels F0, F1, and F2.}	
    \item{\code{Treatment}}{The treatment variable, with 6 levels, where 1 represents the controls.  }		
    \item{\code{Replicate}}{The replicate variable. Observation within the same generation, treatment, and replicate value belong to the same replicate.}
    \item{\code{Genotypic_Sex}}{The genotypic sex either Female or Male.}
    \item{\code{Age}}{The age ether 16_wk or 8_wk.} 
 }
}

\source{
	US EPA Duluth MED Lab 
}
\references{
	TBA
}
\examples{
	data(exampleHistData)
	str(exampleHistData)
}
\keyword{datasets}


