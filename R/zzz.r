.RSCABSEnv <- new.env()
Temp  <- new.env()



.onLoad<-function(...){
#' @import gWidgets
#' @import RGtk2
#' @import R2HTML
#' @import gWidgetsRGtk2	
#' @importFrom grDevices dev.off heat.colors tiff pdf bmp jpeg png gray.colors rainbow terrain.colors topo.colors cm.colors
#' @importFrom graphics barplot par
#' @importFrom methods is 
#' @importFrom stats pchisq pnorm xtabs
#' @importFrom utils read.csv write.table
 
 


}



.onAttach <- function(...) {
	packageStartupMessage('Type Histopath() to begin.')
}