convert2Score <-
function(Dvec){
#This Function will convert any object that is not zero or a positive number to NA 
#' @export
oldw <- getOption("warn")
options(warn=-1)  #Turn off warnings
Dvec<-as.numeric(as.character(Dvec))  #Will induce a a warning 
Dvec[which(Dvec<0)]<-NA
options(warn=oldw) #Turn on warnings
return(Dvec)
}
