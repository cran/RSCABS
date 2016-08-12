checkPlot<-function(){
#Check to see is there is a problem with plotting
#this can only be called through the gui
#' @export

	DVec<-convert2Score(.RSCABSEnv$UseData[[.RSCABSEnv$Response2Graph]])
	if (length(which(is.na(DVec)==FALSE))==0){
		return('Error! There is no data for this endpoint')
	}		
	if (sum(DVec,na.rm=TRUE)==0 && identical(.RSCABSEnv$LowestGraph,'Remove')==TRUE){
		return('Warning! There is no response for this endpoint\n Included the lowest value to see the graph.')
	}	
return()	
}
