runStdSubset<-function(Data){
#' @export
	#This will subset the data for the analysis 
	if (identical(.RSCABSEnv$GenderVar,'Not Used')==FALSE){
		Data<-subsetDataKeep(Data,.RSCABSEnv$GenderVar,.RSCABSEnv$GenderVal);
	}	
	if (identical(.RSCABSEnv$GenerationVar,'Not Used')==FALSE){
		Data<-subsetDataKeep(Data,.RSCABSEnv$GenerationVar,.RSCABSEnv$GenerationVal);
	}
	if (identical(.RSCABSEnv$AgeVar,'Not Used')==FALSE){
		Data<-subsetDataKeep(Data,.RSCABSEnv$AgeVar,.RSCABSEnv$AgeVal);
	}
	return(Data)
}	