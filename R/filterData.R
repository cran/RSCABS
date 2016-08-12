filterData <-
function(effect,Data){
#This function filters problematic columns from the analysis
#effect is column name that is being filtered
#' @export
#First, an already declared variable
ExcludeList<-c(.RSCABSEnv$GenderVar,.RSCABSEnv$GenerationVar,.RSCABSEnv$TreatmentVar,.RSCABSEnv$AgeVar,.RSCABSEnv$ReplicateVar)
if (length(which(ExcludeList==effect))>0){
return(FALSE)
}

Dvec<-convert2Score(Data[ ,effect])
#Second, any Response with nothing but zeros or too many scores
if (max(Dvec,na.rm=TRUE)<1 | max(Dvec,na.rm=TRUE)>8){
return(FALSE)
}

return(TRUE)

}
