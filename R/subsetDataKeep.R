subsetDataKeep <-
function(Data, Var, Val){
#' @export
#This function will create a subset that excludes all but a value of a variable 
#Var is the variable as a string
#Val is a value as a string (factor) or a number (numeric)

ColNames<-colnames(Data)

#Check to see if both the variable and value are in the data set
if (Var==''){
popMessage("Error no variable selected\nTerminating Subset Routine ...\n")
return (Data)
}

if (Var==''){
popMessage("Error no value selected\nTerminating Subset Routine ...\n")
return (Data)
}

if (length(which(ColNames==Var)) == 0){
popMessage(paste("Error ",Var ," not in data set\nTerminating Subset Routine ...\n",sep=''))
return (Data)
}


if (length(which(Data[ ,Var]==Val)) == 0){
popMessage(paste("Error ",Val," not in ",Var," for current data subset",sep=''))
return (Data)
}

Subset<-subset(Data,Data[ ,Var]==Val);

return(Subset)
}
