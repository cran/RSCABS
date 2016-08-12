runRSCABS <-
function(Data,Treatment,Replicate='',Effects='',test.type='RS'){
#This function will produce a table of step-down Cochran-Armitage trend tests with possible Rao-Scott adjustment by slices
#It will Run the test on every effect in the Effect list 
#' @export
#Turn Replicate and Treatment into factors
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
if ( identical(Replicate,'') == FALSE && identical(test.type,'CA')==FALSE){
	Data[ ,Replicate]<-as.factor(Data[ ,Replicate])
}else{
	test.type='CA'
}


if (test.type != 'RS' & test.type != 'CA'){
message('Error: Invalid test type')
return()
}
#Remove all non-whole numbers


#Default for effect every column name that is not a Treatment, Replicate, and has a 0 < K.max < 20
#turn off warnings
options(warn=-1)
if (Effects ==''){
Effects<-colnames(Data)
Maxes<-apply(Data,2,max,na.rm=TRUE)
Remove<-which(as.numeric(Maxes) <= 0 | as.numeric(Maxes) >  20 | is.na(as.numeric(Maxes))==TRUE)
Remove<-c(Remove,which(Effects==Treatment),which(Effects==Replicate))
Remove<-c(Remove,which(is.factor(Data[ ,Effects])))
Remove<-unique(Remove)
if (length(Remove)>0){
Effects<-Effects[-Remove]
}
}

Data[ ,Effects]<-apply(Data[ ,Effects],2,convert2Score)
options(warn=0) #turn on warnings


#Need to remove factors
#Prep Data
Data.Prep<-sapply(c('&Fill#',Effects),prepDataRSCABS,Data=Data,Treatment=Treatment,Replicate=Replicate)
Results.Raw<-sapply(c('&Fill#',Effects),stepKRSCABS,Data=Data.Prep,Treatment=Treatment,Replicate=Replicate,test.type=test.type)
Results<-do.call("rbind", lapply(Results.Raw, data.frame, stringsAsFactors = FALSE))

if (length(which(is.finite(Results[  ,'Statistic'])==FALSE))>0){
Results<-Results[-which(is.finite(Results[  ,'Statistic'])==FALSE), ]
}
rownames(Results)<-{}

return(Results)
}
