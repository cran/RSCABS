RSCABSMain <-
function(Data,GroupVar='',ReplicateVar='',BoScabs=FALSE){
#This is a function to perform RSCABS or Scabs on the Data
#Data: Data set
#GroupVar: is the group variable;  This is the treatment
#Replicate: is the Replicate  variable
#BoScabs: is a bool to use scabs
#Will treat every other variable as a responce variable
#Returns a list of RSCABS analyses
#' @export

#This function assumes NA, are not observed scores. Non-Positive numbers and anything else is an observed non-result

#First Prep the data
ColNames<-colnames(Data)
#The variables need to be reassigned to being the colnumber


if (is.numeric((Data[ ,GroupVar]))==FALSE){  #Check to see if Treatment is numeric, this will Ensure to the groups are ordered
popMessage("Error: Treatment needs to be only numbers\nPlease convert treatment to only numbers  \nTerminating RSCABS...\n")
return()
}

#Converts GroupVar and ReplicateVar to a Factor
Data[ ,GroupVar]<-as.factor(Data[ ,GroupVar])  
if (length(which(colnames(Data)==ReplicateVar)) >0){
Data[ ,ReplicateVar]<-as.factor(Data[ ,ReplicateVar]) 
}else{
BoScabs<-TRUE
}

#Check to make sure ReplicateVar is in the data set and has 
if (identical(ReplicateVar,' ')==TRUE || identical(ReplicateVar,' ') ==TRUE || identical(ReplicateVar,'Not Used')==TRUE ){
BoScabs<-TRUE
}


#Initializes summary table  
Out<-{};

#this follows a procedure outlined in "Statistical Analysis of Histopathology Endpoints" by John W. Green @DuPont et al..
if (BoScabs == FALSE){
test.type='RS'
}
if (BoScabs == TRUE){
test.type='CA'
}

Out<-runRSCABS(Data,GroupVar,ReplicateVar,Effects='',test.type)

#Cleans the outputs to easy readability 
Out<-as.data.frame(Out);
colnames(Out)<-c('Response','Treatment','R-Score','Statistic','P-Value','Signif')
Out[ ,'Statistic']<-round(Out[ ,'Statistic'], digits=5)
Out[ ,'P-Value']<-round(Out[ ,'P-Value'], digits=5)
return(Out)
}
