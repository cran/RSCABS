detailedResults2Output <-
function(Effect,Results,k,ResultsBox,BoolNotebook,Notebook=NULL){
#This is built into the GUI and only output to the GUI
#k is the current Severity score
#ResultsBox this is the box in the gui 
#Notebook is the current notebook in the gui
#' @export
Result<-Results[[k]]

#Extract from the results structure 

#ChiTable
ChiTable<-Result[['ChiResults']]


Nstep<-length(Result$Step)

#Frequency Table 
Freqtable<-Result[['Step']][[1]][['FreqTable']]
Freqtable<-cbind(Freqtable,rowSums(Freqtable))
Freqtable<-rbind(Freqtable,colSums(Freqtable))

Freqtable<-cbind(c(paste('<',k,sep=''),paste('>=',k,sep='')  ,'Total'),Freqtable)
Freqtable<-as.data.frame(Freqtable)
colnames(Freqtable)<-c('Score/Treatment',1:{dim(Freqtable)[2]-2},'Total')



#RSCAB Table 
RSCABTable<-{}
for (j in 1:Nstep){
RSCABTable<-rbind(RSCABTable,Result$Step[[j]]$RSCAB)
}

#Cleans the Table
colnames(RSCABTable)<-c('Response','Treatment','R-Score','Statistic','P-Value','Signif')
rownames(RSCABTable)<-NULL
RSCABTable<-as.data.frame(RSCABTable)
for (j in 1:6){
RSCABTable[ ,j]<-unlist(RSCABTable[ ,j])
}
RSCABTable<-RSCABTable[ ,-1]
RSCABTable[ ,3]<-round(as.numeric(as.character(RSCABTable[ ,3])),5)
RSCABTable[ ,4]<-round(as.numeric(as.character(RSCABTable[ ,4])),5)

if (length(which(RSCABTable[ ,4]=='NaN'))>0){
RSCABTable<-RSCABTable[-which(RSCABTable[ ,4]=='NaN'), ]
}

if (dim(RSCABTable)[1]==0){
RSCABTable<-NULL
}
#This OutPuts to the GUI, on the first tab
if (BoolNotebook==FALSE){
Frame1<-gframe(horizontal = FALSE, container=ResultsBox) #Chi-Squared Table 
Lab1<-glabel("Chi-Squared Heterogeneity Check of Between Replicate Variances",container=Frame1)
if (is.null(ChiTable) != TRUE){
.RSCABSEnv$DataGrid1<-gtable(ChiTable,container=ResultsBox,expand=TRUE)
}else{
LabChi<-glabel("There is no change in treatment for this R score",container=ResultsBox)
}

Frame2<-gframe(horizontal = FALSE, container=ResultsBox) #Frequency Table
Lab2<-glabel("Frequency Table",container=Frame2)
if (is.null(Freqtable) != TRUE){
.RSCABSEnv$DataGrid2<-gtable(Freqtable,container=ResultsBox,expand=TRUE)
}else{
LabFreq<-glabel("There is no Data",container=ResultsBox)
}
Frame3<-gframe(horizontal = FALSE, container=ResultsBox)  #RSCABS Table
Lab3<-glabel("Rao-Scott Cochran-Armitage Test",container=Frame3)
if (is.null(RSCABTable) != TRUE){

.RSCABSEnv$DataGrid3<-gtable(RSCABTable,container=ResultsBox,expand=TRUE)
.RSCABSEnv$DataGrid3<-gtable(RSCABTable,container=ResultsBox,expand=TRUE)
}else{
LabRSCAB<-glabel("There is no change in treatment for this R score",container=ResultsBox)
}
return()
}

#This Outputs to the GUI on a new tab
NewGroup <- ggroup(horizontal = FALSE)

Frame1<-gframe(horizontal = FALSE, container=NewGroup) #Chi-Squared Table 
Lab1<-glabel("Chi-Squared Heterogeneity Check of Between-Replicate Variances",container=Frame1)
if (is.null(ChiTable) != TRUE){
.RSCABSEnv$DataGrid1<-gtable(ChiTable,container=NewGroup,expand=TRUE)
}else{
LabChi<-glabel("There is no change in treatment for this R score",container=NewGroup)
}
Frame2<-gframe(horizontal = FALSE, container=NewGroup) #Frequency Table
Lab2<-glabel("Frequency Table",container=Frame2)
if (is.null(Freqtable) != TRUE){
.RSCABSEnv$DataGrid2<-gtable(Freqtable,container=NewGroup,expand=TRUE)
}else{
LabFreq<-glabel("They is no Data",container=NewGroup)
}
Frame3<-gframe(horizontal = FALSE, container=NewGroup)  #RSCABS Table
Lab3<-glabel("Rao-Scott Cochran-Armitage Test",container=Frame3)
if (is.null(RSCABTable) != TRUE){
.RSCABSEnv$DataGrid3<-gtable(RSCABTable,container=NewGroup,expand=TRUE)
}else{
LabRSCAB<-glabel("There is no change in treatment for this R score",container=NewGroup)
}

add(Notebook,NewGroup, label=paste(Effect,k,sep='P'))

return()
}
