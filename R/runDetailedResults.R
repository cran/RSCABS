runDetailedResults <-
function(Data,GroupVar='',ReplicateVar='',effect=''){

#This is a function to perform RSCABS or Scabs on One effect of the data
#This will be much slower then using RSCABSMain for all effects, but gives more information
#The Output is a Data Structure that need another function to clean and Interpret 
#Data: Data set
#GroupVar: is the group variable;  This is the treatment
#Replicate: is the Replicate  variable
#BoScabs: is a bool to use scabs
#effect is the effect being tested

#This function assumes NA, are not observed scores. Non-Positive numbers and anything else is an observed non-result
#' @export

#First Prep and Check the data 
if (is.numeric((Data[ ,GroupVar]))==FALSE){  #Check to see if Treatment is numeric, this will Ensure to the groups are ordered
popMessage("Error: Treatment needs to be only numbers\nPlease convert treatment to only numbers  \nTerminating RSCABS...\n")
return('No Information')
}


if (is.numeric(Data[,effect])==FALSE){
#Adjust for data structure  
Data[,effect]<-convert2Score(Data[,effect])
}

Kn<-max(Data[ ,effect],na.rm=TRUE)  #Finds the number levels for the Severity of an effect
if (Kn==0){      #effects that have non-positive max Severity scores or a Severity score too large are ignored
popMessage("This effect has no severity scores above Zero\nPlease run with another effect ...\n")
return('No Information')

}


if (length(which(colnames(Data)==ReplicateVar))==0 || ReplicateVar=='' || ReplicateVar==' '){  #Checks for a Replicate Variable
popMessage("No replicate variable is defined. \n More detailed results can not be attained. \n Please Select a replicate variable.\n")
return('No Information')
}



#Use only the columns needed
Data<-Data[ ,c(effect,ReplicateVar,GroupVar)]


#remove NAs

#Pre and test Data------------------------------------------------------------
options(warn=-1)
Data[which(Data[ ,effect]<0) ,effect]<-NA   #negatives are treated as NA
options(warn=0)

if (length(which(is.na(Data[ ,effect])))>0){
Data<-Data[-which(is.na(Data[ ,effect])), ]
}

if  (dim(Data)[1]<=1){
return('No Information')
}
#--------------------------------------------------------------------------------

 
#Initializes summary table  
Out<-{};


#Eliminates the information pertaining to other effects 
Data<-as.data.frame(as.matrix(Data))
Data[ ,effect]<-as.numeric(as.character(Data[ ,effect]))
#Converts GroupVar to a Factor
Data[ ,GroupVar]<-as.factor(Data[ ,GroupVar])  


#Main Loop for each Severity Score
for (i in 1:Kn){

#Reshape the Data
PrepData<-{}
#Find the levels of each effect
TN<-nlevels(Data[ ,GroupVar])        #number of treatments
Tlevel<-levels(Data[ ,GroupVar])     #vector of treatment names
RN<-nlevels(Data[ ,ReplicateVar])    #number of Replicates
Rlevel<-levels(Data[ ,ReplicateVar]) #vector of replicate names

for (j in 1:TN){
Sub1<-subset(Data,Data[ ,GroupVar]==Tlevel[j]) #by group
PrepData[[j]]<-mat.or.vec(2,RN)
for (z in 1:RN){
Sub2<-subset(Sub1,Sub1[ ,ReplicateVar]==Rlevel[z]) #by group
PrepData[[j]][1,z]<-length(which(Sub2[ ,effect]>=i))  #Number greater then or equal to Severity score 
PrepData[[j]][2,z]<-length(Sub2[ ,effect])
}
#remove columns of no observations

}

ChiResults<-{}
#Calculate Heterogeneity Check
for (j in 1:TN){
entry<-rep(0,6)
entry[2]<-Tlevel[j]
entry[3]<-dim(PrepData[[j]])[2]-1
entry[5]<-TN
entry[6]<-'.'

P<-sum(PrepData[[j]][1, ])/sum(PrepData[[j]][2, ])
if (P != 0 & P !=1){
#Calculate Chi Squared test for Heterogeneity
#Rao and Scott 1992
entry[1]<-sum({{PrepData[[j]][1, ]- PrepData[[j]][2, ]*P}^2}/ {PrepData[[j]][2, ]*P*(1-P)},na.rm=TRUE)                        
entry[4]<-round(pchisq(as.numeric(entry[1]),as.numeric(entry[3]),lower.tail = FALSE) ,5) 
entry[1]<-round(as.numeric(entry[1]),2) #Clean up output
ChiResults<-rbind(ChiResults,entry) #adds the entry to the table
}
}
#Change out signif code
if (length(ChiResults)>0){
ChiResults[which(as.numeric(ChiResults[ ,4])<.05),6]<-'*'
ChiResults[which(as.numeric(ChiResults[ ,4])<.01),6]<-'**'
ChiResults[which(as.numeric(ChiResults[ ,4])<.001),6]<-'***'
#Clean up ChiResults
ChiResults<-as.data.frame(as.matrix(ChiResults),row.names=c(1:TN));
colnames(ChiResults)<-c('Value','Treatment','DoseNum','P-value','Doses','Signif')
}


#Prep the data for RSABS
FreqTable<-mat.or.vec(2,TN)
m.i<-rep(0,TN)
for (j in 1:TN){
FreqTable[2,j]<-sum(PrepData[[j]][1, ]) #Number effected 
FreqTable[1,j]<-sum(PrepData[[j]][2, ])-sum(PrepData[[j]][1, ])  #Number not effected

m.i[j]<-sum(PrepData[[j]][2,] != 0)   #Check non-zero totals for each replicate
}

#Convert to RSABS friendly data
x.i.j<-mat.or.vec(dim(PrepData[[1]])[2],TN)
n.i.j<-mat.or.vec(dim(PrepData[[1]])[2],TN)

for (j in 1:TN){
x.i.j[ ,j]<-PrepData[[j]][1, ]
n.i.j[ ,j]<-PrepData[[j]][2, ]
}



#RSABS
#Variable names are the same as in Rao and Scott 1992

Step<-{} #The results from each step down
Test=1  #This will keep stepping down the dose until a significant result is no longer found
count=0;
while (Test>0){

m.i<-as.matrix(m.i)
#This calculates RSCABS
RSCAeffect<-RSCABK(x.i.j,n.i.j,m.i,i,'RS')   #This calculates RSCABS

Row<-cbind(paste(effect,RSCAeffect['R-Score'],sep=''),RSCAeffect$Treatment,RSCAeffect['R-Score'],
RSCAeffect$Statistic,RSCAeffect['P-Value'],RSCAeffect['Signif'])  #A response for the summary table

count<-count+1;
Step[[count]]<-list('FreqTable'=FreqTable,'RSCAB'=Row)
Test=sum(RSCAeffect$Sig!='.')

#Step down
m.i<-m.i[-length(m.i)[1] ]
FreqTable<-FreqTable[  ,-dim(FreqTable)[2]]
x.i.j<-x.i.j[ ,-dim(x.i.j)[2] ]; n.i.j<-n.i.j[ ,-dim(n.i.j)[2]]; 


if (is.matrix(FreqTable)==FALSE){
Test=0   #End step down
}
}

Out[[i]]<-list('ChiResults'=ChiResults,'Step'=Step);

}

return(Out)
}
