prepDataRSCABS <-
function(Effect='',Data={},Treatment='',Replicate=''){
#Data Transform from a list of individuals to matrix format 
#This will take Clustered Data and convert it to by 
#' @export

if(Effect=='&Fill#'){ #ensures the correct data structure output
return()
}
if(length(which(colnames(Data)==Effect))==0){
print(paste(Effect,' is not in data set. Ending function.',sep=''))
return()
}
K.max<-max(Data[ ,Effect],na.rm=TRUE) #max K score


#Remove NA and negative numbers
if (length(which(is.na(Data[ ,Effect])))>0){
Data<-Data[-which(is.na(Data[ ,Effect])), ]
}
if (length(which(Data[ ,Effect]<0))>0){
Data<-Data[-which(Data[ ,Effect]<0), ]
}
#Convert Factors to Numerics
Data[ ,Effect]<-as.numeric(Data[ ,Effect])

if (K.max==0){
print(paste('There is no variation in ',Effect,'. Ending function.',sep=''))
return()
}
#Replicates are rows, Treatment are columns  [Replicate,Treatment]
n.i.j<-xtabs( ~Replicate+Treatment, data=Data)
m.i<-apply(n.i.j,2,function(Vec){
if (length(which(Vec==0))>0){
Vec<-Vec[-which(Vec==0)]
}
return(length(Vec))
})
x.i.j<-array(dim=c(dim(n.i.j)[1],dim(n.i.j)[2],K.max)) #Declare x.i.j , frequency array of scores k or larger 
#Each k level is on the 3rd dimension
for (K in 1:K.max){
x.i.j[ , ,K]<-xtabs( ~Replicate+Treatment, data=Data,subset=Data[ ,Effect]>=K)
}
RSCABS.Prep.Data<-list(x.i.j=x.i.j,n.i.j=n.i.j,m.i=m.i,K.max=K.max)
return(RSCABS.Prep.Data)
}
