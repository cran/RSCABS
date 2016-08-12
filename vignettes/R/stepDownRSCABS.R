stepDownRSCABS <-
function(TestK,x.i.j,n.i.j,m.i,Effect,test.type){
Next<-0; #Step Down test
#' @export
#Replicates are rows, Treatment are columns  [Replicate,Treatment]
Result.K<-as.data.frame({})
x.i.j.K<-x.i.j[ , ,TestK]
while (Next==0){
Result<-RSCABK(x.i.j.K,n.i.j,m.i,TestK,test.type=test.type)

Next<- Result['P-Value']>0.05 | dim(x.i.j.K)[2]==2 #Stop conditions of too high of p.val or 2 treatments
#The step down
x.i.j.K<-x.i.j.K[ ,-dim(x.i.j.K)[2]]
n.i.j<-n.i.j[ ,-dim(n.i.j)[2]]
m.i<-m.i[-length(m.i)]

Result.K<-rbind(Result.K, as.data.frame(Result))
}
Effect<-paste(Effect,TestK,sep='')
Result.K<-cbind(Effect,Result.K)
return (Result.K)
}
