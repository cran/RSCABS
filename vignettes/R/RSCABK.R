RSCABK <-
function(x.i.j,n.i.j,m.i,TestK,test.type){
#This function is called for the detailed results functions 
#This is just for 1 slice 
#' @export
K=1
#Variable names are the same as in Rao and Scott 1992

x.i<-colSums(x.i.j)  #Number of successful observations 
n.i<-colSums(n.i.j)  #Total Number in a treatment 

p.i.hat=x.i/n.i


#Calculate r.ij.sum.sq
#this uses the expanded form of sigma(r.ij) squared
r.ij.sum.sq<-mat.or.vec(length(m.i)[1],K)

p.i.hat.k<-apply(t(p.i.hat),2,rep,dim(n.i.j)[1])  #p.hat 

if(test.type =='CA'){
d.i<-1  #No adjustment
}
if(test.type =='RS'){
#Calculate the Rao-Scott adjustment
x.ij.sum.sq<-colSums(x.i.j^2)
cross<- -2*colSums(x.i.j*n.i.j*p.i.hat.k)
last.sum.sq<-colSums((n.i.j*p.i.hat.k)^2)
r.ij.sum.sq<-x.ij.sum.sq+cross+last.sum.sq;

v.i <- m.i/(m.i - 1)/n.i^2 *r.ij.sum.sq
d.i <- n.i * v.i/(p.i.hat * (1 - p.i.hat))



d.i[which(is.na(d.i))]<-1    #Ignores the correction when incidence is 0% or 100%
d.i[which(is.nan(d.i))]<-1   #Ignores the correction when incidence is 0% or 100%
d.i[which(d.i<1)]<-1         #Cf Rao and Scott Clustered Poisson paper;  
}

#apply adjustment
x.i.new <- x.i/d.i
    n.i.new <- n.i/d.i

scores<-matrix(1:length(x.i.new)-1)
p.hat <- sum(x.i.new)/sum(n.i.new)
mean.score <- sum(scores * n.i.new)/sum(n.i.new)
var.scores <- sum(n.i.new * (scores - mean.score)^2)

RS <- (sum(x.i.new * scores) - p.hat * sum(n.i.new * scores))/sqrt(p.hat * (1 - p.hat) * var.scores) #the statistic value 
#P-Value
p.val <- pnorm(abs(RS), lower.tail = TRUE)  
p.val<-1-p.val
if (is.finite(p.val)==FALSE){
p.val<-1
}
#Adds marks for significance 
Sig<-rep('.',length(p.val))
Sig[which(p.val<=.05)]<-'*'
Sig[which(p.val<=.01)]<-'**'
Sig[which(p.val<=.001)]<-'***'

return (list('Treatment'=max(scores)+1,'R-Score'=TestK,'Statistic' = RS, 'P-Value' = p.val,'Signif'=Sig))
}
