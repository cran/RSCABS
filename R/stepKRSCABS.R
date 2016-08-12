stepKRSCABS <-
function(Effect,Data.Prep,Treatment,Replicate,test.type){
#This iterates though each k-level
#' @export
if (is.null(Data.Prep[[Effect]])==TRUE){ #Skip effects not needed
return()
}

x.i.j<-Data.Prep[[Effect]]$x.i.j
n.i.j<-Data.Prep[[Effect]]$n.i.j
m.i<-Data.Prep[[Effect]]$m.i
K.max<-Data.Prep[[Effect]]$K.max

Results.Effect<-lapply(1:K.max,stepDownRSCABS,x.i.j=x.i.j,n.i.j=n.i.j,m.i=m.i,Effect=Effect,test.type=test.type) #each k-level
Results.Effect<-do.call("rbind", lapply(Results.Effect, data.frame, stringsAsFactors = FALSE))  #'fix' output table
return(Results.Effect)
}
