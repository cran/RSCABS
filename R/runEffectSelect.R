runEffectSelect <-
function(){
#' @export
Varaibles<-colnames(.RSCABSEnv$MainData)
EffectSelectWindow<-gwindow("Please Select a Response", visible=FALSE)
group <- ggroup(horizontal = FALSE, container=EffectSelectWindow,spacing = 20)
 
Subsetselect<- gtable(Varaibles,container=group,expand=TRUE)
SelectButton <- gbutton("Select",container=group,handler= function(h,...) {
.RSCABSEnv$effect<-Subsetselect[svalue(Subsetselect,index=TRUE), ]; 
dispose(EffectSelectWindow)
.RSCABSEnv$ResultsEffect<-runDetailedResults(.RSCABSEnv$UseData,.RSCABSEnv$TreatmentVar,.RSCABSEnv$ReplicateVar,.RSCABSEnv$effect)
if (identical(.RSCABSEnv$ResultsEffect,"No Information")==FALSE){
.RSCABSEnv$LastTest<-'Details'
delete(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox)
.RSCABSEnv$OtherDataBox<-gframe(horizontal = FALSE,expand=TRUE)
add(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox,expand=TRUE)
Kn<-length(.RSCABSEnv$ResultsEffect)
detailedResults2Output(.RSCABSEnv$effect,.RSCABSEnv$ResultsEffect,1,.RSCABSEnv$OtherDataBox,FALSE)
if (Kn>2){
for ( k in 1:Kn){
detailedResults2Output(.RSCABSEnv$effect,.RSCABSEnv$ResultsEffect,k,.RSCABSEnv$OtherDataBox,TRUE,.RSCABSEnv$OtherHistNotebook)
}
}
}
if (identical(.RSCABSEnv$ResultsEffect,"No Information")==TRUE){
popMessage('This Effect has a Constant Result. \n No analysis can be performed.')
}
})

visible(EffectSelectWindow)<-TRUE
}
