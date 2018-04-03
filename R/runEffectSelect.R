runEffectSelect <-
function(){
#' @export
#This should really be an internal function

 #Alternated to take advantage of the inverse scale endpoint
#Invert data for the analysis 

	
Varaibles<-colnames(.RSCABSEnv$MainData)
EffectSelectWindow<-gwindow("Please Select a Response", visible=FALSE)
group <- ggroup(horizontal = FALSE, container=EffectSelectWindow,spacing = 20)
Subsetselect<- gtable(Varaibles,container=group,expand=TRUE)
SelectButton <- gbutton("Select",container=group,handler= function(h,...) {
	.RSCABSEnv$effect<-Subsetselect[svalue(Subsetselect,index=TRUE), ]; 
	dispose(EffectSelectWindow)
	
	#2018-3-30
	InverseBool = is.element(.RSCABSEnv$effect,.RSCABSEnv$InverseScaleVar)
	.RSCABSEnv$ResultsEffect<-runDetailedResults(.RSCABSEnv$UseData,.RSCABSEnv$TreatmentVar,.RSCABSEnv$ReplicateVar,.RSCABSEnv$effect,InverseBool)
	
	
if (identical(.RSCABSEnv$ResultsEffect,"No Information")==FALSE){
.RSCABSEnv$LastTest<-'Details'
delete(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox)
.RSCABSEnv$OtherDataBox<-gframe(horizontal = FALSE,expand=TRUE)
add(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox,expand=TRUE)
Kn<-length(.RSCABSEnv$ResultsEffect)
if (Kn == 1){
	
	detailedResults2Output(.RSCABSEnv$effect,.RSCABSEnv$ResultsEffect,1,.RSCABSEnv$OtherDataBox,FALSE,InverseBool)
}
if (Kn>2){
	for ( k in 1:Kn){
		detailedResults2Output(.RSCABSEnv$effect,.RSCABSEnv$ResultsEffect,k,.RSCABSEnv$OtherDataBox,TRUE,.RSCABSEnv$OtherHistNotebook,InverseBool)
	}
}
}
if (identical(.RSCABSEnv$ResultsEffect,"No Information")==TRUE){
popMessage('This Effect has a Constant Result. \n No analysis can be performed.')
}
})

visible(EffectSelectWindow)<-TRUE

#Un-invert the scale for plotting purposes
		
			


}






