otherPath <-
function(){
#This is called from the Histopath main window and is the control for
#the other histology analysis
.RSCABSEnv$LastTest<-'None';
#' @export
#main window
OtherHistWindow<-gwindow("Other Histology Analysis", visible=FALSE)
size(OtherHistWindow)<-c(712,420)
.RSCABSEnv$OtherHistNotebook <- gnotebook(container =OtherHistWindow, expand=TRUE)
.RSCABSEnv$OtherMaingroup <- ggroup(horizontal = TRUE)
add(.RSCABSEnv$OtherHistNotebook,.RSCABSEnv$OtherMaingroup, label='Histopath Other Main')


.RSCABSEnv$OtherButtonBox<-gframe(horizontal = FALSE, container=.RSCABSEnv$OtherMaingroup)
SCABSButton<-gbutton("Run SCABS",container=.RSCABSEnv$OtherButtonBox,handler= function(h,...){
	
		#Invert data for the analysis 
			if (is.element('Not Used',.RSCABSEnv$InverseScaleVar)==FALSE){
				.RSCABSEnv$UseData<-.invertOrder(.RSCABSEnv$UseData,.RSCABSEnv$InverseScaleVar)
			}
			
	Sys.sleep(0.1) #need to pause 
	.RSCABSEnv$ResultsSCAB<-RSCABSMain(.RSCABSEnv$UseData,.RSCABSEnv$TreatmentVar,.RSCABSEnv$ReplicateVar,TRUE)
	
	#Clean results
	.RSCABSEnv$ResultsSCAB<-unique(.RSCABSEnv$ResultsSCAB)
	.RSCABSEnv$ResultsSCAB<-.orderOutput(.RSCABSEnv$ResultsSCAB)
	
		#Un-invert the scale for plotting purposes
			if (is.element('Not Used',.RSCABSEnv$InverseScaleVar)==FALSE){
				.RSCABSEnv$UseData<-.invertOrder(.RSCABSEnv$UseData,.RSCABSEnv$InverseScaleVar)
				#Un-invert the R-Score on the results tags 
				.RSCABSEnv$ResultsSCAB<-.tagInvert(.RSCABSEnv$ResultsSCAB,.RSCABSEnv$UseData,.RSCABSEnv$InverseScaleVar)
			}
			
	Sys.sleep(0.1) #need to pause 
	
	delete(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox)
	.RSCABSEnv$OtherDataBox<-gframe(horizontal = FALSE,expand=TRUE)
	add(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox,expand=TRUE)
	.RSCABSEnv$DataGrid<-gtable(.RSCABSEnv$ResultsSCAB,container=.RSCABSEnv$OtherDataBox,expand=TRUE)
	.RSCABSEnv$LastTest<-'SCABS'
	})
	
ResponseButton<-gbutton("Get Details on a Response",container=.RSCABSEnv$OtherButtonBox,handler= function(h,...){
	runEffectSelect()
})
#Runs analysis on all effects
AllResponseButton<-gbutton("Get Details on all Responses",container=.RSCABSEnv$OtherButtonBox,handler= function(h,...){
stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
ColNames<-colnames(.RSCABSEnv$UseData)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder')
ColNames<-NULL
return()
}
oldw <- getOption("warn")
options(warn=-1)
dir.create(Dir)
options(warn=oldw)

ResultsDetail<-{}
oldw <- getOption("warn")
options(warn=-1)
for (effect in ColNames){
Filter<-filterData(effect,.RSCABSEnv$UseData)
if (Filter == TRUE) {
	#2018-3-30
	InverseBool = is.element(effect,.RSCABSEnv$InverseScaleVar)
	
	ResultsDetail<-runDetailedResults(.RSCABSEnv$UseData,.RSCABSEnv$TreatmentVar,.RSCABSEnv$ReplicateVar,effect,InverseBool)
Kn<-length(ResultsDetail)
for (k in 1:Kn){
if (identical(ResultsDetail,'No Information')==FALSE ){
	detailedResults2HTML(ResultsDetail,k,Dir,effect,InverseBool)
}
}}}
	options(warn=oldw)
	popMessage('Analysis Complete')
	})

	.RSCABSEnv$SaveButton<-gbutton("Save",container=.RSCABSEnv$OtherButtonBox,handler= function(h,...){
	switch(.RSCABSEnv$LastTest,
	None=popMessage('Please run an analysis first.'),
	SCABS={stemp<-gtkWindow(show=FALSE);saveCB(stemp,.RSCABSEnv$ResultsSCAB)},
	Details={stemp<-gtkWindow(show=FALSE);Dir<-getDir(stemp);
	Kn<-length(.RSCABSEnv$ResultsEffect)
	if (Dir==' '){
	popMessage('A folder was not selected\nPlease Select a folder')
	return()
	}
	oldw <- getOption("warn")
	options(warn=-1)
	dir.create(Dir)
	options(warn=oldw)
	if (.RSCABSEnv$ResultsEffect !="No Information" ) {
		InverseBool = is.element(.RSCABSEnv$effect,.RSCABSEnv$InverseScaleVar)
	
	for (k in 1:Kn){
	detailedResults2HTML(.RSCABSEnv$ResultsEffect,k,Dir,.RSCABSEnv$effect,InverseBool)
	}}},
	)
	})
.RSCABSEnv$OtherDataBox<-gframe(horizontal = FALSE,expand=TRUE)
add(.RSCABSEnv$OtherMaingroup,.RSCABSEnv$OtherDataBox,expand=TRUE)
blankDF = data.frame(variables=character(0), stringsAsFactors=FALSE)
BlankGrid<-gtable(blankDF,  expand=TRUE, fill=TRUE )
add(.RSCABSEnv$OtherDataBox,BlankGrid)
delete(.RSCABSEnv$OtherDataBox,BlankGrid)

visible(OtherHistWindow)<-TRUE
}
