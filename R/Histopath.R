Histopath <-
function(){
#This is the main GUI call for Histopath Data
#' @export
.RSCABSEnv$CanRun<-FALSE
.RSCABSEnv$GenderVar<-'Not Used'
.RSCABSEnv$GenderVal<-'Not Used'
.RSCABSEnv$GenerationVar<-'Not Used'
.RSCABSEnv$GenerationVal<-'Not Used'
.RSCABSEnv$TreatmentVar<-'Not Used'
.RSCABSEnv$AgeVar<-'Not Used'
.RSCABSEnv$AgeVal<-'Not Used'
.RSCABSEnv$ReplicateVar<-''

#main Window 
#Containers are scoped by white space  
.RSCABSEnv$HistoWindow<-gwindow("Histopath", visible=FALSE)
	size(.RSCABSEnv$HistoWindow)<-c(712,420)
	.RSCABSEnv$HistosNotebook <- gnotebook(container =.RSCABSEnv$HistoWindow, expand=TRUE)
		.RSCABSEnv$Maingroup <- ggroup(horizontal = TRUE)
		add(.RSCABSEnv$HistosNotebook,.RSCABSEnv$Maingroup, label="Main")
		.RSCABSEnv$ButtonBox<-gframe(horizontal = FALSE, container=.RSCABSEnv$Maingroup)
		LoadButton<-gbutton("Load Data",container=.RSCABSEnv$ButtonBox,handler= function(h,...){ #load data button
		temp<-gtkWindow(show=FALSE)
		.RSCABSEnv$MainData<-openCB( temp)
		delete(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid)
		delete(.RSCABSEnv$Maingroup,.RSCABSEnv$DataBox)
		.RSCABSEnv$DataBox<-gframe(horizontal = FALSE,expand=TRUE)
		add(.RSCABSEnv$Maingroup,.RSCABSEnv$DataBox,  expand=TRUE)
		.RSCABSEnv$DataGrid<-gtable(.RSCABSEnv$MainData)
		add(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid,expand=TRUE)
		add(.RSCABSEnv$ButtonBox,.RSCABSEnv$SpecButton)
		}) 
		.RSCABSEnv$SpecButton<-gbutton("Specify Data",handler= function(h,...){ #This will bring up the data Specification tab   
		addHistoSpec(.RSCABSEnv$HistosNotebook)})
		.RSCABSEnv$RunButton<-gbutton("Run RSCABS",handler= function(h,...){ #Runs RSCABS
		if (.RSCABSEnv$CanRun==FALSE){
		popMessage('please specify the data first.')
		}
		if (.RSCABSEnv$CanRun==TRUE){
		popMessage('The analysis is running this may take a moment')
		if (.RSCABSEnv$ReplicateVar==''){
		popMessage('A Replicate variable was not defined; Histopath will run SCABS instead of RSCABS')

		}

		.RSCABSEnv$ResultsMain<-RSCABSMain(.RSCABSEnv$UseData,.RSCABSEnv$TreatmentVar,.RSCABSEnv$ReplicateVar,FALSE) #RSCABS
		delete(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid)		
		Sys.sleep(0.1) #need to pause 
		.RSCABSEnv$DataGrid<-gtable(.RSCABSEnv$ResultsMain)
		add(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid,expand=TRUE)
		add(.RSCABSEnv$ButtonBox,.RSCABSEnv$SaveButton)       
		Sys.sleep(0.1) #need to pause 
			plotPath() #call plots
		
		}
		})
		.RSCABSEnv$OtherButton<-gbutton("Run Other Analyses",handler= function(h,...){ #For other analyses
		otherPath()
		})
		.RSCABSEnv$SaveButton<-gbutton("Save Result",handler= function(h,...){ #Save the results
		stemp<-gtkWindow(show=FALSE)
		saveCB( stemp,.RSCABSEnv$ResultsMain)
		})
		.RSCABSEnv$DataBox<-gframe(horizontal = FALSE, label='Histopath Main',expand=TRUE)  #This box contains the displayed data 
		add(.RSCABSEnv$Maingroup,.RSCABSEnv$DataBox,  expand=TRUE)
		blankDF = data.frame(variables=character(0), stringsAsFactors=FALSE)
		.RSCABSEnv$DataGrid<-gtable(blankDF,  expand=TRUE )
		add(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid)

visible(.RSCABSEnv$HistoWindow)<-TRUE
}
