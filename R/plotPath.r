plotPath<-function(){
#Function that plot histopathological data
#' @export

#On Start up
Metrics<-c('Percent','Total')
Lowest<-c('Remove','Include')
Colors<-c('heat.colors','terrain.colors','rainbow','topo.colors','cm.colors','gray.colors')


#Display endpoint with the lowest p-value
Idx<-which(.RSCABSEnv$ResultsMain['P-Value']==min(.RSCABSEnv$ResultsMain['P-Value']))[1]
StartRVal<-strsplit(as.character(.RSCABSEnv$ResultsMain$Response[Idx]),split='')[[1]]
StartRVal<-paste0(StartRVal[-length(StartRVal)],collapse = '')
#for the CBox later on
IdxL<-which(colnames(.RSCABSEnv$UseData)==StartRVal)	

#Defaults	
.RSCABSEnv$Response2Graph<-StartRVal
.RSCABSEnv$MetricGraph<-'Percent'
.RSCABSEnv$LowestGraph<-'Remove'
.RSCABSEnv$PlotParms<-list()

	.RSCABSEnv$PlotWindow<-gwindow("Plotpath", visible=FALSE)
		size(.RSCABSEnv$PlotWindow)<-c(800,600)
		.RSCABSEnv$MainPtGp <- ggroup(horizontal = TRUE, container=.RSCABSEnv$PlotWindow)
			.RSCABSEnv$ControlPtFm <- gframe(horizontal = FALSE, container=.RSCABSEnv$MainPtGp, fill=TRUE)	
		#Handle plotting parameters		
				ResponsePtL<-glabel('Choose Response',container=.RSCABSEnv$ControlPtFm,where='center') 
				.RSCABSEnv$ResponsePtCBx<-gcombobox(colnames(.RSCABSEnv$UseData), selected = IdxL, editable = FALSE, container = .RSCABSEnv$ControlPtFm,
								handler= function(h,...){
									.RSCABSEnv$Response2Graph<-svalue(.RSCABSEnv$ResponsePtCBx)
									.RSCABSEnv$PlotParms$main<-NULL
									#Check for errors
									Msg<-checkPlot()
									if (is.null(Msg)==FALSE){
										popMessage(Msg)	
									}	
									plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
										.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)								
								})			
				 MetricPtL<-glabel('Metric',container=.RSCABSEnv$ControlPtFm,where='center') 
				.RSCABSEnv$MetricPtCBx<-gcombobox(Metrics, selected = 1, editable = FALSE, container = .RSCABSEnv$ControlPtFm,
								handler= function(h,...){
									.RSCABSEnv$MetricGraph<-svalue(.RSCABSEnv$MetricPtCBx)
									.RSCABSEnv$PlotParms$ylab<-NULL
									plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
										.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)
								})		
				 LowestPtL<-glabel('Lowest Value?',container=.RSCABSEnv$ControlPtFm,where='center') 
				.RSCABSEnv$LowestPtCBx<-gcombobox(Lowest, selected = 1, editable = FALSE, container = .RSCABSEnv$ControlPtFm,
								handler= function(h,...){
									.RSCABSEnv$LowestGraph<-svalue(.RSCABSEnv$LowestPtCBx)
									plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
										.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)
								})		
				 ColorPtL<-glabel('Color Palette',container=.RSCABSEnv$ControlPtFm,where='center') 				
				.RSCABSEnv$ColorPtCBx<-gcombobox(Colors, selected = 1, editable = FALSE, container = .RSCABSEnv$ControlPtFm,
								handler= function(h,...){
									.RSCABSEnv$PlotParms$Colors<-NULL
									.RSCABSEnv$PlotParms$ColorFunction<-get(svalue(.RSCABSEnv$ColorPtCBx))
									plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
										.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)
								})
				 TitleBoPtL<-glabel('Remove Title',container=.RSCABSEnv$ControlPtFm,where='center')				
				.RSCABSEnv$TitlePtCBx<-gcombobox(c(FALSE,TRUE), selected = 1, editable = FALSE, container = .RSCABSEnv$ControlPtFm,
								handler= function(h,...){
									if (svalue(.RSCABSEnv$TitlePtCBx) == TRUE){
										.RSCABSEnv$PlotParms$main<-''
									}
									if (svalue(.RSCABSEnv$TitlePtCBx) == FALSE){
										.RSCABSEnv$PlotParms$main<-NULL
									}								
									plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
										.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)
								})				
				#RefreshBt<-gbutton('Refresh Plot', container = .RSCABSEnv$ControlPtFm, handler= function(h,...){	
				#					plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
				#						.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)
				#			})
				
				SaveBt<-gbutton('Save Current Graph', container = .RSCABSEnv$ControlPtFm, handler= function(h,...){						
						#get plot format
						.RSCABSEnv$ChooseWindow<-gwindow("Please select file format", visible=FALSE,horizontal=FALSE)
							TempGp<-ggroup(horizontal=FALSE,container=.RSCABSEnv$ChooseWindow)
							Subsetselect<- gtable(c('pdf','bmp','jpeg','png','tiff'),container=TempGp,expand=TRUE)	
							SelectButton <- gbutton("Select",container=TempGp,handler= function(h,...) {
								.RSCABSEnv$PlotFormat<-svalue(Subsetselect)
								dispose(.RSCABSEnv$ChooseWindow)})
							visible(.RSCABSEnv$ChooseWindow)<-TRUE
							while(is.null(visible(.RSCABSEnv$ChooseWindow))==FALSE){
								Sys.sleep(0.01)
							}
								
							#get file name and save
								stemp<-gtkWindow(show=FALSE)	
								dialog <- gtkFileChooserDialog("Enter a name for the file", stemp,"save", "gtk-cancel",
								GtkResponseType["cancel"], "gtk-save",GtkResponseType["accept"])
									if (dialog$run() == GtkResponseType["accept"]){
										#PLot and save
										if (identical(.RSCABSEnv$PlotFormat,'tiff')==FALSE){
											plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,.RSCABSEnv$MetricGraph,
												.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms,Format=.RSCABSEnv$PlotFormat,File=dialog$getFilename())	
										}else{
											plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,.RSCABSEnv$MetricGraph,
												.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms,Format=.RSCABSEnv$PlotFormat,File=dialog$getFilename())
										}
										dialog$destroy()
										popMessage('File Saved')
									}
									if (dialog$run() == GtkResponseType["cancel"]){									
										dialog$destroy()
										popMessage('Save aborted')
									}														
							})
				SaveAllBt<-gbutton('Save All Graphs', container = .RSCABSEnv$ControlPtFm, handler= function(h,...){
							#get plot format
								.RSCABSEnv$ChooseWindow<-gwindow("Please select file format", visible=FALSE,horizontal=FALSE)
									TempGp<-ggroup(horizontal=FALSE,container=.RSCABSEnv$ChooseWindow)
									Subsetselect<- gtable(c('pdf','bmp','jpeg','png','tiff'),container=TempGp,expand=TRUE)	
									SelectButton <- gbutton("Select",container=TempGp,handler= function(h,...) {
										.RSCABSEnv$PlotFormat<-svalue(Subsetselect)
										dispose(.RSCABSEnv$ChooseWindow)})
									visible(.RSCABSEnv$ChooseWindow)<-TRUE								
								while(is.null(visible(.RSCABSEnv$ChooseWindow))==FALSE){
									Sys.sleep(0.01)
								}
								#get plot directory
								stemp<-gtkWindow(show=FALSE)
								Dir<-''
								Dir<-getDir(stemp)
								#pause loop while directory is being selected
								while(Dir==''){
									Sys.sleep(0.01)
								}
								if (Dir==' '){
									popMessage('A folder was not selected\nPlease Select a folder')
									return()
								}
								Names<-strsplit(as.character(.RSCABSEnv$ResultsMain$Response),split='')
								Names<-lapply(Names,function(X){paste0(X[-length(X)],collapse = '')})
								Names<-unique(Names)								
								Files<-lapply(Names,function(X){paste(Dir,'\\',X,sep='')})
								dir.create(Dir) 
								CantPrint<-''
								for (i in 1:length(Files)){
									
									Msg<-try(plotRSCABS(.RSCABSEnv$UseData,Names[[i]],.RSCABSEnv$TreatmentVar,.RSCABSEnv$MetricGraph,
										.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms,Format=.RSCABSEnv$PlotFormat,File=Files[[i]]))
									if (is(Msg)[1]=='try-error'){
										CantPrint<-paste(CantPrint,Names[[i]],sep=' \n ')
										print(CantPrint)
										dev.off()
									}
								}		
								if (identical(CantPrint,'')==FALSE){
									popMessage(paste('The following can not be saved',CantPrint,sep='\n'))
								}
							})
			
			
			.RSCABSEnv$PtGraphFm <- gframe(horizontal = TRUE, container=.RSCABSEnv$MainPtGp, expand=TRUE)
				GG<-ggraphics(container=.RSCABSEnv$PtGraphFm)

	visible(.RSCABSEnv$PlotWindow)<-TRUE
	plotRSCABS(.RSCABSEnv$UseData,.RSCABSEnv$Response2Graph,.RSCABSEnv$TreatmentVar,
										.RSCABSEnv$MetricGraph,.RSCABSEnv$LowestGraph,.RSCABSEnv$PlotParms)										
}


