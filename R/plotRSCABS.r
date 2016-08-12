
plotRSCABS<-function(Data,Effect,Treatment,Metric='Percent',Lowest='Remove',PlotParms=NULL,Format=NULL,File=NULL,...){
#Data: data set to be graphed
#Effect: endpoint plotted
#Treatment Variable
#Metric: Either 'Total' for counts or 'Percent' for percent 
#Lowest: assuming the lowest is a non-response 
	#use either 'Include' to include it or 'Remove' to Remove it'
#PlotPrms: list of plotting parameters for customizations
#Format: Plotting function as a string, defaults to null which is to window  
#' @export

DVec<-convert2Score(Data[[Effect]])
	counts <- table(DVec, Data[[Treatment]]) 
	percent<-apply(counts,2,function(x){x/sum(x)*100}) 


Out<-switch(Metric,
	Percent=percent,
	Total=counts
	)	
PlotMat<-switch(Lowest,
	Remove=Out[-1, ],
	Include=Out
	)					
#Set up margins for graph		
	barplot(PlotMat)
	#Sys.sleep(.1)
#Set Margin	
	par(mar=c(5, 4, 4, 3))	
	
	PlotParms<-setPlotParms(PlotParms,Out,Effect,Metric,Lowest,...)	
#Added so ... could be used


PlotParms$height<-PlotMat

#Plot the graph	
	do.call(barplot,PlotParms)

		
	if (is.null(Format)==FALSE){
		if (identical(Format,'tiff')==FALSE){
				X<-paste(File,'.',Format,sep='')
				get(Format)(X)
				par(mar=c(5, 4, 4, 3))
			}else{
					X<-paste(File,'.tif',sep='')
					tiff(X,res=600,compression = "none", height=5, width=5, units="in")		
					par(mar=c(5, 4, 4, 3))				
		}	
	#Plot the graph		
		do.call(barplot,PlotParms)	
		Sys.sleep(0.1)
		dev.off()
	}	
}
