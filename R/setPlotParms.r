setPlotParms<-function(PlotParms,Out,Effect,Metric,Lowest,...){
#Defines the default plot parameters
#PlotParms: List that store the plotting parameters
#Out: Results matrix that will be plotted
#Effect: plotted endpoint
#Treatment: name of the treatment 
#' @export

if (is.null(PlotParms)==TRUE){
	PlotParms<-list()	
}


#Added to allow for extra parameter passing.
#Merge ... and plotpara
Extra<-list(...)
if (length(Extra)>0){
	for (e in names(Extra)){
		PlotParms[[e]]<-Extra[[e]]
	}
}
if (is.null(Extra$col)==FALSE){
	PlotParms$Colors<-Extra$col
}

	
	
	
if (identical(Lowest,'Remove')){
	if (dim(Out)[1]==2){
		PlotParms$legend.text<-rownames(Out)[2]
	}
	Out<-Out[-1, ]
}

#Set the default plotting parameters	
	if (is.null(PlotParms$xlab)==TRUE){
		PlotParms$xlab<-'Treatment Group'	
	}
	if (is.null(PlotParms$ylab)==TRUE){
		PlotParms$ylab<-Metric	
	}
	if (is.null(PlotParms$main)==TRUE){			  
		PlotParms$main<-Effect
		try(PlotParms$main<-paste(Effect,'for \n',.RSCABSEnv$GenerationVal,.RSCABSEnv$GenderVal,.RSCABSEnv$AgeVal))	#Relies on entry form 	
	}	
	if (is.null(PlotParms$legend.text)==TRUE){			  
		PlotParms$legend.text<-rownames(Out)			
	}
	if (is.null(PlotParms$args.legend)==TRUE){			  
		PlotParms$args.legend<-list('x' = (par()$usr[2]*.98),'y'=par()$usr[4],'xpd'=TRUE,'xjust'=0)		
	}		
#Color for groups	
	if (is.null(PlotParms$ColorFunction)==TRUE && is.null(PlotParms$Colors)==TRUE ){
		PlotParms$ColorFunction<-heat.colors
	}
	if (is.null(PlotParms$Colors)==TRUE){	
		n<-dim(Out)[1]
		if (is.null(n)==TRUE){
			n<-1
		}
		PlotParms$Colors<-PlotParms$ColorFunction(n)
		
		PlotParms$col<-PlotParms$Colors

	}
	#Added to allow for ...
	PlotParms$ColorFunction<-NULL
	PlotParms$Colors<-NULL
		
return(PlotParms)
}