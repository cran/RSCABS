### R code from vignette source 'RSCABS.Rnw'

###################################################
### code chunk number 1: RSCABS.Rnw:398-440
###################################################
require(RSCABS)
data(exampleHistData)
subIndex<-which(exampleHistData$Generation=='F2' &
  exampleHistData$Genotypic_Sex=='Female' &
  exampleHistData$Age=='16_wk')
exampleHistData.Sub<-exampleHistData[subIndex, ]
Data<-exampleHistData.Sub
Effect="Gon_Asynch_Dev"
Treatment="Treatment"
Metric="Total"
Lowest ="Include"
PlotParms<-list()
PlotParms$xlab<-'Group'
PlotParms$ylab<-'Total Fish'
PlotParms$main<-'Example Graph for \n Example Data and Gon_Asynch_Dev'
PlotParms$Colors<-c('purple1','red3')


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

PlotParms$args.legend<-list('x' = (6.232*.98),'y'=6,'xpd'=TRUE,'xjust'=0)
par(mar=c(5, 4, 4, 3))
PlotParms<-setPlotParms(PlotParms,Out,Effect,Metric,Lowest)		
barplot(PlotMat,
		col=c('purple1','red3'),
		xlab=PlotParms$xlab,
		ylab=PlotParms$ylab,
		main=PlotParms$main,
		legend.text=PlotParms$legend.text,
		args.legend=PlotParms$args.legend	
	)


