detailedResults2HTML <-
function(Results,k,Dir,Effect,Inverse=FALSE){
#This will generate an HTLM Report on the detailed analysis of a Response
#requires the use of a R2HTML library  
#k is the current Severity score
#Dir is the new directory that is created
#Effect is Effect tested
#' @export
Result<-Results[[k]]
ChiTable<-NULL
Freqtable<-NULL
#ChiTable
if (is.null(Result[['ChiResults']])==FALSE){
ChiTable<-Result$ChiResults
}
if (is.null(Result[['Step']][[1]][['FreqTable']])==FALSE){
Nstep<-length(Result$Step)

#Frequency Table 
Freqtable<-Result$Step[[1]]$FreqTable
Freqtable<-cbind(Freqtable,rowSums(Freqtable))
Freqtable<-rbind(Freqtable,colSums(Freqtable))

if (Inverse==TRUE){
	Freqtable<-cbind(c(paste('>',k,sep=''),paste('<=',k,sep='')  ,'Total'),Freqtable)
}else{
	Freqtable<-cbind(c(paste('<',k,sep=''),paste('>=',k,sep='')  ,'Total'),Freqtable)
}
Freqtable<-as.data.frame(Freqtable)
colnames(Freqtable)<-c('Score/Treatment',1:{dim(Freqtable)[2]-2},'Total')
}
#RSCAB Table 
RSCABTable<-{}
for (j in 1:Nstep){
RSCABTable<-rbind(RSCABTable,Result$Step[[j]]$RSCAB)
}

colnames(RSCABTable)<-c('Response','Treatment','R-Score','Statistic','P-Value','Signif')
rownames(RSCABTable)<-NULL
RSCABTable<-as.data.frame(RSCABTable)
for (j in 1:6){
RSCABTable[ ,j]<-unlist(RSCABTable[ ,j])
}
RSCABTable<-RSCABTable[ ,-1]
RSCABTable[ ,3]<-round(as.numeric(as.character(RSCABTable[ ,3])),5)
RSCABTable[ ,4]<-round(as.numeric(as.character(RSCABTable[ ,4])),5)

if (length(which(RSCABTable[ ,4]=='NaN'))>0){
RSCABTable<-RSCABTable[-which(RSCABTable[ ,4]=='NaN'), ]
}
if (dim(RSCABTable)[1]==0){
RSCABTable<-{}}


#This Will output to and HTML file 
HTMLStart(outdir=Dir, filename= paste(Effect,k,sep='P'),
extension="html", echo=FALSE, HTMLframe=TRUE)    #Starts a link to create a HTML file
Title<-paste('Detailed results for the response of ',Effect,' with Severity Score >=',k,
'<p>',.RSCABSEnv$GenderVal,' data',' for generation ',.RSCABSEnv$GenerationVal,'</p>',sep='')

HTML.title(Title, HR=1,CSSstyle='')

#HTML for Chi-Squared Table
HTML('<center><b>Chi-Squared Heterogeneity Check of Between-Replicate Variances</b></center>')
if (is.null(ChiTable)==FALSE){
HTML(ChiTable,row.name=FALSE,innerBorder = 1,HR=1)}
if (is.null(ChiTable)==TRUE){
HTML('<center><b>Produced No Results</b></center>') }

#HTML for Frequency Table
HTML(paste('<center><b>Frequency Table for',Effect,'</b></center>'))
if (is.null(Freqtable)==FALSE){
HTML(Freqtable,row.name=FALSE,innerBorder = 1,CSSstyle='')}
if (is.null(Freqtable)==TRUE){
HTML('<center><b>Produced No Results</b></center>')}

#HTML for RSCABS Table
HTML(paste('<center><b>Rao-Scott Cochran Armitage Test for ',Effect,'</b></center>'))
if (is.null(RSCABTable)==FALSE){
HTML(RSCABTable,row.name=FALSE,innerBorder = 1,CSSstyle='')}
if (is.null(RSCABTable)==TRUE){
HTML('<center><b>Produced No Results</b></center>')}
 
HTMLStop()
   
   #Deletes Junk files
unlink(paste(Dir,'\\',Effect,'P',k,'.html',sep=''))
unlink(paste(Dir,'\\',Effect,'P',k,'_menu.html',sep='')) 
}
