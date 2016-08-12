saveFile <-
function(File,OutData){
#Saves the output
#' @export
write.table(OutData,paste(File,'.csv',sep=''),row.names=FALSE,sep=',')
}
