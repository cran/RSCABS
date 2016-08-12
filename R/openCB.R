openCB <-
function(window) {
#Creates a window to open a file and loads a CVS into R
#This function is called from all Mods
#' @export
dialog <- gtkFileChooserDialog("Choose a CSV file", window, "open",
"gtk-cancel", GtkResponseType["cancel"], "gtk-open",
GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
.RSCABSEnv$FileName<-dialog$getFilename()
if (strsplit(.RSCABSEnv$FileName,'.csv') != .RSCABSEnv$FileName){
df <- read.csv(.RSCABSEnv$FileName)
}
if (strsplit(.RSCABSEnv$FileName,'.csv') == .RSCABSEnv$FileName){
df <-  data.frame(variables=character(0), stringsAsFactors=FALSE)
popMessage('Only comma separated value (.csv) files can be loaded') 
}
}
dialog$destroy()
df
 }
