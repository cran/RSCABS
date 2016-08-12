getDir <-
function(window){
#This function will create a new directory in a user selected location 
#' @export
File<-' '
dialog <- gtkFileChooserDialog("Enter a name for a new folder and push save", window,
"save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save",
GtkResponseType["accept"])

if (dialog$run() == GtkResponseType["accept"]){
File<-dialog$getFilename()
}
dialog$destroy()
return(File)
}
