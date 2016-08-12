saveCB <-
function(window,OutData) {
#Window to save the output
#' @export
dialog <- gtkFileChooserDialog("Enter a name for the file", window,
"save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save",
GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"])
saveFile(dialog$getFilename(),OutData)
dialog$destroy()
 }
