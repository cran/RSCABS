popMessage <-
function(Message){
#This function will pop up and display a message to the user
#' @export
PopWindow<-gwindow("Alert!",width = 240, height= 157,visible=FALSE)
Textframe<-gframe(horizontal = FALSE, container=PopWindow,expand=TRUE,fill=TRUE)
TextMessage<-gtext(Message,container=Textframe,expand=TRUE,fill=TRUE);
Okbutton<-gbutton("OK",container=Textframe,handler= function(h,...){
dispose(PopWindow)
})
visible(PopWindow)<-TRUE
 }
