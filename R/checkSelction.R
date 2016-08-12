checkSelction <-
function(){
#This function builds a message telling the user they 
#still need to specify variable names and values 
#If the message is modified by this function it will 
#display the message to the user and not let them move on
#' @export
msg<-'You still need to select:\n'
if (.RSCABSEnv$TreatmentVar=='' ||.RSCABSEnv$TreatmentVar=='Not Used' )
{msg<-paste(msg,'A Treatment Variable\n')
}
return(msg)
}
