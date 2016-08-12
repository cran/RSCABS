addHistoSpec <-
function(Notebook){
#This tab is where the user specifies the data set
#Main tab for specifying data
#This tab is where the user specifies the data set
#Main tab for specifying data
#' @export
SpecGroup <- ggroup(horizontal = FALSE, container=Notebook, label='Data specification')

#This Box contains the gender select buttons
GendBox<-gframe(horizontal = TRUE, spacing = 5, container=SpecGroup)
GendVarButton<-gbutton("Select Gender Variable",container=GendBox,handler= function(h,...){
selectPara('GenderVar','GendVarLabel','.RSCABSEnv')})
GendVarFrame<-gframe(horizontal = FALSE, container=GendBox,expand=TRUE)
.RSCABSEnv$GendVarLabel<-glabel("Gender Variable Not Selected",container=GendVarFrame,expand=TRUE)
size(GendVarFrame)<-c(175,35)
GendValButton<-gbutton("Select Gender Value",container=GendBox,handler= function(h,...){
selectPara('GenderVal','GendValLabel','.RSCABSEnv')})
GendValFrame<-gframe(horizontal = FALSE, container=GendBox,expand=TRUE)
.RSCABSEnv$GendValLabel<-glabel("Gender Variable Not Selected",container=GendValFrame,expand=TRUE)
size(GendValFrame)<-c(175,35)

#This Box contains the generation select buttons
GenderBox<-gframe(horizontal = TRUE, spacing = 5, container=SpecGroup)
GenerVarButton<-gbutton("Select Generation Variable",container=GenderBox,handler= function(h,...){
selectPara('GenerationVar','GenerVarLabel','.RSCABSEnv')})
GenerVarFrame<-gframe(horizontal = FALSE, container=GenderBox,expand=TRUE)
.RSCABSEnv$GenerVarLabel<-glabel("Generation Variable Not Selected",container=GenerVarFrame,expand=TRUE)
size(GenerVarFrame)<-c(175,35)
GenerValButton<-gbutton("Select Generation Value",container=GenderBox,handler= function(h,...){
selectPara('GenerationVal','GenerValLabel','.RSCABSEnv')})
GenerValFrame<-gframe(horizontal = FALSE, container=GenderBox,expand=TRUE)
.RSCABSEnv$GenerValLabel<-glabel("Generation Variable Not Selected",container=GenerValFrame,expand=TRUE)
size(GenerValFrame)<-c(175,35)

#This Box contains the age select buttons
AgeBox<-gframe(horizontal = TRUE, spacing = 5, container=SpecGroup)
AgeVarButton<-gbutton("Select Age Variable",container=AgeBox,handler= function(h,...){
selectPara('AgeVar','AgeVarLabel','.RSCABSEnv')})
AgeVarFrame<-gframe(horizontal = FALSE, container=AgeBox,expand=TRUE)
.RSCABSEnv$AgeVarLabel<-glabel("Age Variable Not Selected",container=AgeVarFrame,expand=TRUE)
size(AgeVarFrame)<-c(175,35)
AgeValButton<-gbutton("Select Age Value",container=AgeBox,handler= function(h,...){
selectPara('AgeVal','AgeValLabel','.RSCABSEnv')})
AgeValFrame<-gframe(horizontal = FALSE, container=AgeBox,expand=TRUE)
.RSCABSEnv$AgeValLabel<-glabel("Age Variable Not Selected",container=AgeValFrame,expand=TRUE)
size(AgeValFrame)<-c(175,35)


#This box contains all the buttons related to experimental design 
DesignBox<-gframe(horizontal = TRUE, spacing = 5, container=SpecGroup)
TreatButton<-gbutton("Select Treatment Variable",container=DesignBox,handler= function(h,...){
selectPara('TreatmentVar','TreatVarLabel','.RSCABSEnv')})
TreatVarFrame<-gframe(horizontal = FALSE, container=DesignBox,expand=TRUE)
.RSCABSEnv$TreatVarLabel<-glabel("Treatment Variable Not Selected",container=TreatVarFrame,expand=TRUE)
size(TreatVarFrame)<-c(175,35)
RepButton<-gbutton("Select Replicate Variable",container=DesignBox,handler= function(h,...){ #Note this is not needed 
selectPara('ReplicateVar','RepVarLabel','.RSCABSEnv')})
RepVarFrame<-gframe(horizontal = FALSE, container=DesignBox,expand=TRUE)
.RSCABSEnv$RepVarLabel<-glabel("Replicate Variable Not Selected",container=RepVarFrame,expand=TRUE)
size(RepVarFrame)<-c(175,35)


EndBox<-gframe(horizontal = FALSE, container=SpecGroup)
#This button will run the check to make sure the analysis can run
OkButton<-gbutton("Confirm Selected Values and Variables",container=EndBox,handler= function(h,...){
msg<-checkSelction()
if(msg=='You still need to select:\n'){
.RSCABSEnv$CanRun<-TRUE
.RSCABSEnv$UseData<-runStdSubset(.RSCABSEnv$MainData)
delete(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid)
add(.RSCABSEnv$ButtonBox,.RSCABSEnv$RunButton)
add(.RSCABSEnv$ButtonBox,.RSCABSEnv$OtherButton)
.RSCABSEnv$DataGrid<-gtable(.RSCABSEnv$UseData)
add(.RSCABSEnv$DataBox,.RSCABSEnv$DataGrid,expand=TRUE)
popMessage('The analysis can be ran from the Histopath Main tab')}
if(msg!='You still need to select:\n'){
popMessage(msg)}
})

}
