selectPara <-function(VarName,LabelName=NULL,Enviro,What=NULL,Mult=FALSE,Display=NULL){
#' @export
#   Creates a window that allows for the selection of a variable or value
#VarName: The name of the variable or value to define
#LabelName: The name of the label that is updated
#Enviro: The name of the current Environment as a string
#What: either NULL, or a character vector to choose objects amongst  
#Mult: Bool indicating the ability to select multiple values 
#Display is the name of the window this function creates
if (is.null(What)==TRUE){
	Word<-strsplit(VarName,split='')[[1]]
	Type<-paste0(Word[{length(Word)-2}:length(Word)],collapse='')
	if (identical(Type,'Var')==TRUE){
		Varaibles<-c('Not Used',colnames(get('MainData',envir = get(Enviro))))
		if (is.null(Display)==TRUE){
			Display<-paste(paste0(Word[1:{length(Word)-3}],collapse=''),'Variable')
		}
	}

	if (identical(Type,'Val')==TRUE){
	#Look at the var that uses that val	
		Word[length(Word)]<-'r'
		From<-paste0(Word,collapse='')
		Choices<-levels(as.factor(get('MainData',envir = get(Enviro))[ ,get(From,get(Enviro))]))
		Varaibles<-c('Not Used',Choices)
		if (is.null(Display)==TRUE){
			Display<-paste(paste0(Word[1:{length(Word)-3}],collapse=''),'Value')
		}
	}
}else{
	Varaibles<-c('Not Used',What)
}

	SelectWindow<-gwindow(paste("Please select the",Display), visible=FALSE)
	group <- ggroup(horizontal = FALSE, container=SelectWindow,spacing = 20)

	SubSetSelect<- gtable(Varaibles,container=group,expand=TRUE,multiple=Mult)
	SelectButton <- gbutton("Select",container=group,handler= function(h,...){ 
	assign(VarName,   SubSetSelect[svalue(SubSetSelect,index=TRUE), ], envir = get(Enviro))

	if (Mult==TRUE){
		if(is.null(LabelName)==FALSE){
			try(temp<-get(LabelName,envir =get(Enviro)))		
			try(svalue(temp) <- 'Multiple Values');	
			LabelName<-NULL
		}
	}

	if (is.null(LabelName) ==FALSE){
		try(tempVar<-get(VarName, envir = get(Enviro)),silent = TRUE)
		try(tempWig<-get(LabelName,envir =get(Enviro)),silent = TRUE)
	
		try(svalue(tempWig) <- tempVar);
	
	}
	dispose(SelectWindow)

	})

#Added to pause GUI on call	
	addHandlerUnrealize(SelectWindow, handler = function(h,...) {
			assign(VarName,   'Not Used', envir = get(Enviro))
		  })
	 
	 
	visible(SelectWindow)<-TRUE
return()
}

		
