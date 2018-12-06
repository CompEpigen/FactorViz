#This file contains the reactive elements of the shiny app
server_reactive<-function(input, output, server_env){
  server_env$dir <- reactive(input$dir)
  server_env$path <- reactive({
    ret<-""
    if(!is.null(PATH)){
      ret<-PATH
    }
    if(!is.null(server_env$dir())){
    volumes<-c(
      computer = ("/"),
      home = paste('/home/', CURRENT_USER, "/", sep = "")
    )
    pat<-parseDirPath(volumes,server_env$dir())
    if(length(pat)>0){
      ret<-paste(pat, "/", sep="")
    }
  }
    if(!is.null(input$text_dir) && input$text_dir!=''){
      ret<-input$text_dir
    }
    return(ret)
  })
  server_env$dataset <- reactive(quote({
    server_env$df()
    results<-medecom_object
    results
    }),quoted=TRUE)

  server_env$Selected<-reactiveValues(K=NULL, LAMBDA=NULL, CG=NULL)
  }
