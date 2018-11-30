#This file contains the reactive elements of the shiny app
server_reactive<-function(input, output, server_env){
  server_env$dir <- reactive(input$dir)
  server_env$path <- reactive({
    ret<-""
    if(!is.null(PATH)){
      ret<-PATH
    }
    if(server_env$dir()){
    volumes<-c(
      computer = ("/"),
      home = paste('/home/', CURRENT_USER, "/", sep = "")
    )
    ret<-parseDirPath(volumes,server_env$dir())
    }
    if(!is.null(input$text_dir)){
      if(input$dir!=""){
      ret<-input$text_dir
    }
    }
    return(ret)
  })
  server_env$dataset <- reactive(quote({
    server_env$df()
    results<-medecom_object
    results
    }),quoted=TRUE)

  server_env$K<-reactive(K)
  server_env$lambda<-reactive(LAMBDA)
  server_env$cg<-reactive(CG)
  }
