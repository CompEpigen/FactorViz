#This file contains the reactive elements of the shiny app
server_reactive<-function(input, output, server_env){
  server_env$dir <- reactive(input$dir)
  server_env$path <- reactive({
    volumes<-c(
      computer = ("/"),
      home = paste('/home/', CURRENT_USER, "/", sep = "")
    )
    parseDirPath(volumes,input$dir)
  })
  server_env$dataset <- reactive(quote({
    results<-medecom_object
    results
    }),quoted=TRUE)
  }
