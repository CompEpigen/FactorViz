#This file contains the reactive elements of the shiny app
server_reactive<-function(input, output, server_env){
  server_env$dir <- reactive(input$dir)
  server_env$path <- reactive({
    volumes<-c(
      computer = ("/"),
      home = paste('/home/', CURRENT_USER, "/", sep = "")
    )
    parseDirPath(volumes,server_env$dir())
  })
  server_env$dataset <- reactive(quote({
    server_env$df()
    results<-medecom_object
    results
    }),quoted=TRUE)
  }
