#This file contains the reactive elements of the shiny app
server_reactive<-function(input, output, server_env){
  server_env$dir <- reactive(input$dir)

  server_env$path <- reactive({
    home <- normalizePath("~")
    if (is.recursive(server_env$dir())){
      file.path(home, paste(unlist(server_env$dir()$path[-1]), collapse = .Platform$file.sep))
    }else{
      c('Please Choose a directory')
    }
  })
  server_env$dataset <- reactive(quote({
    results<-medecom_object
    results
    }),quoted=TRUE)
  }
