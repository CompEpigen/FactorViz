#This file contains the reactive elements of the shiny app

dir <- reactive(input$dir)

path <- reactive({
  home <- normalizePath("~")
  if (is.recursive(dir())){
    file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  }else{
    'NA'
  }
})



dataset <- reactive(quote({
  results<-medecom_object
  results
}),quoted=TRUE)