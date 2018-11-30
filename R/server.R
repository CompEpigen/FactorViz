# This script contains the basic server interface of FactorViz, server code related to
# ouptut, reactive elements and output can be found inside server folder.

base_server <- function(input, output) {


  server_env <- new.env()
  shinyDirChoose(
    input,
    'dir',
    roots = c(
      computer = ("/"),
      home = paste('/home/', CURRENT_USER, "/", sep = "")
    ),
    filetypes = c('', 'RData')
  )
  server_env$df <- eventReactive(input$load, {
    path<-server_env$path()
    withProgress(
      message = 'Loading datasets in progress\n',
      detail = 'This may take a while...',
      value = 0,
      {
          hideTabs(input, output)
          load_data(path)
          showTabs(input, output)
      }
    )
  })
  showTabs<-function(input, output){
    showTab(inputId = "base_nav", target = "K selection")
    showTab(inputId = "base_nav", target = "Lambda selection")
    showTab(inputId = "base_nav", target = "LMCs")
    showTab(inputId = "base_nav", target = "Meta Analysis")
    showTab(inputId = "base_nav", target = "Proportions")
  }
  hideTabs<-function(input, output){
    hideTab(inputId = "base_nav", target = "K selection")
    hideTab(inputId = "base_nav", target = "Lambda selection")
    hideTab(inputId = "base_nav", target = "LMCs")
    hideTab(inputId = "base_nav", target = "Meta Analysis")
    hideTab(inputId = "base_nav", target = "Proportions")
  }
  if(!MEDSET_FLAG){
    hideTabs(input, output)
  }
  #source(server_reactive.R, local=TRUE)
  server_reactive(input, output, server_env)
  #source(server_getters.R, local=TRUE)
  server_getters(input, output, server_env)
  #source(server_output.R, local=T)
  server_output(input, output, server_env)
  #source(server_plot.R, local=T)
  server_plot(input, output, server_env)
  server_pdf(input, output, server_env)
  server_table(input, output, server_env)
}
