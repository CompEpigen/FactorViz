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
          load_data(path)
      }
    )
  })

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
