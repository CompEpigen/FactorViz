# This script contains the basic server interface of FactorViz, server code related to
# ouptut, reactive elements and output can be found inside server folder.

base_server <- function(input, output, session) {


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
    withProgress(
      message = 'Loading datasets in progress\n',
      detail = 'Loading Data...',
      value = 0.3,
      {
          hideTabs(input, output)
          if(!input$multiplepath){
          load_data(decomp_output=server_env$path())
          }else{
            if(input$medecom_path=="Loaded as object"){
              medecom_set=PATH$MEDECOM_SET
            }else{
              medecom_set=input$medecom_path
            }
            if(input$annC_path=="Loaded as object"){
              annC=PATH$ANN_C
            }else{
              annC=input$annC_path
            }

            if(input$annS_path=="Loaded as object"){
              annS=PATH$ANN_S
            }else{
              annS=input$annS_path
            }

            if(input$ref_meth_path=="Loaded as object"){
              ref_meth=PATH$REF_METH
            }else{
              ref_meth=input$ref_meth_path
            }
            load_data(medecom_set=medecom_set, ann_C=annC, ann_S=annS, ref_meth=ref_meth)
          }
          incProgress(0.3, detail = "Doing Sanity Checks")
          server_env$check<-sanity_check()
          if(server_env$check[[1]]){
            print(server_env$check[[2]])
            showTabs(input, output)
          }
          else{
            print(server_env$check[[2]])
            start_state_initialiser()
          }
          incProgress(0.3, detail = "Adding final touches")
           server_env$Selected$K=NULL
           server_env$Selected$LAMBDA=NULL
           server_env$Selected$CG=NULL
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
  server_tab_uniformity_keeper(input, output, session, server_env)
}
