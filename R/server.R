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
        ANN_C <<- "ann_C.RData"
      	ANN_S <<- "ann_S.RData"
      	MEDSET <<- "medecom_set.RData"
      	REF <<- "ref_meth.RData"
      	CURRENT_USER <<- Sys.info()[["user"]]

      	MEDSET_FLAG <<- F
      	ANN_C_FLAG <<- F
      	TRUE_T_FLAG <<- F
      	TRUE_A_FLAG<<-F
      	PHENO_DATA_FLAG <<- F
      	SAMPLE_NAME_FLAG <<- F
      	METH_DATA_FLAG <<- F

      	medecom_object <<- NULL
      	input_object <<- list(
      		pheno.data = NULL,
      		meth.data = NULL,
      		sample.names = NULL
      	)

      	true_T_matrix <<- NULL
      	true_A_matrix <<- NULL
      	true_T_matrix_ref <<- NULL
      	cg_annot_object <<- NULL
      	gene_annot_object <<- NULL
      	prepared_annotation <<- list(
      		"cat_list" = NULL,
      		"cat_inf_vs" = NULL,
      		"faetures" = NULL,
      		"settings" = NULL
      	)
      	gene_set_object <<- NULL

      	medecom_ref_object<<-NULL

      	lola.db<<-NULL

        if (file.exists(paste(path, MEDSET, sep = "/"))) {
          new.envi <- new.env()
          load(paste(path, MEDSET, sep = "/"), envir=new.envi)
          medecom_object <<- get(ls(envir = new.envi),envir = new.envi)
          MEDSET_FLAG <<- T
        }
        incProgress(1 / 4)
        if (file.exists(paste(path, ANN_C, sep = "/"))) {
          new.envi <- new.env()
          load(paste(path, ANN_C, sep = "/"), envir=new.envi)
          cg_annot_object <<- get(ls(envir = new.envi),envir = new.envi)
          ANN_C_FLAG <<- T
        }
        incProgress(1 / 4)
        if (file.exists(paste(path, ANN_S, sep = "/"))) {
          new.envi <- new.env()
          load(paste(path, ANN_S, sep = "/"), envir=new.envi)
          input_object$pheno.data <<- get(ls(envir = new.envi),envir = new.envi)
          PHENO_DATA_FLAG <<- T
          if ("sample_id" %in% input_object$pheno.data) {
            input_object$sample.names <- input_object$pheno.data$sample_id
          } else{
            input_object$sample.names <- 1:nrow(input_object$pheno.data)
          }
          SAMPLE_NAME_FLAG <<- T
        }
        incProgress(1 / 4)
        if (file.exists(paste(path, REF, sep = "/"))) {
          new.envi <- new.env()
          load(paste(path, REF, sep = "/"), envir=new.envi)
          true_T_matrix <<- get(ls(envir = new.envi),envir = new.envi)
          TRUE_T_FLAG <<- T
        }
        incProgress(1 / 4)
        new.envi <- new.env()
        gc()
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
  server_table(input, output, server_env)

}
