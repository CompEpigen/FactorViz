# base file to call from commad line

# starttest
# @export

#source("app.R", local=T)
startFactorViz <- function() {
  app <- shiny::shinyApp(
    ui = baseUI,
    server = base_server,
    onStart = function() {
      
      #
      # DeComp FILES
      #
      ANN_C <<- "ann_C.RData"
      ANN_S <<- "ann_S.RData"
      MEDSET <<- "medecom_set.RData"
      REF <<- "ref_meth.RData"
      CURRENT_USER <<- Sys.info()[["user"]]
      
      #
      # FLAGS
      #
      
      MEDSET_FLAG <<- F
      ANN_C_FLAG <<- F
      TRUE_T_FLAG <<- F
      PHENO_DATA_FLAG <<- F
      SAMPLE_NAME_FLAG <<- F
      METH_DATA_FLAG <<- F
      
      #
      # Base Objects
      #
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
      
    }
  )
  
  shiny::runApp(app)
}
#startFactorViz(NULL)
