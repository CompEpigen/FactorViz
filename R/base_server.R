# This script contains the basic server interface of FactorViz, server code related to 
# ouptut, reactive elements and output can be found inside server folder.

base_server<-function(input, output) {
  source('server/getters.R', local = T)
  source('server/output.R', local = T)
  source('server/reactive.R', local=T)
  source('server/plot.R', local=T)
  shinyDirChoose(input, 'dir', roots=c(computer=("/"), home=paste('/home/', CURRENT_USER, "/", sep="")), filetypes=c('', 'RData') )
  df<-eventReactive(input$load, {
    withProgress(message = 'Loading datasets in progress\n',
                 detail = 'This may take a while...', value = 0, style = getShinyOption('old'),
                 {
                   home <- normalizePath("~")
                   rel_path<-paste(unlist(dir()$path[-1]), collapse ='/')
                   if (file.exists(paste(home, rel_path, MEDSET, sep = "/"))){
                     load(paste(home, rel_path, MEDSET, sep = "/"), envir=globalenv())
                     medecom_object<<-medecom.set
                     MEDSET_FLAG<<-T
                   }
                   incProgress(1/4)
                   if (file.exists(paste(home, rel_path, ANN_C, sep = "/"))){
                     load(paste(home, rel_path, ANN_C, sep = "/"), envir=globalenv())
                     cg_annot_object<<-ann.C
                     ANN_C_FLAG<<-T
                   }
                   incProgress(1/4)
                   if (file.exists(paste(home, rel_path, ANN_S, sep = "/"))){
                     load(paste(home, rel_path, ANN_S, sep = "/"), envir=globalenv())
                     input_object$pheno<<-ann.S
                     PHENO_DATA_FLAG<<-F
                     if("sample_id" %in% colnames(ann.S)){
                       input_object$sample.names<<-ann.S$sample_id
                     }else{
                       input_object$sample.names<<-1:nrow(ann.S) 
                     }
                     SAMPLE_NAME_FLAG<<-T
                   }
                   incProgress(1/4)
                   if (file.exists(paste(home, rel_path, REF, sep = "/"))){
                     load(paste(home, rel_path, REF, sep = "/"), envir=globalenv())
                     true_T_matrix<<-ref.meth
                     TRUE_T_FLAG<<-T
                   }
                   
                   incProgress(1/4)
                })
  })
}
