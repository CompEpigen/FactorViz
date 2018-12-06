fitPhenotypeModel<-function(Ahat, pheno_data, target_var, adj_vars, zero_level, discardLMC){
  
  largest<-which.max(rowMeans(Ahat))
  smallest<-which.min(rowMeans(Ahat))
  
  if(is.integer(pheno_data[[target_var]])){
    exclude<-is.na(pheno_data[[target_var]])
  }else if(is.double(pheno_data[[target_var]])){
    exclude<-is.na(pheno_data[[target_var]])
  }else if(is.factor(pheno_data[[target_var]])){
    exclude<-is.na(pheno_data[[target_var]]) | as.character(pheno_data[[target_var]])  %in% c("exclude", "Exclude", "exl", "Excl")
  }else if(is.character(pheno_data[[target_var]])){
    exclude<-is.na(pheno_data[[target_var]]) | pheno_data[[target_var]] %in% c("exclude", "Exclude", "exl", "Excl")
  }else{
    exclude<-rep(FALSE, nrow(pheno_data))
  }
  
  dataSet<-as.list(pheno_data[which(!exclude),,drop=FALSE])
  Ahat<-Ahat[,which(!exclude)]

  if(!is.numeric(dataSet[[target_var]])){
    if(is.character(dataSet[[target_var]])){
      if(!any(is.na(as.integer(dataSet[[target_var]])))){
        dataSet[[target_var]]<-as.integer(dataSet[[target_var]])
      }else{
        dataSet[[target_var]]<-as.factor(dataSet[[target_var]])
      }
    }
    if(is.factor(dataSet[[target_var]])){
      target_levels<-levels(dataSet[[target_var]])
      target_levels<-c(zeroLevel, setdiff(target_levels, zeroLevel))
      dataSet[[target_var]]<-factor(dataSet[[target_var]], levels=target_levels)
      dataSet$target_quant<-as.integer(dataSet[[target_var]])
      dataSet$target_quant[dataSet$target_quant>1]<-2L
    }
  }else{
    dataSet$target_quant<-as.integer(dataSet[[target_var]])
  }
  
  if(discardLMC=="largest"){
    exc_lmc<-largest
  }else if(discardLMC=="smallest"){
    exc_lmc<-smallest
  }
  
  for(cmp in seq_len(nrow(Ahat))[-exc_lmc]){
    dataSet[[sprintf("LMC%d",cmp)]]<-Ahat[cmp,]
  }
  
  theModel<-as.formula(sprintf("target_quant~%s", 
                               paste(c(sprintf("LMC%d", seq_len(nrow(Ahat))[-exc_lmc]), adj_vars),collapse="+")))		
  
  dump("dataSet", file='/tmp/dataSet.RDump')
  
  fit<-lm(theModel,  data=dataSet)
}