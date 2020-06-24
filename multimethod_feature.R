multimethods <- function(param, df, class_target,filename){
  filtered_features = data.frame()
  fm = data.frame()
  for (i in 1:nrow(param)){
    
    result = classfonction(filename,df,param[i,],class_target)
    
    filtered_features = rbind(filtered_features, cbind(result))
    
    rep_times = nrow(as.data.frame(result))
    method_vector = rep(param[i,]$method_fs, rep_times)
    
    fm = rbind(fm,cbind(method_vector))
    
    
    
    
    
  }
  filtered_features = cbind(filtered_features,fm)
  print(filtered_features)
  write.table(x = filtered_features, file = "FFF.txt")
}


multimethods(parameters,timepoint,"timepoint","test.pdf")
multimethods(dualparam,IL2_only_RESPONSE,"responder","response.pdf")
multimethods(dualparam,IL2_only_RESPONSE,"responder","response.pdf")
