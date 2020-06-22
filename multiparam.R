multimethods <- function(param, df, class_target,filename){
  for (i in 1:nrow(param)){
    
    classfonction(filename,df,param[i,],class_target)
    
      
    
  }
}


multimethods(parameters,TLUPI,"treatment","test.pdf")

