rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)
library(mlr)
library(ggplot2)
LUP1 = subset(LUP_001_profile, select = c(V1, V3))
LUP2 = subset(LUP_002_profile, select = c(V1, V3))              
LUP3 = subset(LUP_003_profile, select = c(V1, V3))
LUP4 = subset(LUP_004_profile, select = c(V1, V3))

#MERGE SIMPLE

LUP = merge(LUP1, LUP2, by = c("V1"), all.x = TRUE, all.y = TRUE)



#MULTIMERGE W/ LOOP
#erreurloop verte : Error in x[1, dim(x)] : nombre de dimensions incorrect
#erreur : arguments imply differing number of rows: 253, 204, 217

multimerge = function(x,...){
  
  
  l <- list(x,...)
  #y <- l[1, dim(x)]
  for (i in l){
    #for (j in y){
      LUP = merge(i, x, by = c("V1"), all = TRUE)
      
    #}
  }
  return(LUP)
}

#MULTIMERGE W/ REDUCE()
multimergeR = function(x,...){
  LUP = Reduce(function(x, y) merge(x, y, by = c("V1"), all=TRUE), list(x,...))
  return(LUP)
}

#TEST REDUCE
LUP = multimergeR(LUP1,LUP2,LUP3,LUP4) 

#TEST LOOP
multimerge(list(LUP1,LUP2,LUP3))  
  
  
  
  
  


