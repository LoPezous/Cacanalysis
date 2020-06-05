rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)
library(mlr)
library(ggplot2)
LUP1 = subset(LUP_001_profile, select = c(V1, V3))
LUP2 = subset(LUP_002_profile, select = c(V1, V3))              
LUP3 = subset(LUP_003_profile, select = c(V1, V3))
LUP1$id = 1:nrow(LUP1)
LUP2$id = 1:nrow(LUP2)

#MERGE SIMPLE

LUP = merge(LUP1, LUP2, by = c("V1"), all.x = TRUE, all.y = TRUE)



#FONCTION MERGE DE DATAFRAME NOMBRE VARIABLE
#erreur : Error in x[1, dim(x)] : nombre de dimensions incorrect

multimerge = function(x){
  x <- list()
  y <- x[1, dim(x)]
  for (i in x){
    for (j in y){
      LUP = merge(i, j, by = c("V1"), all.x = TRUE, all.y = TRUE)
      
    }
  }
  return(LUP)
}

#TEST
multimerge(list(LUP1,LUP2,LUP3))  
  
  
  
  
  


