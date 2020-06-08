rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

multimerge = function(repertoire){
  files = dir(repertoire, full.names = TRUE)
  #files = list.files(filesdir)
  col_names = c("species", "V2","abundance", "V4", "V5")
  df = data.frame(col1 = character(),
                  col2 = numeric(),
                  col3 = numeric(),
                  col4 = numeric(),
                  col5 = numeric())
  colnames(df) = col_names
  
  
  for (x in files){
    dfx <- read.delim(x) #more columns than column names (problème de sep = ?)
    colnames(dfx) = col_names 
    
    print(x)
    out = merge(dfx, df, by = c("species"), all = TRUE)
    
  }
  return(out)
  
}
multimerge("C:/Users/marti/Desktop/StageI3/LUPILDF")
