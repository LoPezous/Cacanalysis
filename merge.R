rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

multimerge = function(repertoire){
  files = dir(repertoire, full.names = TRUE)
  #files = list.files(filesdir)
  col_names = c("species", "V2","abundance", "coverage", "nreads")
  df = data.frame(col1 = character(),
                  col2 = numeric(),
                  col3 = numeric(),
                  col4 = numeric(),
                  col5 = numeric())
  colnames(df) = col_names
  
  
  for (x in files){
    dfx <- read.delim(x,header = FALSE, skip = 5) 
    colnames(dfx) = col_names 

    out = merge(dfx, df, by = c("species"), all = TRUE)
    
  }
  return(out)
  
}
LUP = multimerge("C:/Users/marti/Desktop/StageI3/LUPILDF")
LUP
