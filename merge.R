rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

merge_OTU = function(repertoire){
  files = dir(repertoire, full.names = TRUE)
  col_names = c("species", "unknown","abundance", "coverage", "nreads")
  df = data.frame(col1 = character(),
                  col2 = numeric(),
                  col3 = numeric(),
                  col4 = numeric(),
                  col5 = numeric())
  colnames(df) = col_names
  
  
  for (file in files){
  
    dfx <- read.delim(file, header = FALSE, skip = 5) 
    colnames(dfx) = col_names 

    df = merge(dfx, df, by = c("species"), all = TRUE)
    
  }
  return(df)
  
}
LUPI = merge_OTU("C:/Users/marti/Desktop/StageI3/LUPILDF")
LUPI
