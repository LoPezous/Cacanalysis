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
    col_namesdfx = c("species", "unknown",basename(file), "coverage", "nreads")
    colnames(dfx) = col_namesdfx 
    dfx = dfx[c("species", basename(file))]
    colnames(dfx) = gsub("_profile.txt","", colnames(dfx))
    df = merge(dfx, df, by = c("species"), all = TRUE)
    
  }
  df$abundance = NULL
  df$unknown = NULL
  df$coverage = NULL
  df$nreads = NULL
  return(df)
  
}
LUPI = merge_OTU("C:/Users/marti/Desktop/StageI3/LUPILDF")
LUPI
