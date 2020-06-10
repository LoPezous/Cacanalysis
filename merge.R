
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
  #ENLEVE RESIDUS DES COLONNES DE "DF" VIDE
  df$abundance = NULL
  df$unknown = NULL
  df$coverage = NULL
  df$nreads = NULL
  #FLIP
  df_duplicate = df #conserve species
  df$species = NULL #sinon problème quand transpose
  df = t(df) #transpose 
  df = as.data.frame(df) #reconversion en df
  colnames(df) = df_duplicate$species #colnames = nom OTU
  df$file = rownames(df) #noms des profils dans une colonne
  #
  rownames(df) = lupil[lupil$file == df$file]$sampleID
  
  return(df)
  
}
LUPI = merge_OTU("C:/Users/marti/Desktop/StageI3/LUPILDF")
LUPI
