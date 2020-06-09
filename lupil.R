rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)
lupil$Treatment = gsub("ILT-101", "IL2", lupil$Treatment)
lupil$Treatment = gsub("Placebo", "PL", lupil$Treatment)

lupil$file = rownames(lupil)
lupil = lupil[c("Treatment","file")]
