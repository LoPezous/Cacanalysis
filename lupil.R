rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)
lupil$Treatment = gsub("ILT-101", "IL2", lupil$Treatment)
lupil$Treatment = gsub("Placebo", "PL", lupil$Treatment)
lupil$Responder[lupil$Responder == "TRUE"] <- "R"
lupil$Responder[lupil$Responder == "FALSE"] <- "NR"
lupil$sampleID = paste0(lupil$Treatment,"_",lupil$Responder,"_",lupil$ID)
lupil$file = rownames(lupil)

