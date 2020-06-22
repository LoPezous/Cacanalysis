options(stringsAsFactors = FALSE)
parameters = data.frame()
method_fs = c(as.character("anova.test"),as.character("auc"),as.character("FSelectorRcpp_information.gain"),as.character("kruskal.test"))
method_classif = c(as.character("classif.glmnet"),as.character("classif.ada"),as.character("classif.logreg"),as.character("classif.binomial"))
thresh_fs = c(0.05,0.1,5,10)
thresh_classif = c(0.5,0.6,0.7,0.8)

parameters = expand.grid(method_fs,method_classif,thresh_fs,thresh_classif)
colnames(parameters) = c("method_fs","method_classif","thresh_fs","thresh_classif")
parameters$method_fs = as.character(parameters$method_fs)
parameters$method_classif = as.character(parameters$method_classif)
param_thresh_fs = parameters$thresh_fs
param_thresh_fs[1,1]
x = parameters[1,]



write.table(x = parameters, file = "parameters.txt")
