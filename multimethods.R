multimethods <- function(feature_method_list, classif_method_list, file_name.pdf, df, filter_perc, class_target){
  for (feature_selection_method in feature_method_list){
    for (classif_method in classif_method_list){
      classfonction(file_name.pdf,df, feature_selection_method, filter_perc, classif_method,class_target)
      
      
    }
  }
}

#TEST#
multimethods(list("FSelectorRcpp_information.gain","anova.test","auc","kruskal.test"), list("classif.glmnet","classif.ada","classif.logreg","classif.binomial"),"test.pdf", TLUPI, 0.3, "treatment")
multimethods(list(as.character(parameters[1,]$method_fs), as.character(parameters[2,]$method_fs)),list(as.character(parameters[1,]$method_classif),as.character(parameters[2,]$method_classif)), "test.pdf", TLUPI, 0.3, "treatment")
