multimethods <- function(feature_method_list, classif_method_list, file_name.pdf, df, filter_perc, class_target){
  for (feature_selection_method in feature_method_list){
    for (classif_method in classif_method_list){
      classfonction(file_name.pdf,df, feature_selection_method,0.8,classif_method,class_target)
    }
  }
}

#TEST
multimethods(list("FSelectorRcpp_information.gain","anova.test"), list("classif.glmnet","classif.logreg"),"test.pdf", TLUPI, 0.8, "treatment")
