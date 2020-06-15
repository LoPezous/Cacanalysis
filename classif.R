rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

library(mlr)
library(ggplot2)
library(glmnet)

classif <- function(file_name.pdf, df, feature_selection_method_list, filter_perc, method , class_target) {
  df[is.na(df)] = 0
  #TASK
  task = makeClassifTask(data = df, target = class_target)
  
  #FEATURE SELECTION
  for (feature_selection_method in feature_selection_method_list){
    
    fv = generateFilterValuesData(task, method = feature_selection_method )
    filtered.task = filterFeatures(task, fval = fv, perc = filter_perc)
    print(fv)
    fv_data = fv$data
    fvdf = as.data.frame(fv$data)
    
    #PLOT FILTER VALUES
    pdf(file = paste0(feature_selection_method,"_","fv.pdf"))
    print(ggplot(fvdf, aes(x= name, y = value))+
            geom_col()+
            theme_bw())
    dev.off()
    write.table(x=fvdf, file = paste0(feature_selection_method,"_","fv.txt"))
    #LEARNER
    methoddf = listLearners("classif", properties = c("prob"))
    '%notin%' <- Negate('%in%')
    
    if (method %notin% methoddf$class) {
      return("method does not fit") 
    } else {
      
      base_learner = makeLearner(method, predict.type = "prob")
      learner = makeFilterWrapper(learner = base_learner, fw.method = feature_selection_method, fw.perc = filter_perc)
      
      
      #TRAINING
      training = train(learner, filtered.task)
      print(getFilteredFeatures(training))
      #PREDICTION
      prediction = predict(training, newdata = df)
      prediction_df = as.data.frame(prediction)
      prediction_df$id = 1:nrow(prediction_df)
      
      col_names = c("truth", "prob.class", "1-prob.class", "response","id")
      colnames(prediction_df) = col_names
      
      #PERFORMANCE
      print(performance(prediction))
      
      #PLOT
      pdf(file = file_name.pdf)
      print(ggplot(prediction_df, aes(x = id, y = prob.class -0.5, fill = truth))+
              geom_col()+
              theme_bw())
      dev.off()
      print(ggplot(prediction_df, aes(x = id, y = prob.class -0.5, fill = truth))+
              geom_col()+
              theme_bw())
      #OUTPUT
      output = prediction_df
      
      return(output)
      
    }
    
  }
  
  
}


##TEST
classif("traitement.pdf", TLUPI ,list("FSelectorRcpp_information.gain","anova.test"), 0.8, "classif.glmnet", "treatment")
