rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

library(mlr)
library(ggplot2)
library(glmnet)

classfonction <- function(file_name.pdf, df, feature_selection_method, filter_perc, method, class_target) {
  df[is.na(df)] = 0
  #TASK
  task = makeClassifTask(data = df, target = class_target)
  
  #FEATURE SELECTION
  fv = generateFilterValuesData(task, method = feature_selection_method )
  filtered.task = filterFeatures(task, fval = fv, perc = filter_perc)
  print(fv)
  
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
      geom_col())+
      theme_bw()
    dev.off()
    print(print(ggplot(prediction_df, aes(x = id, y = prob.class -0.5, fill = truth))+
                  geom_col())+
            theme_bw())
    #OUTPUT
    output = write.table(prediction_df)
    
    return(output)
    
  }
  
}


#TEST
classfonction("traitement.pdf", TLUPI ,"FSelectorRcpp_information.gain", 0.25, "classif.glmnet", "treatment")


