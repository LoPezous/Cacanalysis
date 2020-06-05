rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)
library(mlr)
library(ggplot2)

classfonction <- function(file_name.pdf, df, methode, class_target) {
  
  #TASK
  
  task = makeClassifTask(data = df, target = class_target)
  
  #LEARNER
  methoddf = listLearners("classif", properties = c("prob"))
  '%notin%' <- Negate('%in%')
  
  if (methode %notin% methoddf$class) {
    return("method does not fit") 
  } else {
    learner = makeLearner(methode, predict.type = "prob")
    
    #TRAINING
    training = train(learner, task)
    
    #PREDICTION
    prediction = predict(training, newdata = df)
    prediction_df = as.data.frame(prediction)
    prediction_df$id = 1:nrow(prediction_df)
    col_names = paste("prob", 1:4, sep ="")
    colnames(prediction_df) = col_names
    
    
    #PLOT
    pdf(file = file_name.pdf)
    print(ggplot(prediction_df, aes(x = prob1, y = prob3 -0.5, fill = prob2), col = "black")+
      geom_col())+
      theme_bw()
    dev.off()
    #OUTPUT
    output = write.table(prediction_df)
    
    return(output)
    
  }
  #
}

#TEST
iris_subset <- subset(iris, Species != "virginica")
classfonction("test.pdf", iris_subset, "classif.logreg", "Species")
