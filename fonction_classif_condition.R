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
    #col_names = paste("prob", 1:4, sep ="")
    col_names = c("truth", "prob.class", "1-prob.class", "response","id")
    colnames(prediction_df) = col_names
    
    
    #PLOT
    pdf(file = file_name.pdf)
    print(ggplot(prediction_df, aes(x = id, y = prob.class -0.5, fill = response))+
      geom_col())+
      theme_bw()
    dev.off()
    #OUTPUT
    output = write.table(prediction_df)
    
    return(output)
    
  }
  
  #return(list(output, perfo))
}

#TEST
LUPI = merge_OTU("C:/Users/marti/Desktop/StageI3/LUPILDF")



classfonction("test.pdf", LUPI, "classif.glmnet", "class")
LUPIT = t(LUPI)
class = c("NA",rep("class1",10),rep("class2",10))
TLUPI = as.data.frame(LUPIT)
TLUPI$class = class
