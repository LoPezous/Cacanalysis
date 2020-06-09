rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

library(mlr)
library(ggplot2)
library(glmnet)

classfonction <- function(file_name.pdf, df, feature_selection_method, method, class_target) {
  df[is.na(df)] = 0
  #TASK
  task = makeClassifTask(data = df, target = class_target)
  
  #FEATURE SELECTION
  fv = generateFilterValuesData(task, method = feature_selection_method )
  filtered.task = filterFeatures(task, fval = fv, perc = 0.25)
  
  #LEARNER
  methoddf = listLearners("classif", properties = c("prob"))
  '%notin%' <- Negate('%in%')
  
  if (method %notin% methoddf$class) {
    return("method does not fit") 
  } else {
    
    base_learner = makeLearner(method, predict.type = "prob")
    learner = makeFilterWrapper(learner = base_learner, 
                            fw.method = "FSelectorRcpp_information.gain", fw.perc = 0.25)
    
    #TRAINING
    training = train(learner, filtered.task)
    
    #PREDICTION
    prediction = predict(training, newdata = df)
    prediction_df = as.data.frame(prediction)
    prediction_df$id = 1:nrow(prediction_df)
    
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

#TRAITEMENT DE LA DATAFRAME AVANT CLASSIF
LUPI = merge_OTU("C:/Users/marti/Desktop/StageI3/LUPILDF")

LUPI_duplicate = LUPI #conserve species
LUPI$species = NULL #sinon problème quand transpose
LUPIT = t(LUPI) #transpose 
#class = c(rep("class1",10),rep("class2",10)) classes arbitraires pour test
TLUPI = as.data.frame(LUPIT) #reconversion en df
colnames(TLUPI) = LUPI_duplicate$species #colnames = nom OTU
#TLUPI$class = class
TLUPI$file = rownames(TLUPI) #noms des fichiers dans une colonne pour merge avec lupil
TLUPI = merge(lupil, TLUPI, by = c("file"),all = TRUE ) #merge selon file
TLUPI$file = NULL #en dernier pour que la colonne file ne devienne pas une feature

#TEST
classfonction("test.pdf", TLUPI,"FSelectorRcpp_information.gain", "classif.glmnet", "Treatment")
