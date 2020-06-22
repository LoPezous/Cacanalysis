classfonction <- function(file_name.pdf, df, x , class_target) {
  df[is.na(df)] = 0
  #TASK
  task = makeClassifTask(data = df, target = class_target)
  
  #FEATURE SELECTION
  fv = generateFilterValuesData(task, method = x$method_fs)
  filtered.task = filterFeatures(task, fval = fv, threshold = x$thresh_fs )
  fv_data = fv$data
  fvdf = as.data.frame(fv$data)
  
  #PLOT FILTER VALUES
  pdf(file = paste0(x$method_fs,"_","filtVal.pdf"))
  print(ggplot(fvdf, aes(x= name, y = value))+
          geom_col()+
          theme_bw())
  dev.off()
  write.table(x=fvdf, file = paste0(x$method_fs,"_","filtVal.txt"))
  #LEARNER
  methoddf = listLearners("classif", properties = c("prob"))
  '%notin%' <- Negate('%in%')
  
  if (x[2] %notin% methoddf$class) {
    return("method does not fit") 
  } else {
    
    base_learner = makeLearner(x$method_classif, predict.type = "prob")
    learner = makeFilterWrapper(learner = base_learner, fw.method = x$method_fs, fw.threshold = x$thresh_fs)
    
    
    #TRAINING
    training = train(learner, filtered.task)
    write.table(x = getFilteredFeatures(training), file = paste0(x$method_fs,"_",x$method_classif,"_","Ffeatures.txt"))
    
    #PREDICTION
    pred = predict(training, newdata = df)
    
    prediction = setThreshold(pred, x$thresh_classif)
    
    prediction_df = as.data.frame(prediction)
    prediction_df$id = 1:nrow(prediction_df)
    
    col_names = c("truth", "prob.class", "1-prob.class", "response","id")
    colnames(prediction_df) = col_names
    
    #PERFORMANCE
    perf = generateThreshVsPerfData(prediction, measures = list(fpr, tpr, mmce, auc))
    pdf(file = paste0("ROC","_",x$method_fs,x$method_classif,".pdf"))
    print(plotROCCurves(perf))
    dev.off()
    write.table(x= perf$data, file = paste0("perf","_",x$method_fs,x$method_classif,x$thresh_fs,x$thresh_classif, ".txt"))
    print(perf)
    
    
    #PLOT
    pdf(file = paste0("plot",x$method_fs,"_",x$method_classif,"_",x$thresh_fs,"_",x$thresh_classif,"_",file_name.pdf))
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

classfonction("test.pdf",TLUPI, parameters[1,], "treatment")
