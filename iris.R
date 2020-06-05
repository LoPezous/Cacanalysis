rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)
library(mlr)
library(ggplot2)

#DATA
iris_subset <- subset(iris, Species != "virginica")
iris_subset$id = 1:100


#TASK
iris_task = makeClassifTask(data = iris_subset, target = "Species")

#LEARNER
logreg_learner = makeLearner("classif.logreg", predict.type = "prob")

#TRAINING
logregiris_training = train(logreg_learner, iris_task)



#PREDICTION
predlogreg = predict(logregiris_training, newdata = iris_subset)

predframe = as.data.frame(predlogreg)
predframe$id = 1:100

#PLOT
pdf(file = "vertical_plot.pdf")
ggplot(predframe, aes(x = id, y = prob.setosa-0.5, col = response))+
  geom_col()
dev.off()






