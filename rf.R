train<- read.table(choose.files(), header = T, sep = ",")
library(rpart)
library(graphics)
library(randomForest)
library(h2o)

test<- read.table(choose.files(), header = T, sep = ",")
rm(list = ls())
rpartfit<- rpart(Status_Final ~ ., data = train)
pred<- predict(rpartfit, test, type = "class")
table(pred, test$Status_Final)
mean(pred==test$Status_Final)

library(randomForestSRC)
library(caret)

cfit<- train(Status_Final~., method="rf", data= train)
pred.rf<- predict(cfit, newdata = test)
table(pred.rf, test$Status_Final)
mean(pred.rf==test$Status_Final)

library(adabag)
adafit<- boosting(Status_Final~., data = train, boos = T, mfinal = 100, coeflearn = 'Breiman')
pred.ad<- predict(adafit, test)
table(pred.ad$class, test$Status_Final)
mean(pred.ad$class==test$Status_Final)
summary(pred.ad)

h2o.init(nthreads = -1, max_mem_size = "4G")
h2o.removeAll()
traindata<- h2o.importFile(path = normalizePath("C:/Study/SAS&R/BA/5. Ensemble Learning/InputDT.csv"))
testdata<- h2o.importFile(path = normalizePath("C:/Study/SAS&R/BA/5. Ensemble Learning/TestDT.csv"))
traindata<- h2o.assign(traindata, "traindata.hex")
testdata<- h2o.assign(testdata, "testdata.hex")

rf1<- h2o.randomForest(
  training_frame = traindata,
  validation_frame = testdata,
  x=2:13,
  y=1,
  model_id = "rf_covType_v1",
  ntrees = 100,
  stopping_rounds = 2,
  score_each_iteration = T,
  seed = 1500000
)
summary(rf1)
rf1@model$validation_metrics@metrics$Gini

pred.rf1<- h2o.predict(object = rf1, newdata = testdata)
mean(pred.rf1$predict==testdata$Status_Final)

rf2<- h2o.randomForest(
  training_frame = traindata,
  validation_frame = testdata,
  x= 2:13, y= 1, model_id = "rf_covType2",
  ntrees = 50, max_depth = 5, stopping_rounds = 15,
  stopping_tolerance = 0.0001, score_each_iteration = T, seed = 1234567
)
summary(rf2)
rf2@model$validation_metrics@metrics$AUC
rf2@model$validation_metrics@metrics$Gini

pred.rf2<- h2o.predict(object = rf2, newdata = testdata)
table(pred.rf2$predict, testdata$Status_Final)

mean(pred.rf2$predict==testdata$Status_Final)

gbm1<- h2o.gbm(
  training_frame = traindata,
  validation_frame = testdata,
  x=2:13, y= 1, model_id = "gbm_covType1",
  seed = 12345
)

pred.gbm<- h2o.predict(object = gbm1, newdata = testdata)
mean(pred.gbm$predict==testdata$Status_Final)

gbm2<- h2o.gbm(
  training_frame = traindata, validation_frame = testdata,
  x= 2:13, y= 1, ntrees = 50, learn_rate = 0.15,
  max_depth = 3, sample_rate = 0.5, col_sample_rate = 0.85,
  stopping_rounds = 25, stopping_tolerance = 0.0005,
  score_each_iteration = T, model_id = "gbm_covType3",
  seed = 123456)

gbm2@model$validation_metrics@metrics$AUC
gbm2@model$validation_metrics@metrics$Gini

pred.gbm2<- h2o.predict(object = gbm2, newdata = testdata)
mean(pred.gbm2$predict==testdata$Status_Final)
View(testdata$Status_Final)
pred.gbm2$predict

h2o.shutdown()
Y





