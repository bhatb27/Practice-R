library(C50)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(rattle)
library(RColorBrewer)

churndata<- read.csv(file = "C:/Study/SAS&R/BA/Decision Trees Codes & Files/churnTrain.csv", header= T)
fit<- rpart(churn ~ ., data = churndata, method = "class")
summary(fit)
fancyRpartPlot(fit, cex=0.5)

printcp(fit)
pfit<- prune(fit, cp=0.02381)
fancyRpartPlot(pfit, cex=0.6)

t<- cbind(churndata, pred= predict(pfit, newdata = churndata, type = "class"))
table(t$churn, t$pred)

rfit<- C5.0(churn ~ ., data = churndata)
t1<- cbind(churndata, pred=predict(rfit, newdata = churndata, type = "class"))
table(t1$churn, t1$pred)
summary(rfit)
cfit<- ctree(churn ~ ., data = churndata, control = ctree_control(maxdepth = 4))
summary(cfit)
plot(cfit)
t2<- cbind(churndata, pred2=predict(cfit, newdata = churndata))
table(t2$churn, t2$pred2)


