rm(list = ls())
mydata<- read.csv(file = 'C:/Study/SAS&R/BA/Linear & Logistic In R/bankloans.csv')

str(mydata)
mystat<- function(x){
  if(class(x)=="numeric"){
    var_type=class(x)
    n=length(x)
    nmiss=sum(is.na(x))
    a<- x[!is.na(x)]
    m=mean(a)
    std=sd(a)
    v=var(a)
    mi=min(a)
    qtr<- quantile(a, probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
    q3<- quantile(a, probs = 0.75)
    q1<- quantile(a, probs = 0.25)
    ma=max(a)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Type=var_type, n=n, nmiss=nmiss, mean=m, std=std, variance=v,
           min=mi, qtr=qtr, max=ma, outl1=ot1, outl2=ot2, outl3=ot3))
  }
  else{
    var_type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}


var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}
?IQR

sapply(mydata, is.numeric)
str(mydata)
mydata$ed<- factor(mydata$ed)

num_var<- sapply(mydata, is.numeric)
oth_var<- !sapply(mydata, is.numeric)

my_num_stat<- t(data.frame(apply(mydata[num_var],2, var_Summ)))
my_cat_stat<- t(data.frame(apply(mydata[oth_var],2, var_Summ)))
View(my_num_stat)

mycust<- mydata[!is.na(mydata$default),]
new_cust<- mydata[is.na(mydata$default),]

mycust[,num_var] <- impute(mycust[,num_var],fun = "mean")
View(mycust)

apply(is.na(mycust),2, sum)

m1_qu<- function(x){
  quantiles<- quantile(x, probs = c(0.01,0.99), na.rm = T)
  x[x < quantiles[1]]<- quantiles[1]
  x[x > quantiles[2]]<- quantiles[2]
  x
}
mycust1<- mycust

mycust1[,-2]<- apply(mycust1[,-2,],2,m1_qu)
mycust1[,-2]

mycust[,3]<- m1_qu(mycust[,3])
rm(mycust1)

set.seed(54321)
train_ind<- sample(1:nrow(mycust), size = floor(0.7*nrow(mycust)))
traindata<- mycust[train_ind, ]
testdata<- mycust[-train_ind, ]

str(traindata)

fit<- glm(default ~ ., data = traindata, family = binomial(logit))
summary(fit)

step1<- step(fit)

fit1<- glm(default ~ age + employ + address + debtinc + creddebt, data = mycust,family = binomial(logit))
summary(fit1)

traindata<- cbind(traindata, pred=predict(fit1,traindata,type = "response"))
View(traindata)

deciles<- quantile(traindata$pred, probs = c(seq(0.1, 0.9, by=0.1)))

abc<- findInterval(traindata$pred, deciles)
abc
traindata$decile<- findInterval(traindata$pred, c(-Inf, deciles, Inf))

traindata$decile<- factor(traindata$decile)
str(traindata)

require(dplyr)
decile_grp<- group_by(traindata, decile)
decile_sum_train<- summarise(decile_grp, total_cnt=n(), Min_prob=min(pred), max_prob=max(pred),
                            default_cnt=sum(default), non_def_cnt=total_cnt-default_cnt)
View(decile_sum_train)

fit_train_DA <- sqldf("select decile, min(pred) as Min_prob
                       , max(pred) as max_prob
                      , sum(default) as default_Count
                      , (count(decile)-sum(default)) as Non_default_Count 
                      from traindata
                      group by decile
                      order by decile desc")

table(traindata$pred>0.38, traindata$default)
table(traindata$pred>0.25, traindata$default)
table(traindata$pred>0.193, traindata$default)

testdata<- cbind(testdata, pred=predict(fit1, testdata, type = "response"))
table(testdata$pred>0.25, testdata$default)

new_cust$pred<- predict(fit1, new_cust, type = "response")
new_cust$default<- ifelse(new_cust$pred>0.25,1,0)
table(new_cust$default)

train1<- cbind(traindata, Prob=predict(fit, type="response")) 
View(train1)
require(ROCR)
pred_train_fit2 <- prediction(train1$Prob, train1$default)
perf_fit2 <- performance(pred_train_fit2, "tpr", "fpr")
plot(perf_fit2)
abline(0, 1)
performance(pred_train_fit2, "auc")@y.values
