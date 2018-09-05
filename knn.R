mydata<- read.csv(file = "C:/Study/SAS&R/BA/8. KNN/reg data2.5.csv")
require(class)

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

vars <- c( "SeriousDlqin2yrs" , "RevolvingUtilizationOfUnsecuredLines" ,  "age",   
           "NumberOfTime30.59DaysPastDueNotWorse" , "DebtRatio", "MonthlyIncome" , "NumberOfOpenCreditLinesAndLoans" ,
           "NumberOfTimes90DaysLate" , "NumberRealEstateLoansOrLines" , "NumberOfTime60.89DaysPastDueNotWorse" , 
           "NumberOfDependents")

diag_stats<-t(data.frame(apply(mydata[vars], 2, mystats)))


#Outliers
mydata$age[mydata$age>87]<- 87
mydata$RevolvingUtilizationOfUnsecuredLines[mydata$RevolvingUtilizationOfUnsecuredLines>1]<- 1
mydata$NumberOfTime30.59DaysPastDueNotWorse[mydata$NumberOfTime30.59DaysPastDueNotWorse>12.9993772]<- 13
mydata$DebtRatio[mydata$DebtRatio>6466.4650758]<- 6466.4650758
mydata$NumberOfOpenCreditLinesAndLoans[mydata$NumberOfOpenCreditLinesAndLoans>23.890613]<- 24
mydata$NumberOfTimes90DaysLate[mydata$NumberOfTimes90DaysLate>12.7738847]<- 13
mydata$NumberRealEstateLoansOrLines[mydata$NumberRealEstateLoansOrLines>4.407553]<- 4
mydata$NumberOfTime60.89DaysPastDueNotWorse[mydata$NumberOfTime60.89DaysPastDueNotWorse>12.7059249]<- 13
mydata$MonthlyIncome[mydata$MonthlyIncome>45311.57]<- 45311.57
mydata$NumberOfDependents[mydata$NumberOfDependents>4]<- 4

summary(mydata)

#missing values
mydata$MonthlyIncome[is.na(mydata$MonthlyIncome ==  TRUE)] <- 6460
mydata$NumberOfDependents[is.na(mydata$NumberOfDependents ==  TRUE)] <- 1
mydata$age[mydata$age==0]<- 52


set.seed(125)
smp_size <- floor(0.70 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
training <- mydata[train_ind, ]
testing <- mydata[-train_ind, ]

trainset$SeriousDlqin2yrs <- factor(trainset$SeriousDlqin2yrs) 
testset$SeriousDlqin2yrs <- factor(testset$SeriousDlqin2yrs) 

set.seed(450)
ctrl<- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
)

warnings()
plot(knnfit)
knnpredict<- predict(knnfit, testset)
table(knnpredict, testset$SeriousDlqin2yrs)
mean(knnpredict==testset$SeriousDlqin2yrs)









