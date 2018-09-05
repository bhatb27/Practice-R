car_sale<- read.csv(file = 'C:/Study/SAS&R/BA/Linear & Logistic In R/car_sales.csv')
require(Hmisc)
sd(car_sale)
mystats<- function(x){
  nmiss<- sum(is.na(x))
  a<- x[!is.na(x)]
  m<- mean(a)
  L<- length(a)
  s<- sd(a)
  mi<- min(a)
  qtrs<- quantile(a,probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  ma<- max(a)
  UL<- m+3*s
  LL<- m-3*s
  outl<- ma>UL | mi<LL
  return(c(nmiss=nmiss,outlier=outl,mean=m, length=L, sd=s, min=mi, pctl=qtrs, max=ma, UpperL=UL, LowerL=LL))
}

vars <- c( "Sales_in_thousands" , "X__year_resale_value" ,  "Price_in_thousands",   
           "Engine_size" , "Horsepower", "Wheelbase" , "Width" ,"Power_perf_factor" , "Length" , "Curb_weight" , 
           "Fuel_capacity", "Fuel_efficiency" )


stats<- t(data.frame(apply(car_sale[vars],2,mystats)))
write.csv(stats,file = "C:/Study/SAS&R/BA/experimental/stats.csv")

car_sale$Sales_in_thousands[car_sale$Sales_in_thousands>257.09]<- 260.64532
car_sale$X__year_resale_value[car_sale$X__year_resale_value>52.4331275]<- 52.4331275
car_sale$Price_in_thousands[car_sale$Price_in_thousands>70.44571441]<- 70.44571441
mydata<- data.frame(apply(car_sale[vars],2,function(x) impute(x,mean)))

mydata1<- cbind(mydata,vehicle_type=car_sale$Vehicle_type)
mydata1$vehicle_type<- factor(mydata1$vehicle_type)

hist(mydata1$Sales_in_thousands)
hist(log(mydata1$Sales_in_thousands))
mydata1$Ln_sales<- log(mydata1$Sales_in_thousands)

set.seed(78235)
abc<- sample(1:nrow(mydata1), size = floor(0.70*nrow(mydata1)))

traindata<- mydata1[abc, ]
testdata<- mydata1[-abc, ]

require(corrplot)
install.packages("corrplot")
corrplot(cor(mydata1[, vars],use = "pairwise.complete.obs"), method = "triangle", t1.cx=0.7)

warnings()
scatterplotMatrix(mydata1)

fit<- lm(Ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
         +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+vehicle_type, data=traindata)
summary(fit)

vif(fit)

step1 <- stepAIC(fit, direction = "both")
ls(step1)
step1$anova

fit2<- lm(Ln_sales ~ X__year_resale_value + Price_in_thousands + Engine_size + 
            Length + Fuel_capacity + Fuel_efficiency + vehicle_type, data = traindata)
summary(fit2)
vif(fit2)

fit3<- lm(Ln_sales ~ X__year_resale_value + Price_in_thousands + 
            Length + Fuel_capacity + Fuel_efficiency + vehicle_type, data = traindata)
summary(fit3)
vif(fit3)

fit4<- lm(ln_sales~ Price_in_thousands + Engine_size + 
            Wheelbase +  Fuel_efficiency + 
            Vehicle_type, data = traindata)
summary(fit4)
vif(fit4)

t1<- cbind(traindata, pred_sales=exp(predict(fit4, traindata)))
View(t2)
?transform

t2<- transform(t2, APE=abs(pred_sales - Sales_in_thousands)/Sales_in_thousands)

t2<- cbind(testdata, pred_sales=exp(predict(fit4, testdata)))

decile1<- quantile(t2$pred_sales, probs = seq(0.1,0.9, by=0.1))
t2$decile<- findInterval(t2$pred_sales, c(-Inf, decile1, Inf))

check2<- sqldf("select decile, count(decile) as cnt_decile, avg(pred_sales) as avg_pred_sales,
               avg(sales_in_thousands) as avg_act_sales
               from t2
               group by decile
               order by decile desc")

View(check2)

coefficients(fit4)
confint(fit4, level = 0.9)
influence(fit4)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit4)

























