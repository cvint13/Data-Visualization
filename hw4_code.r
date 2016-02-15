#BU MET CS555
#HW 4
#Multiple Regression and Residuals

#Load data into R
library(xlsx)
weight.data <- read.xlsx("C:\\Users\\cvint\\CS555\\hw4\\weightlossdata.xlsx",1)

#Scatterplot between weight loss and exercise time
#explanatory=exercise time
#response = weight loss
library(ggplot2)
scatter <- ggplot(weight.data,aes(x=ExerciseTime,y=WeightLoss)) + geom_point(colour="purple",size=3)
scatter + ggtitle("Exercise Time vs. Weight Loss") +
  theme(plot.title=element_text(face="bold"))

#calculate the correlation between the two above variables
attach(weight.data)
weight.exer.corr <- cor(x=ExerciseTime,y=WeightLoss)
weight.exer.corr
#0.5377304

#perform linear regression
scatter + ggtitle("Exercise Time vs. Weight Loss") +
  geom_line(method="lm",
            stat="smooth",
            colour="green") +
  theme(plot.title=element_text(face="bold"))

detach(weight.data)
weight.exer.lm <- lm(WeightLoss ~ ExerciseTime,data = weight.data)
weight.exer.lm
#Call:
#  lm(formula = WeightLoss ~ ExerciseTime, data = weight.data)
#
#  Coefficients:
#  (Intercept)  ExerciseTime  
#   3.06836       0.05793

summary(weight.exer.lm)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.96418 -0.57487 -0.04282  0.55927  2.80960 
#
#Coefficients:
#              Estimate   Std. Error t value  Pr(>|t|)   
#(Intercept)   3.06836    0.85607    3.584    0.00333 **
#ExerciseTime  0.05793    0.02519    2.300    0.03869 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.186 on 13 degrees of freedom
#Multiple R-squared:  0.2892,	Adjusted R-squared:  0.2345 
#F-statistic: 5.288 on 1 and 13 DF,  p-value: 0.03869


#residuals plot
plot(weight.exer.lm,which=1,main="Weight Loss vs. Exercise Residuals Plot")

#Check for normality of residual distribution
hist(weight.exer.lm$residuals,
     col = "purple",
     xlab="Residuals",
     main="Normality of Residuals")

#Note the influence of the outlier
hist(weight.exer.lm$residuals,
     col = "purple",
     xlab="Residuals",
     main="Normality of Residuals",
     breaks=10)

#Check influence of outliers
#2
attach(weight.data)
weightloss.2 <- WeightLoss[-2]
exertime.2 <- ExerciseTime[-2]
weight.data.2 <- data.frame(weightloss.2,exertime.2)
detach(weight.data)
cor.2 <- cor(y=weightloss.2,x=exertime.2)
cor.2
#0.6872559
#Check for difference in p value and Corr Coeff
lm.2 <- lm(weightloss.2~exertime.2,data=weight.data.2)
summary(lm.2)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.7584 -0.4017  0.2068  0.6030  1.2591 
#
#Coefficients:
#             Estimate   Std. Error   t value  Pr(>|t|)   
#(Intercept)  2.70435    0.66104      4.091    0.00150 **
#exertime.2   0.06303    0.01923      3.277    0.00661 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.9025 on 12 degrees of freedom
#Multiple R-squared:  0.4723,	Adjusted R-squared:  0.4283 
#F-statistic: 10.74 on 1 and 12 DF,  p-value: 0.006612



#6
attach(weight.data)
weightloss.6 <- WeightLoss[-6]
exertime.6 <- ExerciseTime[-6]
weight.data.6 <- data.frame(weightloss.6,exertime.6)
detach(weight.data)
cor(y=weightloss.6,x=exertime.6)
#0.5822528
lm.6 <- lm(weightloss.6~exertime.6,data=weight.data.6)
summary(lm.6)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.5478 -0.6178 -0.1689  0.4797  2.6667 
#
#Coefficients:
#             Estimate   Std. Error  t value   Pr(>|t|)   
#(Intercept)  3.23079    0.78865      4.097    0.00148 **
#exertime.6   0.05723    0.02307      2.481    0.02891 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.086 on 12 degrees of freedom
#Multiple R-squared:  0.339,	Adjusted R-squared:  0.2839 
#F-statistic: 6.155 on 1 and 12 DF,  p-value: 0.02891

weight.data$WeightLoss[6]
weight.data$ExerciseTime[6]

#12
attach(weight.data)
weightloss.12 <- WeightLoss[-12]
exertime.12 <- ExerciseTime[-12]
weight.data.12 <- data.frame(weightloss.12,exertime.12)
detach(weight.data)
cor(y=weightloss.12,x=exertime.12)
#0.5531925
lm.12 <- lm(weightloss.12~exertime.12,data=weight.data.12)
summary(lm.12)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.0656 -0.5675 -0.1319  0.5243  2.7046 
#
#Coefficients:
#             Estimate   Std. Error t value  Pr(>|t|)   
#(Intercept)  3.20639    0.84493    3.795    0.00255 **
#exertime.12  0.05675    0.02467    2.300    0.04017 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.161 on 12 degrees of freedom
#Multiple R-squared:  0.306,	Adjusted R-squared:  0.2482 
#F-statistic: 5.292 on 1 and 12 DF,  p-value: 0.04017
weight.data$ExerciseTime[12]
weight.data$WeightLoss[12]

#Graph with influence points
scatter.2 <- ggplot(weight.data,aes(x=ExerciseTime,y=WeightLoss)) + 
  geom_point(colour="purple",size=3) +
  geom_line(method="lm",
            stat="smooth",
            colour="green") +
  geom_line(method="lm",
            stat="smooth",
            colour="pink",
            data=weight.data.2,
            aes(y=weightloss.2,x=exertime.2)) +
  geom_line(method="lm",
            stat="smooth",
            colour="red",
            data=weight.data.6,
            aes(y=weightloss.6,x=exertime.6)) +
  geom_line(method="lm",
            stat="smooth",
            colour="blue",
            data=weight.data.12,
            aes(y=weightloss.12,x=exertime.12))

scatter.2 + ggtitle("Influence Points") +
  theme(plot.title=element_text(face="bold")) +
  annotate("text",x=c(28,30,31),y=c(7.5,3.4,2.9),label="Influence")

#multiple regression
lm.mult <- lm(WeightLoss~BaselineWeight+ExerciseTime,data=weight.data)
lm.mult
#Coefficients:
#  (Intercept)  BaselineWeight    ExerciseTime  
#   -0.32208         0.01954         0.05645 
summary(lm.mult)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.6055 -0.4925 -0.0222  0.3457  2.9982 
#
#Coefficients:
#                Estimate    Std. Error  t value  Pr(>|t|)  
#(Intercept)     -0.3221     4.0534     -0.08     0.938  
#BaselineWeight   0.0195     0.0228      0.86     0.409  
#ExerciseTime     0.0565     0.0255      2.21     0.047 *
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.2 on 12 degrees of freedom
#Multiple R-squared:  0.33,	Adjusted R-squared:  0.218 
#F-statistic: 2.96 on 2 and 12 DF,  p-value: 0.0904

#F test for multiple regression model
qf(.95,df1=2,df2=12)
#3.9

#95% confidence interval of exercise time coefficient
confint(weight.exer.lm)

#residuals plot of model
mult.res <- plot(lm.mult,which=1,main="Multiple Regression Residuals")
hist(lm.mult$residuals,
     col = "lightblue",
     xlab="Residuals",
     breaks=10,
     main="Normality of Residuals")

