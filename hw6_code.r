#HW5

#Read data
#install.packages("xlsx")
library(xlsx)
pain_data <- read.xlsx("C:\\Users\\cvint\\CS555\\hw6\\pain.xlsx",1)

#proportion of patients who received tratment and experienced pain
p.treat <- mean(pain_data$Pain[which(pain_data$Treatment=="New Treatment")])
p.treat
#0.17

#proportion of patients who received placebo and experienced pain
p.placebo <- mean(pain_data$Pain[which(pain_data$Treatment=="Placebo")])
p.placebo
#0.35

#Reference group (p2) is the placebo group
#Calculate Risk Difference
r.d <- p.treat-p.placebo
r.d
#-0.18

#Formally test the 2-sample proportion
z.stat <- qnorm(.975)
z.stat
z <- -.08/sqrt(.26*.74/50)
z
prop.test(c(p.treat*100,p.placebo*100),c(100,100),correct=FALSE)
#2-sample test for equality of proportions without continuity correction
#
#data:  c(p.treat * 100, p.placebo * 100) out of c(100, 100)
#X-squared = 8.42, df = 1, p-value = 0.003711
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  -0.29899419 -0.06100581
#sample estimates:
#  prop 1 prop 2 
#  0.17   0.35 

#Logistic regression with treatment as the only explanatory variable
lrm <- glm(pain_data$Pain ~ pain_data$Treatment,family="binomial")
lrm

#formal test
summary(lrm)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.9282  -0.9282  -0.6105   1.4490   1.8825  
#
#Coefficients:
#                               Estimate   Std. Error z value  Pr(>|z|)    
#(Intercept)                    -1.5856     0.2662    -5.956   2.58e-09 ***
#  pain_data$TreatmentPlacebo   0.9666      0.3389     2.852   0.00434 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 229.22  on 199  degrees of freedom
#Residual deviance: 220.67  on 198  degrees of freedom
#AIC: 224.67

#odds ratio
o.r <- exp(cbind(OR = coef(lrm), confint.default(lrm)))
o.r

#c-statistic
#install.packages("pROC")
library(pROC)
c.stat <- roc(pain_data$Pain,as.numeric(pain_data$Treatment))
c.stat
plot(c.stat,main="ROC Curve")

#perform multiple logistic regression with treatment, age and severity vs. pain
mlrm <- glm(Pain ~ Treatment + Age + Severe,data=pain_data,family="binomial")
mlrm
#Call:  glm(formula = Pain ~ Treatment + Age + Severe, family = "binomial", 
#           data = pain_data)
#
#Coefficients:
#  (Intercept)  TreatmentPlacebo               Age            Severe  
#       5.5703            2.7612           -0.1933            1.0561  
#
#Degrees of Freedom: 199 Total (i.e. Null);  196 Residual
#Null Deviance:	    229.2 
#Residual Deviance: 136.5 	AIC: 144.5

summary(mlrm)
install.packages("aod")
library(aod)
wald.test(b=coef(mlrm),Sigma = vcov(mlrm),Terms=2:4)
#Wald test:
#  ----------
#  
#  Chi-squared test:
#  X2 = 39.7, df = 3, P(> X2) = 1.2e-08

#Odds Ratio
exp(mlrm$coefficients[2])
#15.81889

exp(mlrm$coefficients[3]*10)
#0.1447416

exp(mlrm$coefficients[4])
#2.875093

#c.statistic
pain_data$prob <- predict(mlrm,type=c("response"))
pain_data$prob
g <- roc(pain_data$Treatment,pain_data$prob)
g
plot(g)