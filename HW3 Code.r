#Cynthia Vint
#BU MET CS555
#HW3

#Load data into R for analysis from Excel
library(xlsx)
birth.data <- read.xlsx("C:\\Users\\cvint\\CS555\\hw3\\BirthWeight.xlsx",1)

#create scatterplot
library(ggplot2)
scatter <- ggplot(data=birth.data,aes(x=Estriol,y=BirthWeight)) +
  geom_point(shape=1,colour="blue") +
  geom_line(method="lm",
            stat="smooth",
            colour="purple")

scatter + ggtitle("Estriol Levels\nvs.\nBirth Weight") +
  theme(plot.title=element_text(face="bold"))

#calculate the correlation coefficient
attach(birth.data)
detach(birth.data)
estriol.corr <- cor(Estriol,BirthWeight)
estriol.corr
#0.6535911

#Least squares regression equation
estriol.lm <- lm(BirthWeight ~ Estriol,data=birth.data)
estriol.lm
#Call:
#  lm(formula = BirthWeight ~ Estriol, data = birth.data)

#Coefficients:
#  (Intercept)  BirthWeight  
#   4.183        0.159 

#Create the ANOVA table
birth.anova <- anova(estriol.lm)
birth.anova
#Analysis of Variance Table
#
#Response: BirthWeight
#           Df  Sum Sq  Mean Sq   F value    Pr(>F)    
#Estriol    1   16.306  16.3056   20.135     0.0001208 ***
#Residuals  27  21.865  0.8098                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#table that gives the standard error of the slope
table.2 <- summary(estriol.lm)
table.2

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.31784 -0.69884 -0.09581  0.64517  1.53317 
#
#Coefficients:
#             Estimate   Std. Error t value Pr(>|t|)    
#(Intercept)  4.18281    0.62395    6.704   3.39e-07 ***
#  Estriol    0.15900    0.03543    4.487   0.000121 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.8999 on 27 degrees of freedom
#Multiple R-squared:  0.4272,	Adjusted R-squared:  0.406 
#F-statistic: 20.14 on 1 and 27 DF,  p-value: 0.0001208

#Perform formal test with alpha = .1
qf(.9,df1=1,df2=27)
qt(.95,df=27)

#Get 90% confidence interval on regression coefficient
confint(estriol.lm,level=.9)
#             5 %        95 %
#(Intercept)  3.12003753 5.2455741
#Estriol      0.09864681 0.2193558