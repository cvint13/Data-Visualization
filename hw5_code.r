#Cynthia Vint
#HW 5

#Load the data
library(xlsx)
housing.data <- read.xlsx("C:\\Users\\cvint\\CS555\\hw5\\housing.xls",1)

#get rid of the null row
housing.data <- housing.data[-nrow(housing.data),]
housing.data

#formatted better summary
library(plyr)

#price
ddply(housing.data,"bed",summarise,
      N =length(price),
      mean=mean(price),
      sd = sd(price))
#    bed  N      mean       sd
#1   3    13     493307.7   199598.6
#2   4    13     676348.2   224506.3
#3   5    8      1020687.5  247130.8

#sqft
ddply(housing.data,"bed",summarise,
      N =length(sqft),
      mean=mean(sqft),
      sd = sd(sqft))
#     bed  N     mean        sd
#1    3    13    2106.615    579.1182
#2    4    13    2705.154    1010.8182
#3    5    8     4361.250    1430.0794

#aov analysis on price
#bedrooms v price
boxplot(price ~ factor(bed),data=housing.data,
        main="Price vs. Number of Bedrooms",
        xlab="Number of Bedrooms",
        ylab="Price",
        col=rainbow(3))

#bedrooms v sqft
boxplot(sqft ~ factor(bed),data=housing.data,
        main="Sq. Footage vs. Number of Bedrooms",
        xlab="Number of Bedrooms",
        ylab="Square Footage",
        col=rainbow(3))

#Summary of price
price.m <- aov(price ~ factor(bed),data = housing.data)
summary(price.m)
#             Df   Sum Sq    Mean Sq     F value   Pr(>F)    
#factor(bed)  2    1.38e+12  6.900e+11   14.16     4.28e-05 ***
#  Residuals  31   1.51e+12  4.872e+10                     
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#f statistic to test
qf(.95,df1=2,df2=31)
#3.304817

#Tukey test
TukeyHSD(price.m)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = price ~ factor(bed), data = housing.data)
#
#$`factor(bed)`
#    diff     lwr       upr      p adj
#4-3 183040.5 -30046.66 396127.7 0.1033694
#5-3 527379.8 283257.75 771501.9 0.0000253
#5-4 344339.3 100217.21 588461.3 0.0042900

#Create dummy variables
housing.data$dummy1 <- ifelse(housing.data$bed == 4,1,0)
housing.data$dummy2 <- ifelse(housing.data$bed == 5,1,0)
#NOTE: It would have been easier to label them dummy4 and dummy5, good to remember in future

#redo the one-way ANOVA using the lm method with the dummy variables
dummy.m.1 <- lm(price ~ dummy1 + dummy2, data=housing.data)
summary(dummy.m.1)
#Call:
#  lm(formula = price ~ dummy1 + dummy2, data = housing.data)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-344388 -150058  -15418   80317  518652 
#
#Coefficients:
#                Estimate    Std. Error   t value   Pr(>|t|)    
#(Intercept)     493308      61221        8.058     4.24e-09 ***
#  dummy1        183041      86579        2.114     0.0426 *  
#  dummy2        527380      99189        5.317     8.65e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 220700 on 31 degrees of freedom
#Multiple R-squared:  0.4774,	Adjusted R-squared:  0.4437 
#F-statistic: 14.16 on 2 and 31 DF,  p-value: 4.278e-05

#one-way ANCOVA
install.packages("car")
install.packages("quantreg")
library(car)
options(contrasts=c("contr.treatment","contr.poly"))
lm(housing.data$price~factor(housing.data$bed)+housing.data$sqft)
Anova(lm(housing.data$price~factor(housing.data$bed)+housing.data$sqft),type=3)
#Anova Table (Type III tests)
#
#Response: housing.data$price
#                               Sum Sq Df F value    Pr(>F)    
#  (Intercept)              1.0557e+11  1  4.4471   0.04342 *  
#  factor(housing.data$bed) 8.0903e+10  2  1.7039   0.19911    
#  housing.data$sqft        7.9823e+11  1 33.6238 2.451e-06 ***
#  Residuals                7.1220e+11 30                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#least square means
install.packages("lsmeans")
library(lsmeans)
housing.data$bed.factor <- factor(housing.data$bed)
ls.data<-lsmeans(lm(housing.data$price~housing.data$bed.factor+housing.data$sqft),
        pairwise~housing.data$bed.factor,
        adjust="tukey")
#housing.data$bed.factor   lsmean       SE df lower.CL upper.CL
#3                       418268.9 44649.93 30 327081.5 509456.2
#4                       388713.0 46384.45 30 293983.3 483442.7
#5                       359480.1 48567.51 30 260292.0 458668.2
#
#Confidence level used: 0.95 
#
#$contrasts
#contrast estimate        SE df t.ratio p.value
#3 - 4    29555.86  5097.063 30   5.799  <.0001
#3 - 5    58788.71 10138.421 30   5.799  <.0001
#4 - 5    29232.85  5041.357 30   5.799  <.0001
#
#P value adjustment: tukey method for comparing a family of 3 estimates 
plot(ls.data$lsmeans,
     xlab="Range",
     ylab="Number of Bedrooms",
     main="LS Means")
