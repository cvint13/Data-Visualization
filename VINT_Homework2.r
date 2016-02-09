#Cynthia Vint
#BU MET CS555
#HW2

library(ggplot2)
library(xlsx)
#read in xlsx file
iq.scores <- read.xlsx("C:\\Users\\cvint\\CS555\\IQScores.xlsx",sheetIndex = 1)
iq.scores

#Summarize the data
girl.data <- iq.scores[which(iq.scores$Gender=="F"),]
girl.data
summary(girl.data$IQ)
#   Min. 1st Qu.  Median    Mean   3rd Qu.  Max.    NA's 
#   82.0 108.0    113.0     113.9  120.0    140.0   9

boy.data <- iq.scores[which(iq.scores$Gender=="M"),]
boy.data
summary(boy.data$IQ)
#   Min.  1st Qu.  Median  Mean   3rd Qu.  Max. 
#   83.0  100.5    108.5   107.8  113.8    136.0 

#Overall
summary(iq.scores$IQ)

#Make comparative table
iq.summaries <- rbind(summary(boy.data$IQ),summary(girl.data$IQ))
row.names(iq.summaries) <- c("Boys","Girls")
iq.summaries

#Make an overlaid histogram
iq.hist <- ggplot(iq.scores,aes(x=IQ,fill=Gender)) + 
  geom_histogram(binwidth=5,position="dodge") 
iq.hist

#Show the two best fit distributions
iq.scores$Gender <- factor(iq.scores$Gender)

iq.dens <- ggplot(iq.scores,aes(x=IQ,fill=Gender)) +
  geom_density(alpha=.3)
iq.dens

#Part b: does girls' IQ differ from 105?
#standard deviation
girl.sd <- sd(girl.data$IQ)/sqrt(length(girl.data$IQ))
girl.sd
girl.z <- (mean(girl.data$IQ)-105)/girl.sd
girl.z

#test statistic
test.p <- 2*pnorm(girl.z,lower.tail = FALSE)
test.p

#90% confidence interval
z.90 <- qnorm(.9)
z.90
conf.90 <- c(mean(girl.data$IQ)-(z.90*girl.sd),mean(girl.data$IQ)+(z.90*girl.sd))
cat("90% Confidence Interval: ",conf.90[1]," - ",conf.90[2])

#Test if girls' IQs are greater than boys' IQs.
girl.test <- (sd(girl.data$IQ)^2)/length(girl.data$IQ)
boy.test <- (sd(boy.data$IQ)^2)/length(boy.data$IQ)
test.t <- (mean(girl.data$IQ)-mean(boy.data$IQ))/sqrt(girl.test+boy.test)
test.t
p.test <- pnorm(test.t,lower.tail = FALSE)
p.test
