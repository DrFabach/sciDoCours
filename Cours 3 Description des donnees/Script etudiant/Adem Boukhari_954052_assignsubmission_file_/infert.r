
infert<-infert
head(infert)
str(infert)
infert$case<-as.factor(infert$case)

#descriptive statistics
mean(infert$age, na.rm = T)
median(infert$age, na.rm = T)
quantile(infert$age, na.rm = T)
min(infert$age, na.rm = T)
max(infert$age, na.rm = T)
max(infert$age, na.rm = T)-min(infert$age, na.rm = T)
var(infert$age, na.rm = T)
sd(infert$age, na.rm = T)
summary(infert$age, na.rm = T)

#descriptive statistics for categorical variables

table(infert$spontaneous)
prop.table(table(infert$spontaneous))*100


table(infert$education)
prop.table(table(infert$education))*100


        
#descriptive statistics for quantitative variables with psych package

install.packages("psych")
library(psych)

describe(infert$parity, na.rm = TRUE, interp=FALSE, ranges = TRUE,quant=c(.25,.75),IQR=TRUE)
describe(infert$age, na.rm = TRUE, interp=FALSE, ranges = TRUE,quant=c(.25,.75),IQR=TRUE)

#

summary(infert$age[which(infert$case==1)])
describe(infert$age[which(infert$case==1)], na.rm = TRUE, interp=FALSE, ranges = TRUE,quant=c(.25,.75),IQR=TRUE)

prop.table(table(infert$case, infert$spontaneous))*100
cor(infert$age, infert$spontaneous)


