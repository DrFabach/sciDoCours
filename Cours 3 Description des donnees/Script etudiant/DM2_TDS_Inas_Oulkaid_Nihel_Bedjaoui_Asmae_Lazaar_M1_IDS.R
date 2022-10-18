install.packages("tidyverse")
library("tidyverse")

#Récupératon bdd
data("infert")

#Description des données: qualitatives("education","induced", "case", "spontneous") , 
#quantitatives("age","parity", "matched set number","stratum number"
is.factor(infert$education)
is.numeric(infert$induced)
infert$induced<- factor(infert$induced,c("0","1","2"))
is.factor(infert$induced)
is.factor(infert$case)
infert$case <- factor(infert$case, c("0","1"))
is.factor(infert$case)
is.factor(infert$spontaneous)
infert$spontaneous <- factor(infert$spontaneous)
is.factor(infert$spontaneous)

install.packages("psych")
library("psych")
describe(infert)

#variables quantitatives : "age" et "parity"

#paramètre de position 
#moyenne
mean(infert$age,na.rm=T)
mean(infert$parity,na.rm=T)
#médianne
median(infert$age, na.rm=T)
median(infert$parity, na.rm=T)
#Quartiles 
quantile(infert$age, na.rm = T)
quantile(infert$parity, na.rm = T)

#paramètre de dispersion
#extrêmes 
min(infert$age, na.rm = T)
max(infert$age, na.rm = T)
min(infert$parity, na.rm = T)
max(infert$parity, na.rm = T)
#étendu
max(infert$age, na.rm = T) - min(infert$age, na.rm = T)
max(infert$parity, na.rm = T)-min(infert$parity, na.rm = T)
#variance
var(infert$age, na.rm = T)
var(infert$parity, na.rm = T)
#Ecart-type
sd(infert$age, na.rm = T)
sd(infert$parity, na.rm = T)

#variables qualitatives : "induced" et "parity"
#Fréquences relatives
table("infert")
prop.table(table("infert"))



#Croisement entre quanti/quali
#croiser age avec case, parity avec case,éducation/parity , age/spontaneous , age/induced 
by(infert$age, infert$case, mean)
by(infert$parity, infert$case, mean)
#!!!!!!by(infert$education, infert$parity, mean) -> faux (à mettre sur la présentation)
by(infert$parity, infert$education, mean)
by(infert$age, infert$spontaneous, mean)
by(infert$age, infert$induced, mean)


#Croisement variables quantitatives 
#croiser âge/parity 
cor(infert$age,infert$parity)

#Croisement variables qualitatives 
#induced abortions/case , spontaneous abortions/case 

#table effectif
table(infert$induced, infert$case)
# Proportions en ligne
prop.table(table(infert$induced, infert$case),1)
# Proportions en colonne
prop.table(table(infert$induced, infert$case),2)
# Proportions globales
prop.table(table(infert$induced, infert$case))

#table effectif
table(infert$spontaneous, infert$case)
# Proportions en ligne
prop.table(table(infert$spontaneous, infert$case),1)
# Proportions en colonne
prop.table(table(infert$spontaneous, infert$case),2)
# Proportions globales
prop.table(table(infert$spontaneous, infert$case))

