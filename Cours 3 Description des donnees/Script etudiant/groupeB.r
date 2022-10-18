#DEVOIR II: Description des données:

data("infert")
?infert
install.packages("tidyverse")
install.packages("pander")
library(tidyverse)
library(pander)
str(infert)

#On a choisi "education" et "induced" comme variables qualitatives; "age" et "stratum" comme quantitatives. 
#On a transformé les variables "induced" & "case" en variables du type "factor" lorsqu'elles sont quali, pourtant, pas "numeric".

as.factor(infert$induced)
infert$induced <- as.factor(infert$induced)
as.factor(infert$case)
infert$case <- as.factor(infert$case)
str(infert)


#PARAMÈTRES DE POSITION: "Age"
mean(infert$age) #moyenne
median(infert$age) #mediane
min(infert$age, na.rm = T) #min
max(infert$age, na.rm = T) #max
range(infert$age, na.rm = T) #range
max(infert$age)-min(infert$age) #étendue
rangeAge<-range(infert$age)
quantile(infert$age) #quantile
(quantile(infert$age, 0.25)) #1er
(quantile(infert$age, 0.50)) #2ème
(quantile(infert$age, 0.75)) #3ème
quantile(infert$age, probs = seq(.1, .9, by = .1)) #on a trouvé ça en ligne mais nous ne voyons pas forcement l'application.
IQR(infert$age) #intervale interquartile
(var(infert$age, na.rm = T)) #variance
(sd(infert$age, na.rm = T)) #écart-type
#Mode: créer fonction (code trouvé en ligne):
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]  
}
getmode(infert$age)

#Pour coefficient de variation:
getcofvar <- function(y) {
  sd(y, na.rm = TRUE)/mean(y, na.rm = TRUE)*100
}

getcofvar(infert$age)


#PARAMÈTRES DE POSITION: "Stratum/Stratus"
mean(infert$stratum)
median(infert$stratum)
min(infert$stratum, na.rm = T)
max(infert$stratum, na.rm = T)
range(infert$stratum, na.rm = T)
max(infert$stratum)-min(infert$stratum)
rangeStra<-range(infert$stratum)
quantile(infert$stratum)
(quantile(infert$stratum, 0.25))
(quantile(infert$stratum, 0.50))
(quantile(infert$stratum, 0.75))
IQR(infert$stratum)
(var(infert$stratum, na.rm = T)) #574.4853 ??? on a pas compris ce résultat.
(sd(infert$stratum, na.rm = T))
getmode(infert$stratum) # ??? il n'y a pas de répetition de valeurs mais on sait pas pourquoi ça a montré "1".
getcofvar(infert$stratum)

#VARIABLES QUALITATIVES: "education" et "induced":
table(infert$education) #pourquoi ça a pris juste une valeur?  ;'(
{results='asis'} #???
pander(table(infert$education))
prop.table(table(infert$education))

table(infert$induced)
pander(table(infert$induced))
prop.table(table(infert$induced))

#CROISEMENT DE VARIABLES:
#par fonction "by" (quanti-quali)
by(infert$education, infert$age, mean)
by(infert$education, infert$age, mean, na.rm = T) #ça n'a pas fonctionné, je pense. Il y a une liste avec chaque age et un indice "NA" en dessous.

table(infert$age, infert$education)
table(infert$age, infert$induced)

#correlation (quanti-quanti):
cor(infert$age, infert$education) #message d'erreur: y must be numeric
cor(infert$age, infert$stratum) #r: -0.21, donc il n'y a pas de correlation entre ces deux variables?

#par conditions (quanti-quali):
mean(infert$age[which(infert$education==1)]) #réponse: NaN
mean(infert$age[which(infert$education==0)]) #réponse: NaN
mean(infert$age[which(!infert$education==1)]) #réponse: 31.50