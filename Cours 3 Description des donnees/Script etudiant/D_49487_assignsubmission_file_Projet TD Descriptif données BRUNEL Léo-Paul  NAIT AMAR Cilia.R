data("infert")

donnee <- infert
# On commence par décrire les variables quantitatives 
## On a choisis les variables ages et parity 
#moyenne

mean(donnee$age, na.rm = TRUE)
mean(donnee$parity, na.rm = TRUE)

#mediane

median(donnee$age, na.rm = T)
median(donnee$parity, na.rm = T)

#min

min(donnee$age, na.rm = T)
min(donnee$parity, na.rm = T)

#max 

max(donnee$age, na.rm = T)
max(donnee$parity, na.rm = T)

#range 

range(donnee$age)
range(donnee$parity)

# On créer une fonction pour calculer l'etendu
etendu<- function(variable){
  minX<- min(variable, na.rm = T)
  maxX<- max(variable, na.rm = T)
  return(maxX-minX)
}


etendu(donnee$age)
etendu(donnee$parity)

#quartile

(quantile(donnee$age, 0.25))
(quantile(donnee$parity, 0.25))

(quantile(donnee$age, 0.75))
(quantile(donnee$parity, 0.75))

(quantile(donnee$age, c(0.25, 0.75)))
(quantile(donnee$parity, c(0.25, 0.75)))

#Interval interquartile 

IQR(donnee$age)
IQR(donnee$parity)

#Variance
(var(donnee$age, na.rm = T))
(var(donnee$parity, na.rm = T))

#écart-type
(sd(donnee$age, na.rm = T))
(sd(donnee$parity, na.rm = T))

#On peut passer aux variables qualitatives  
## On a choisis les variables education et induced 

donnee$induced <- as.factor(donnee$induced)
str(donnee)
#table effectifs

table(donnee$education)
table(donnee$induced)

#table frequence 

prop.table(table(donnee$education))
prop.table(table(donnee$induced))

# Croisement variable quali-quali 
prop.table(table(donnee$education, donnee$induced))*100 
prop.table(table(donnee$education, donnee$case))*100 #On donne directement les pourcentages 
prop.table(table(donnee$induced, donnee$case))
prop.table(table(donnee$education, donnee$case),1)*100
prop.table(table(donnee$induced, donnee$case),1)

table(donnee$education, donnee$case)
table(donnee$induced, donnee$case)


#Croisement variable quanti-quanti

cor(donnee$age, donnee$parity )

install.packages("ggplot2") ## On utilise ce package pour créer un nuage de point 
library(ggplot2)
ggplot(donnee, aes(x = age, y = parity )) + geom_point(shape = 19, color = "black" , size = 3) + labs(x = "age", y = "parity") + ggtitle("Nuage de point représentant la corrélation entre les variables age et parity")
## Le résultat est proche de 0 on peut donc dire qu'il n'y a pas de correlation en les deux variables


#Croisement variable quanti-quali
## Par la fonction by 
by(donnee$age, donnee$case, mean)
by(donnee$age, donnee$case, var)
by(donnee$parity, donnee$case, mean,)
by(donnee$parity, donnee$case, var,)

## Par diplyr 
#install.packages("dplyr")
library(dplyr)


resultats <- donnee %>%
  group_by(case) %>%
  summarise(moyenne_quantitative = mean(age))

print(resultats)


resultats1 <- donnee %>%
  group_by(case) %>%
  summarise(variance_quantitative = var(parity))

print(resultats1)
