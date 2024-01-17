

# Description des données de santé

## Import de la base de données

Ceci est le script du cours






# CTRL+ALT+I = Nouveau chunk






BDD<- mtcars

BDD

#Base de données présente de base dans R : [lien ici](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/infert.html) 

head(BDD)



### Ecart R
#- R utilise des packages qui comprennent des fonctions 


# chargement d'une library
# base est chargée de base
library(base)




# installation d'une librairie 
# install.packages("tidyverse")




# chargement de la librairie
library(tidyverse)


- R dispose d'une aide intégrée


#aide sur une librairie
?tidyverse
help(tidyverse)




# aide sur une fonction
# ?read.csv2
# 
# 
# - Les fonctions utilises des arguments
# 
# 
read.csv2(" ")


# 
# 
# 
# - R est un langage de programmation avec différents types d'objets


read.csv("Cours 3 Description des donnees/infert.csv")


# Ceci est un objet 

a<-1
1->a
# Deconseillé
a=1



#Il peut être appelé par son nom 
a



#Il peut être modifier par son nom
a<-2 
a



# Il peut comprendre autre chose qu'une valeur unique 
a<- c("a","b","c")
a
# La fonction c() crée un vecteur




# Les valeurs d'un vecteur peuvent être accédées par leur indice 
a[1]
# Accès à partir d'un vecteur
a[c(1,3)]



BDD

### Le nom des objets 

# - Eviter les caractères spéciaux, accents
# - toujours commencer par une lettre 
# - espace interdit
# - certains noms qui sont déja utilisés
# 
# Deux façon traditionnelles : 
# - Serpent : nom_de_la_variable
# - chameau : nomDeLaVariable
# nom.de.la.variable


typeof(a)
str(a)




# BDD est un data frame
BDD[1,1]
BDD[1,]
head(BDD[,1])

# Interogger variable
head(BDD$var)



# ========================================================



# Analyse de la structure de BDD
(str(BDD))


(names(BDD))



## Modification des types de variables 


# Changement de numérique en facteur
BDD$var<-as.factor(BDD$var)
head(BDD$var)




# Changement de numérique en facteur avec choix des modalités
BDD$var<- factor(BDD$var,  labels =  c("0","1","2 or more"))
head(BDD$var)




BDD$var<- as.factor(BDD$var)



# Conversion de facteur en numérique
facteur<- as.factor(c(10,4,10,2,3,4,6,5,7,5,4,10,2))
as.numeric(as.character(facteur))




# Retour cours

# Variables quantitatives :
- moyenne

mean(BDD$age, na.rm = TRUE)


- mediane

median(BDD$age, na.rm = T)


- min

min(BDD$age, na.rm = T)


- max 

max(BDD$age, na.rm = T)


-range 

range(BDD$age)




###############
### Ecart R ### 
############### 


max(BDD$age)-min(BDD$age)
rangeAge<-range(BDD$age)
rangeAge[2]-rangeAge[1]



Fonction dans R

nomfonction <- function(arg1, arg2 = valeurParDefaut){

somme<- arg1+ arg2
# plusieurs opérations

return(somme)
}



# Création de fonction :
etendu<- function(variable){
  minX<- min(variable, na.rm = T)
  maxX<- max(variable, na.rm = T)
  return(maxX-minX)
}
str(etendu)
etendu
etendu(BDD$age)


###########

- quartile

# premier quantile
(quantile(BDD$age, 0.25))
#Troisième quantile
(quantile(BDD$age, 0.75))
#1er et 3eme quantiles
(quantile(BDD$age, c(0.25, 0.75)))


- Interval interquartile 

IQR(BDD$age)




- Variance, écart type


#Variance
(var(BDD$age, na.rm = T))

#écart-type
(sd(BDD$age, na.rm = T))




# Retour cours

# Variable Quali : 

- table effectifs

table(BDD$education)


Astuce 

{r results='asis'}
library(pander)
pander(table(BDD$var))



- table fréquence


prop.table(table(BDD$var))





# Croisement de variable

## Quali Quali

- table effectif

table(BDD$var, BDD$var2)


-table proportions


# Proportions en ligne

prop.table(table(BDD$var, BDD$var2),1)

# Proportions en colonne
prop.table(table(BDD$var, BDD$var2),2)



# Proportions globales
prop.table(table(BDD$var, BDD$var2))




## Quanti-quali

- Par des conditions 


# moyenne age groupe 1
mean(BDD$age[which(BDD$var==1)])


# moyenne age groupe 2
mean(BDD$age[which(BDD$var==0)])


# moyenne age groupe pas 1 
mean(BDD$age[which(!BDD$var==1)])



- Par la fonction by


by(BDD$age, BDD$var, mean)


## Quanti Quanti

- correlation

cor(BDD$age, BDD$var)



# Données manquantes

Les donnéees manquantes sont noté NA dans R

vecteur<- c(1,2,3,NA,5,6)
mean(vecteur)
mean(vecteur, na.rm = T)


- Calculer moyenne, médiane de vecteur






