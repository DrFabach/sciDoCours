#DM2 DE TECHNIQUE EN SCIENCE DES DONNÉES GROUPE B ( CHERIFATI INÉS, SALAA SOFIA, MAHDI RYMA)

#utilisation de cette base de donnée 
BDD <- salaries_copie
str(BDD)

# Décrire les différentes variables

# calculer les moyennes 
moy.phd <- mean(BDD$yrs.since.phd,na.rm = TRUE)
moy.service <- mean(BDD$yrs.service, na.rm = TRUE)
moy.salary <- mean(BDD$salary, na.rm=TRUE)



#calculer la mediane 
median(BDD$yrs.since.phd,na.rm = TRUE)
median(BDD$yrs.service,na.rm = TRUE)
median(BDD$salary,na.rm = TRUE)

#calculer le mode

effectif.rank <- table(BDD$rank)
sort(effectif.rank)
## donc le mode pour le rank est "prof"
effectif.discipline <- table(BDD$discipline)
sort(effectif.discipline)
## donc le mode pour displine est "B"
effectif.sex <- table(BDD$sex)
sort(effectif.sex)
## donc le mode pour sex est "male"

#calculer quantile
quantile(BDD$yrs.since.phd)
quantile(BDD$yrs.service)
quantile(BDD$salary)
 #calculer l'extreme et l'etendue 
#yrs.since.phd
min.years.since.phd <- min(BDD$yrs.since.phd,na.rm = TRUE)
max.years.since.phd <- max(BDD$yrs.since.phd,na.rm = TRUE)
etendue.years.since.phd <- max.years.since.phd - min.years.since.phd
#yrs.service
min.years.service <- min(BDD$yrs.service, na.rm = TRUE)
max.years.service <- max(BDD$yrs.service, na.rm = TRUE)
etendue.years.service <- max.years.service - min.years.service
#salary
min.salary <- min(BDD$salary, na.rm = TRUE)
max.salary <- max(BDD$salary, na.rm = TRUE)
etendue.salary <- max.salary - min.salary


## Il aurait été possible d'utiliser uniquement la fonction summary pour avoir toutes ces valeurs
summary(BDD)

# calculer la variance 
var(BDD$yrs.since.phd)
var(BDD$yrs.service)
var(BDD$salary)

#ecart-type
sd.phd <- sd(BDD$yrs.since.phd, na.rm = TRUE)
sd.service <- sd(BDD$yrs.service, na.rm = TRUE)
sd.salary <- sd(BDD$salary, na.rm = TRUE)


# calculer le coefficient de variation 
#creation d'une fonction 
cv <- (function(x) {sd(x,na.rm = TRUE)/mean(x, na.rm = TRUE) * 100})
#years.since.phd
cv(BDD$yrs.since.phd)
#years.service
cv(BDD$yrs.service)
#salary 
cv(BDD$salary)

# calaculer la frequence relative 
#rank 
prop.table(effectif.rank)
#discipline 
prop.table(effectif.discipline)
#sex
prop.table(effectif.sex)

# Croiser les variables avec la variable d'intérêt
#Deux variable quantitative 
cor(BDD$salary,BDD$yrs.service)
## il semblerait qu'il n'y ait pas de correlation entre le salaire et nombre d'année de service

cor(BDD$salary, BDD$yrs.since.phd)
## il semble qu'il n'y ait pas de corrélation entre le salaire et nombre d'années phd

#variable quantitative et qualitative 
#Correlation entre le salaire et le sexe 
boxplot(BDD$salary~BDD$sex,
        col = "purple"  , border = "black",
        main= "Sexe",
        ylab = "salary")
## On voit que le salaire des hommes est plus élevé que celui des femmes 

#corrélation entre le rang et le nombre d'année de service 
boxplot(BDD$yrs.service~BDD$rank,
        col = "purple"  , border = "black",
        main= "rank",
        ylab = "yrs.service")
##il y a un lien entre le nombre d'année de service et rang des proffesseurs

#corrélation entre deux variable qualitative 
table(BDD$rank,BDD$sex,deparse.level = 2,useNA = "always")




