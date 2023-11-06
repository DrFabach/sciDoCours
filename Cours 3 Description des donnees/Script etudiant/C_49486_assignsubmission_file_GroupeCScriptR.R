#importer la library tidyverse,dyplr
library(tidyverse)
library(dplyr)
#install.packages("skimr")
library(skimr)
#install.packages("summarytools")
library(summarytools)


##################################Étude des facteurs influençant la qualité du sommeil##################################################

#Colonnns : -----------------------------------------------------------------------------------------------------------------------

#Person ID: An identifier for each individual.
#Gender: The gender of the person (Male/Female).
#Age: The age of the person in years.
#Occupation: The occupation or profession of the person.
#Sleep Duration (hours): The number of hours the person sleeps per day.
#Quality of Sleep (scale: 1-10): A subjective rating of the quality of sleep, ranging from 1 to 10.
#Physical Activity Level (minutes/day): The number of minutes the person engages in physical activity daily.
#Stress Level (scale: 1-10): A subjective rating of the stress level experienced by the person, ranging from 1 to 10.
#BMI Category: The BMI category of the person (e.g., Underweight, Normal, Overweight).
#Daily Steps: The number of steps the person takes per day.
#Sleep Disorder: The presence or absence of a sleep disorder in the person (None, Insomnia, Sleep Apnea).

#-----------------------------------------------------------------------------------------------------------------------------------------------



#importer notre jeu de données 

data<- read.csv("Emplacement", header = TRUE)
data
str(data)
summary(data)

#Calcule des paramètre de position et de dispertion---------------------------------------------------------------


skim(data)
dfSummary(data)


#Variables qualitatives fréquence relative-------------------------------------------------------------------------------------

#Calcule de la fréquence relative pour toute les variable qualitative en utilisant une fonction et la vectoriser sur toute les variables qualitatives

colonne_quali<- sapply(data,is.character)

freq_rel_variable_quali<- lapply(data[,colonne_quali],function(x) prop.table(table(x)))

print(freq_rel_variable_quali)



#Croisement des variables---------------------------------------------------------------------------------------------------------

#Utilisation des paramètres de position et dispersion pour décrire l'ensemble des variables en fonction de la variable groupe d'intérêt.


mean_groupby_sleep_quality <- aggregate(data[, c("sleep.duration","age","physical.activity.level","daily.steps","stress.level")], by = list(data$quality.of.sleep), FUN = mean)
print(mean_groupby_sleep_quality)

sd_groupby_sleep_quality <- aggregate(data[, c("age","sleep.duration","physical.activity.level","daily.steps","stress.level")], by = list(data$quality.of.sleep), FUN = sd)
print(sd_groupby_sleep_quality)

IQR_groupby_sleep_quality <- aggregate(data[, c("age","sleep.duration","physical.activity.level","daily.steps","stress.level")], by = list(data$quality.of.sleep), FUN = IQR)
print(IQR_groupby_sleep_quality)

#Croisement des variables Occupation,BMI category, Sleep disorder, Gender, avec la variable Quality of sleep
#Par Moyenne

gender_quality_mean <-data%>%group_by(gender)%>%summarise(moyenne_qualite_sommeil=mean(quality.of.sleep))
print(gender_quality_mean)

occupation_quality_mean<-data%>%group_by(occupation)%>%summarise(moyenne_qualite_sommeil=mean(quality.of.sleep))
print(occupation_quality_mean)

bmi_quality_mean<-data%>%group_by(bmi.category)%>%summarise(moyenne_qualite_sommeil=mean(quality.of.sleep))
print(bmi_quality_mean)

sleep_disorder_quality_mean<-data%>%group_by(sleep.disorder)%>%summarise(moyenne_qualite_sommeil=mean(quality.of.sleep))
print(sleep_disorder_quality_mean)

#Par Ecart type

gender_quality_sd<-data%>%group_by(gender)%>%summarise(ecart_type_qualite_sommeil=sd(quality.of.sleep))
print(gender_quality_sd)

occupation_quality_sd<-data%>%group_by(occupation)%>%summarise(ecart_type_qualite_sommeil=sd(quality.of.sleep))
print(occupation_quality_sd)

bmi_quality_sd<-data%>%group_by(bmi.category)%>%summarise(ecart_type_qualite_sommeil=sd(quality.of.sleep))
print(bmi_quality_sd)

sleep_disorder_quality_sd<-data%>%group_by(sleep.disorder)%>%summarise(ecart_type_qualite_sommeil=sd(quality.of.sleep))
print(sleep_disorder_quality_sd)

#Corrélation----------------------------------------------------------------------------------------------------------------------------

#Corrélation entre la qualité du sommeil et (l'age,durée du someil,activité physique, nombre de pas par jour et niveau de stresse)

correl<- sapply(c("age","sleep.duration","physical.activity.level","stress.level"), function(x) cor(data[x],data$quality.of.sleep))
correl



#Croisemebnt niveau de stress et genre

stress_level_gender<- table(data$stress.level, data$gender)

prop.table(stress_level_gender)


#Croisement niveau de stress et age

stress_level_gender<-data%>%group_by(stress.level)%>%summarise(moyenne_age=mean(age))

print(stress_level_gender)


# Croisement de bmi.category et sleep.disorder

tableau_bmi_sleep_disorder<- table(data$bmi.category,data$sleep.disorder)

prop.table(tableau_bmi_sleep_disorder)

####################################################FIN###############################################################

