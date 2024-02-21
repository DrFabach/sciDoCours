# A faire

# Trouver les meilleures régressions  pour prédire hospital_los_day: length of stay in hospital (days, numeric)
et  day_28_flg: death within 28 days (binary: 1 = yes, 0 = no)



data_tot<- readRDS("data.rds")

# informations sur la base : 

### https://physionet.org/content/mimic2-iaccd/1.0/

## Chargement des données
library(ggplot2)
library(tidyverse)
library(class)





data_tot<-data_tot%>%select( ____)

data_tot$hosp_exp_flg<- as.factor(data_tot$hosp_exp_flg)
set.seed(45)

sample_train<-sample(1:dim(data_tot)[1],600,replace = F)
data_train <- data_tot[sample_train,]
data_test <- data_tot[-sample_train,]


# Test de l'entraînement des modèles sur un fold

folds <- cut(seq(1,nrow( ___ )),breaks=10,labels=FALSE)



fit<- glm(_____ ~ ___ , data = data_train[!folds==1,], family = "binomial")
summary(fit)


predictions<- predict(___,data_train[!folds==1,],type = "response")

##Seuil à 0.5
table(predictions>0.5, data_train$___[!folds==1] )

F1 = ___
acc= ___
rappel = ___
precision = ___
sensibilite = ___
specificite =___
vpn = ___



# Cross validation du meilleur seuil glm  : 

seuils<- ____
res_data<- data.frame()
for(i in ___){
  for( ___ in seuils){
    fit_i<- glm(___ ~ ___ , data = data_train [___,], family ="binomial")
    predictions_i<- predict(data_train [___,],type = "response")
    
    ___
    tableau_contingence<- table( ____ > ___,  data_test$___)
    ___parametres de performance____
    
    res_data<-rbind(res_data,
                    data.frame( ___parametres de performance_____)
    )
  }
}

## Choix des variables à mettre dans me modèle de prédiction 


# Même chose pour prédire la durée de séjour


# Bonus essayer de standardiser les variables quantitatives pour voir si ça améliore les résultats
https://www.statsoft.fr/concepts-statistiques/glossaire/c/centrer.html

