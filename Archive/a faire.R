# A faire

# Entrainer un algorithme ( glm et knn) pour predire hosp_exp_flg.



data_tot<- readRDS("data.rds")
# informations sur la base : 
# https://physionet.org/content/mimic2-iaccd/1.0/

### Chargement des données
library(ggplot2)
library(tidyverse)
library(class)

set.seed(45)




data_tot<-data_tot%>%select( "age", "gender_num", 
 "bmi", "sapsi_first", "sofa_first", "chf_flg", "afib_flg", "renal_flg", 
"liver_flg", "copd_flg", "cad_flg", "stroke_flg", "mal_flg", 
"resp_flg", "map_1st", "hr_1st", "temp_1st", "spo2_1st", "abg_count", 
"wbc_first", "hgb_first", "platelet_first", "sodium_first", "potassium_first", 
"tco2_first", "chloride_first", "bun_first", "creatinine_first", 
"po2_first", "pco2_first", "iv_day_1","hosp_exp_flg")

data_tot$hosp_exp_flg<- as.factor(data_tot$hosp_exp_flg)

sample_train<-sample(1:dim(data_tot)[1],600,replace = F)
data_train <- data_tot[sample_train,]
data_test <- data_tot[-sample_train,]


### Test de l'entrainement des modèles sur un fold

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



## Cross validation du meilleur seuil glm  : 
seuils<- ____
res_data<- data.frame()
for(i in ___){
 for( ___ in families){
  fit_i<- glm(___ ~ ___ , data = data_train [___,], family ="binomial")
  predictions_i<- predict(data_train [___,],type = "response")
  
 ___
tableau_contingence<- table( ____ > ___,  data_test$___)
  res_data<-rbind(res_data,
                  data.frame(___)
)
}
}


