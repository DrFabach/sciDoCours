# A faire

# Entrainer un algorithme ( glm et knn) pour predire hosp_exp_flg .




data_tot<- readRDS("data.rds")
# informations sur la base : 
# https://physionet.org/content/mimic2-iaccd/1.0/

### Chargement des données
library(ggplot2)
library(dplyr)
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

folds <- cut(sample(seq(1,nrow( data_train ))),breaks=10,labels=FALSE)



fit<- glm(hosp_exp_flg ~ . , data = data_train[!folds==1,], family = "binomial")
summary(fit)


predictions<- predict(fit,data_train[!folds==1,],type = "response")

##Seuil à 0.5
table(predictions>0.5, data_train$hosp_exp_flg[!folds==1] )

indice_performance<- function(prediction, y_vrai ){
  VP <- sum(prediction & y_vrai )
  VN <- sum( (!prediction) & (!y_vrai) )
  FP <- sum(prediction & !y_vrai)
  FN <- sum( !prediction & y_vrai)
  sensibilite_rappel = VP/(VP+ FN)
  specificite = VN/(VN+FP)
  preci_VPP = VP / (VP + FP)
  VPN<- VN/(VN + FN)
  Accuracy <- (VP + VN )/ (VP+VN+FP+FN)
  F1 <- 2*(sensibilite_rappel*preci_VPP)/(sensibilite_rappel+preci_VPP)

  return(list(performance=list(
    sensibilite_rappel=sensibilite_rappel,
    specificite=specificite,
    preci_VPP=preci_VPP,
    VPN=VPN,
    Accuracy=Accuracy,
    F1=F1
  ),
  table_contingence= table(prediction, y_vrai)))

}
indice_performance(predictions >0.5,data_train$hosp_exp_flg[!folds==1]==1)


## Cross validation du meilleur seuil glm  : 
seuils<- c(1:100)/100
res_data<- data.frame()
for(i in 1:10){
  for( seuil_i in seuils){
    fit_i<- glm(hosp_exp_flg ~ age +bmi +saps_first +age * bmi  , data = data_train [!folds==i,], family ="binomial")
    predictions_i<- predict(fit_i,data_train [folds==i,],type = "response")
    
    res_data<-rbind(res_data,
                    data.frame(fold= i ,seuil = seuil_i,
                               indice_performance(predictions_i >seuil_i,data_train$hosp_exp_flg[folds==i]==1)$performance
                    )
    )
  }
}

dim(res_data)
res_mean<-res_data%>%group_by(seuil)%>%summarise_at(c("sensibilite_rappel",
                                            "specificite",
                                            "preci_VPP","Accuracy","F1"), function(x) median(x, na.rm = T))


res_mean%>%arrange(desc(Accuracy))
table( data_train$hosp_exp_flg)
res_mean%>%ggplot(aes(x=seuil, y= F1))+geom_line()
## Affichage des meilleurs paramètres.


## Refaire la meme chose avec un knn et cv le nombre de voisin .
res_data<- data.frame()
voisins<-c(1:50)
for(i in 1:10){
  for( n_k in voisins){

    pr<- knn(train = (data_train%>%select(-hosp_exp_flg))[!folds==i,], 
             test = (data_train%>%select(-hosp_exp_flg)) [folds==i,],
             cl = data_train[!folds==i, "hosp_exp_flg"],
             k = n_k)

    
    res_data<-rbind(res_data,
                    data.frame(fold= i ,n_k = n_k,
                               indice_performance(pr==1,data_train$hosp_exp_flg[folds==i]==1)$performance
                    )
    )
  }
}

res_data
res_mean<-res_data%>%group_by(n_k)%>%summarise_at(c("sensibilite_rappel",
                                                      "specificite",
                                                      "preci_VPP","Accuracy","F1"), function(x) median(x, na.rm = T))


res_mean%>%arrange(desc(F1))

data_train2<- data_train%>%mutate_if(is.numeric,scale)

summary(data_train$age[!folds==1])

res_data<- data.frame()
voisins<-c(1:50)
for(i in 1:10){
  for( n_k in voisins){
    
    pr<- knn(train = (data_train2%>%select(-hosp_exp_flg))[!folds==i,], 
             test = (data_train2%>%select(-hosp_exp_flg)) [folds==i,],
             cl = data_train2[!folds==i, "hosp_exp_flg"],
             k = n_k)
    
    
    res_data<-rbind(res_data,
                    data.frame(fold= i ,n_k = n_k,
                               indice_performance(pr==1,data_train$hosp_exp_flg[folds==i]==1)$performance
                    )
    )
  }
}

res_mean<-res_data%>%group_by(n_k)%>%summarise_at(c("sensibilite_rappel",
                                                    "specificite",
                                                    "preci_VPP","Accuracy","F1"), function(x) median(x, na.rm = T))


res_mean%>%arrange(desc(F1))
res_mean%>%ggplot(aes(x=n_k, y= F1))+geom_line()
