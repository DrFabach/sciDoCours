title: "R Notebook"
output:
  html_document:
  df_print: paged
---
  
#ttps:/physionet.org/content/mimic2-iaccd/1.0/
  # Algorithmede machine learning
  
  ``
library(pander)
librery(ggplot2)
library(tidyverse)
library(caret)
library(class)
install.packages("tree")
library(tree)
install.packages("MASS")
library(MASS)
install.packages("randomForest")
library(randomForest)
library(dplyr)
require(e1071)
install.packages("mlr3")
library(mlr3)
# saveRDS(objet, "chemin/objet.rds")
data<-readRDS("data.rds")
str(data)

set.seed(45)
sample_train<-sample(1:dim(data)[1],600,replace = F)

data<-data%>%select( "age", "gender_num","bmi", "sapsi_first", "sofa_first", "chf_flg", "afib_flg", "renal_flg", 
                             "liver_flg", "copd_flg", "cad_flg", "stroke_flg", "mal_flg", 
                             "resp_flg", "map_1st", "hr_1st", "temp_1st", "spo2_1st", "abg_count", 
                             "wbc_first", "hgb_first", "platelet_first", "sodium_first", "potassium_first", 
                             "tco2_first", "chloride_first", "bun_first", "creatinine_first", 
                             "po2_first", "pco2_first", "iv_day_1","hosp_exp_flg")
data$hosp_exp_flg<- as.factor(data$hosp_exp_flg)
data_train <- data[sample_train,]
data_test <- data[-sample_train,]
normParam <- preProcess(data_train)


norm.train<- predict(normParam, data_train)
norm.testData <- predict(normParam, data_test)

require(e1071) 
#Entrainement d'un modèle
model <- naiveBayes(hosp_exp_flg~., data = norm.train)
class(model) 
pred <-predict(model,norm.train)
table(pred,norm.train$hosp_exp_flg)

pred <- predict(model,norm.testData)
table(pred, norm.testData$hosp_exp_flg)
## knn
#Entrainement d'un modèle sur de données non normalisées

mod_knn = knn(data_train%>%select(-hosp_exp_flg), data_test%>%select(-hosp_exp_flg), data_train$hosp_exp_flg, k = 2, prob=TRUE)
table(mod_knn,data_test$hosp_exp_flg)

mod_knn2 = knn( norm.train%>%select(-hosp_exp_flg), 
                                   norm.testData%>%select(-hosp_exp_flg), 
                                norm.train$hosp_exp_flg, k = 2, prob=TRUE)
table(mod_knn2,data_test$hosp_exp_flg)

mod_arbre = tree(hosp_exp_flg~., data =data_train)
plot(mod_arbre)
text(mod_arbre)
prune.tree (tree, k = NULL, best = NULL, data, nwts,
     method = c("deviance", "misclass"), loss, eps =1e-3
     data(hosp_exp_flg, package="MASS")
hosp_exp_flg.tr<- tree(hosp_exp_flg~., hosp_exp_flg)
plot(print(hosp_exp_flg.tr))
hosp_exp_flg <- md_tree(fgl.tr,, prune.tree)
for(i in 2:5)  hosp_exp_flg.cv$dev <- hosp_exp_flg.cv$dev +
  cv.tree(hosp_exp_flg.tr,,prune.tree)$dev
hosp_exp_flg.cv$dev <- hosp_exp_flg.cv$dev/5
plot(hosp_exp_flg.cv)
plot(mod_arbre)


library(randomForest)
# entrainement d'un random forest  
mod_rf = randomForest(hosp_exp_flg~., data = data_train,ntree=1000,classwt =c(4,1)
table(data_test$hosp_exp_flg, predict(mod_rf,data_test))

#library(smv)
# entrainement d'un svm
mod_svm = svm(hosp_exp_flg~., data = norm.train)
table(predict(mod_svm,norm.testData), norm.testData$hosp_exp_flg)

#cross validation
#définition de la stratégie de crossvalidation

fit_control<-trainControl(method = ("repeatedcv"),
                          number = 5,
                          repeats = 5)
str(traincontrol)
#définition de la grille d'hyperparamètre à tester
gridsearch <-  expand.grid( 
  scale  = c(1/100000,1/10000,1/1000,1/100,1/10), 
  C = c(1,2,3,4),
  degree = c(1,2,3,4,5))
#Entrainement d'un svm par crossvalidation
fit1<- train(hosp_exp_flg~., data = norm.train,
            method="svmPoly",
            tuneGrid= gridsearch,
            trControl = fit_control)


# Modification de la mesure à maximiser
f1 <- function (data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = "oui")
  recall  <- sensitivity(data$pred, data$obs, postive = "oui")
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  f1_val
} 
fit_control<-trainControl(method = "repeatedcv",
                          number = 2,
                          classProbs = TRUE,
                          summaryFunction = f1,
                          repeats = 4)
norm.train$hosp_exp_flg<-ifelse(norm.train$hosp_exp_flg=="1","oui","non")
fit1<- train (hosp_exp_flg~., data = norm.train, method="svmpoly",
            tuneGrid= gridsearch,
            trControl = fit_control,
          metric = "F1")



fit_control<-trainControl(fit_control<-trainControl(method = "repeatedcv",
                                                    number = 2,
                                                    classProbs = TRUE,
                                                    summaryFunction = f1,
                                                    repeats = 4)
                          norm.train$hosp_exp_flg<-ifelse(norm.train$hosp_exp_flg=="1","oui","non")
                          fit1<- train (hosp_exp_flg~., data = norm.train, method="svmpoly",
                                        tuneGrid= gridsearch,
                                        trControl = fit_control,
                                        metric = "F1"))
#définition de la grille d'hyperparamètre à tester
gridsearch <-  expand.grid( 
  scale  = c(1/100000,1/10000,1/1000,1/100,1/10), 
  C = c(1,2,3,4),
  degree = c(1,2,3,4,5)))
#Entrainement d'un svm par crossvalidation
fit1<- train (hosp_exp_flg~., data = norm.train, method="svmpoly",
                          tuneGrid= gridsearch,
                          trControl = fit_control,
                          metric = "F1"))


saveRDS(mod_svm,"./meilleur_modele")
saveRDS(normParam,"./normParam")

fonction_prediction<- function(bdd,normParam, modele){
  bdd<-bdd%>%select( "age", "gender_num", 
                     "bmi", "sapsi_first", "sofa_first", "chf_flg", "afib_flg", "renal_flg", 
                     "liver_flg", "copd_flg", "cad_flg", "stroke_flg", "mal_flg", 
                     "resp_flg", "map_1st", "hr_1st", "temp_1st", "spo2_1st", "abg_count", 
                     "wbc_first", "hgb_first", "platelet_first", "sodium_first", "potassium_first", 
                     "tco2_first", "chloride_first", "bun_first", "creatinine_first", 
                     "po2_first", "pco2_first", "iv_day_1")
  bdd_norm<- predict(normParam,bdd)
  pred <- predict(modele,bdd)
  return(pred)
  }
mod_svm<-readRDS("./meilleur_modele")
normParam<-readRDS("./normParam")
fonction_prediction(data_tot,normParam = normParam,modele = mod_svm)
```
