install.packages("tidyverse")
install.packages("mlr3")
install.packages("mlr3learners")
install.packages("ranger")
library(tidyverse)
library(mlr3)
library(mlr3learners)
library(ggplot2)
library(ranger)

dataini <- readRDS("data.rds")
{
# glimpse(dataini)
}
# preparation
dataini$hosp_exp_flg <- as.factor(dataini$hosp_exp_flg)
task <- TaskClassif$new("hospred", dataini, target = "hosp_exp_flg")
train_set = sample(task$row_ids, 0.8 * task$nrow)
test_set = setdiff(task$row_ids, train_set)

# initialisation de la Regression Logistique
learner_logreg <- lrn("classif.log_reg")
{
#print(learner_logreg)
}
# entrainer
learner_logreg$train(task, row_ids = train_set)
{
# learner_logreg$model
# summary(learner_logreg$model)
}
# initialisation du Random Forest
learner_rf <- lrn("classif.ranger", importance = "permutation")

# entrainer
learner_rf$train(task, row_ids = train_set)
{
# learner_rf$importance()
}
# utiliser ggplot pour dessiner la graphe d'importance
importance = as.data.table(learner_rf$importance(), keep.rownames = TRUE)
colnames(importance) = c("Feature", "Importance")
ggplot(data=importance,
       aes(x = reorder(Feature, Importance), y = Importance))+ 
       geom_col() + coord_flip() + xlab("")

# commencer a la prediction utilisant donnees de test
pred_logreg <- learner_logreg$predict(task, row_ids = test_set)
pred_rf <- learner_rf$predict(task, row_ids = test_set)

# Matrice de confusion
pred_logreg$confusion
pred_rf$confusion

# Validation Croisee
cvtest <- rsmp("cv", folds = 10)
res_logreg <- resample(task, learner = learner_logreg, resampling = cvtest)
res_logreg
res_logreg$aggregate()
res_rf <- resample(task, learner = learner_rf, resampling = cvtest)
res_rf
res_rf$aggregate()

# Trouver les parametres a tuner
{
# learner_logreg$param_set
# learner_rf$param_set
}

# mtry = floor(sqrt(ncol(data) - 1)) ici floor(sqrt(46-1)) = 6 

# Tuner hyperparametres num.trees & mtry
# num.trees: nombre d'arbre decision
# mtry: sampling de variable de chaque fois
rf_def = lrn("classif.ranger", id = "default", predict_type = "prob")

rf_low = lrn("classif.ranger", id = "low", predict_type = "prob",
             num.trees = 100, mtry = 3)

rf_high = lrn("classif.ranger", id = "high", predict_type = "prob",
              num.trees = 1000, mtry = 12)

learners_rf = list(rf_low, rf_def, rf_high)
hypertest_rf = benchmark_grid(task, learners = learners_rf, resamplings = cvtest)
bmr_rf = benchmark(hypertest_rf)
# Comparer coefficient d'erreur et auc des 3 
measures = msrs(c("classif.ce", "classif.auc"))
performances_rf = bmr_rf$aggregate(measures)
performances_rf[, .(learner_id, classif.ce, classif.auc)]



# Tuner hyperparametres maxit & epsilon
# maxit: maximum fois d'iteration
logreg_def = lrn("classif.log_reg", id = "default", predict_type = "prob")

logreg_low = lrn("classif.log_reg", id = "low", predict_type = "prob",
             maxit = 12, epsilon = 0.0000000001)

logreg_high = lrn("classif.log_reg", id = "high", predict_type = "prob",
              maxit = 50, epsilon = 0.00001)

learners_logreg = list(logreg_low, logreg_def, logreg_high)
hypertest_logreg = benchmark_grid(task, learners = learners_logreg, resamplings = cvtest)
bmr_logreg = benchmark(hypertest_logreg)
# Comparer coefficient d'erreur et auc des 3 
measures = msrs(c("classif.ce", "classif.auc"))
performances_logreg = bmr_logreg$aggregate(measures)
performances_logreg[, .(learner_id, classif.ce, classif.auc)]

# On peut voir que rf.ce<logreg.ce, rf.auc>logreg.auc
# Donc on choisit modele de Random Forest


saveRDS(learner_rf, "./meilleur_model")

pred_var <- function(bdd, var, model){
  bdd$var <- as.factor(bdd$var)
  task <- TaskClassif$new("new", bdd, target = as.character(var))
  pred <- model$predict(task, row_ids = task$row_ids)
  return(pred)
}
