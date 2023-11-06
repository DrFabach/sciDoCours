#Visualisation de la database Infert
df <- infert
table(df)

#Mesures pour la variable quantitative Age
age_stats <- c(
  Moyenne = mean(df$age, na.rm = TRUE),
  Médiane = median(df$age, na.rm = TRUE),
  Variance = var(df$age, na.rm = TRUE),
  EcartType = sd(df$age, na.rm = TRUE),
  Minimum = min(df$age, na.rm = TRUE),
  Maximum = max(df$age, na.rm = TRUE),
  Etendue = diff(range(df$age, na.rm = TRUE)),
  Quartile1 = quantile(df$age, 0.25, na.rm = TRUE)[[1]],
  Quartile3 = quantile(df$age, 0.75, na.rm = TRUE)[[1]]
)

#Mesures pour la variable quantitative pooled.stratum
pooled_stratum_stats <- c(
  MoyennePS = mean(df$pooled.stratum, na.rm = TRUE),
  MédianePS = median(df$pooled.stratum, na.rm = TRUE),
  VariancePS = var(df$pooled.stratum, na.rm = TRUE),
  EcartTypePS = sd(df$pooled.stratum, na.rm = TRUE),
  MinimumPS = min(df$pooled.stratum, na.rm = TRUE),
  MaximumPS = max(df$pooled.stratum, na.rm = TRUE),
  EtenduePS = diff(range(df$pooled.stratum, na.rm = TRUE)),
  Quartile1PS = quantile(df$pooled.stratum, 0.25, na.rm = TRUE)[[1]],
  Quartile3PS = quantile(df$pooled.stratum, 0.75, na.rm = TRUE)[[1]]
)




#Dataframe bilan
summary_df <- data.frame(
  Age = age_stats,
  PooledStratum = pooled_stratum_stats
)

#Pour avoir les mêmes noms de ligne pour les mesures de Age et Pooled Stratum

row.names(summary_df) <- c("Moyenne", "Médiane", "Variance", "EcartType", "Minimum", "Maximum", "Etendue", "Quartile1", "Quartile3")

#affiche le tableau-bilan
print(summary_df)







#Lien entre les variables qualitatives education et induced

df$education <-as.factor(df$education)
df$induced <- as.factor (df$induced)
df$case <- as.factor (df$case)

by(df$education, df$induced, summary)


#utiliser dplyr pour obtenir des tableaux complexes (mesures en fonction de case)
#Avec pooled stratum
library(dplyr)

df %>%
  group_by(case) %>%
  summarise(
    moyenne = mean(pooled.stratum, na.rm = TRUE),
    mediane = median(pooled.stratum, na.rm = TRUE),
    ecart_type = sd(pooled.stratum, na.rm = TRUE),
    variance = var(pooled.stratum, na.rm = TRUE),
    Minimum = min(pooled.stratum, na.rm = TRUE),
    Maximum = max(pooled.stratum, na.rm = TRUE),
    Etendue = diff(range(pooled.stratum, na.rm = TRUE)),
    Quartile1 = quantile(pooled.stratum, 0.25, na.rm = TRUE)[[1]],
    Quartile3 = quantile(pooled.stratum, 0.75, na.rm = TRUE)[[1]]
  )

#pareil avec age
library(dplyr)

df %>%
  group_by(case) %>%
  summarise(
    moyenne = mean(age, na.rm = TRUE),
    mediane = median(age, na.rm = TRUE),
    ecart_type = sd(age, na.rm = TRUE),
    variance = var(age, na.rm = TRUE),
    Minimum = min(age, na.rm = TRUE),
    Maximum = max(age, na.rm = TRUE),
    Etendue = diff(range(age, na.rm = TRUE)),
    Quartile1 = quantile(age, 0.25, na.rm = TRUE)[[1]],
    Quartile3 = quantile(age, 0.75, na.rm = TRUE)[[1]]
  )





#pour les variables qualitatives, il est pertinent de mesurer la fréquence des variables

prop.table(table(df$education))
prop.table(table(df$case))



#exemple avec âge selon la variable case (quanti-quali)

moyenne_ageCase1 <- mean(df$age[which(df$case==1)])
moyenne_PsCase1 <- mean(df$pooled.stratum[which(df$case==1)])
data.frame(moyenne_ageCase1, moyenne_PsCase1)

moyenne_ageCase0 <- mean(df$age[which(df$case==0)])
moyenne_PsCase0 <- mean(df$pooled.stratum[which(df$case==0)])
data.frame(moyenne_ageCase0, moyenne_PsCase0)

#exemple avec des moyennes du nombre pooled stratum selon la variable education (quanti-quali)

moyenne_0à5 <- mean(df$pooled.stratum[which(df$education=="0-5yrs")])
moyenne_6à11 <- mean(df$pooled.stratum[which(df$education=="6-11yrs")])
moyenne_12etplus <- mean(df$pooled.stratum[which(df$education=="12+ yrs")])

data.frame(moyenne_0à5, moyenne_6à11, moyenne_12etplus)



