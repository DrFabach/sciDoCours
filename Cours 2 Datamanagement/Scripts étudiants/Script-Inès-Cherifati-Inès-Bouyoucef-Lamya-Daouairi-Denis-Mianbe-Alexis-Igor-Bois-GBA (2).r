#installation d'Excel pour importation des données
install.packages("readxl")
library(xl)

#création de la fonction pour lire les données du fichier Excel.
#lecture du document grace à "Print"
base <-read_excel("data1.xlsx")
print(base)

#mise en évidence de l'en-tête grace à "head"
head(base)
#mise en évidence de la structure du document
str(base)
#Tête de série : hétérogène quantitative discrète 
#Date de lancement /date de la dernière action : date 
#Id : ? 
#Sexe du médecin : qualitatif nominal
#Délai de prise en charge : quantitative discrète 
#Médecin adresseur : qualitatif nominal 
#Pathologie : descriptif qualitative fonction booléenne information binaire (oui/non)
#Diagnostic posé : texte 
#Nombre d’examen : quantitatif discret 
#Type d’examen : descriptive 
#Type d’examen complémentaire : (qualitatif nominal) 
#Nombre de consultations : quantitatif discret 
#Autre spécialiste : qualitatif nominal

                                                                            
#mise en évidence des éléments manquants (N/A)
is.na(base)
#mise en évidence des éléments manquantes dans les lignes puis dans les colones.
rowSums(is.na(base))
colSums(is.na(base))

#suppression des données manquantes, affichage.
base2 <-na.omit(base)
base2

#importation des éléments de la feuille n°2.
base3 <- read_excel("data1.xlsx", sheet = 2)
#lecture du document grace à "Print"
print(base3)
#mise en évidence des éléments manquants (N/A)
is.na(base3)

#suppression des données manquantes, affichage.
base4 <- na.omit(base3)
base3

#conversion de typologie en mode tidy.
base$"Sexe du Medecin traitant[base == Masculin]<-M
base$"Sexe du medcin traitant[base == Féminin]<-F
base$`Medecin adresseur`[base2 == "Généraliste"]<-0
base$`Medecin adresseur`[base2 == "Spécialiste"]<-1

#Tentative de joindre les sheets 
basefinal<-base2%>%left_join(base4,by =c("id" = "id"))

