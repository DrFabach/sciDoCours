#============================== DM1 Nihel Ryma et Sofia ============================ 
#install.packages("readxl")
#install.packages("dplyr")
library("readxl")
library("dplyr")

# chemin relatif car création d'un projet R
data1 <- read_excel("data1.xlsx", sheet = 1)
data2 <- read_excel("data1.xlsx", sheet = 2)

str(data1)
str(data2)

#=====================================================================================
names(data1)
# plusieurs colonnes ont le même nom /!\
# on renomme alors les colonnes R P et T de data1 :
data1 = rename(data1,"Pathologie lié au travail ? [Commentaire sur l'AT]"="Pathologie lié au travail ? [Commentaire]...16")
data1 = rename(data1,"Pathologie lié au travail ? [Commentaire sur l'AT non reconnu]"="Pathologie lié au travail ? [Commentaire]...18")
data1 = rename(data1,"Pathologie lié au travail ? [Commentaire sur autre"="Pathologie lié au travail ? [Commentaire]...20")


#on renomme les colonnes B et C de data2 :
#vu que le sexe du médecin existe dans la data1 et que la colonne est différente de la B
#on deduit que la colonne B est le sexe du patient
data2 = rename(data2,"Sexe du patient"=Sexe)
#On suppose que l'age conserne celui du patient car c'est sur lui que sont fait les examens
data2 = rename(data2,"Age du patient"=Age)

# join de data1 et data2 :
data1 = rename(data1,"ID"="ID :")
data2 = rename(data2,"ID"="ID : ")
bdd<-data2%>%left_join(data1,by =c("ID" = "ID"))
str(bdd)
names(bdd)


#on remarque que la colonne H sur excel a finalement un nom dans cet IDE: ...8
print(select(bdd,"...8"))

# supressions des 4 colonnes vides:
select(bdd,-c("...8","Pathologie lié au travail ? [Commentaire sur l'AT non reconnu]","Nombre de consultation : [Onco]","Nombre de consultation : [Neurochir]"))

# homogeneisation dernière page
bdd$"Dernière page"[which(bdd$"Dernière page"==-1)]<-NA

#string dans la colonne alors que l'on veut que des valeurs numériques
bdd$"Délais de prise en charge (mois)"[which(bdd$"Délais de prise en charge (mois)"=="4  mois")]<-4

#homogénéisation du sexe du médecin

bdd$"Sexe du medcin traitant"[which(bdd$"Sexe du medcin traitant"=="f")]<-"Féminin"
bdd$"Sexe du medcin traitant"[which(bdd$"Sexe du medcin traitant"=="m")]<-"Masculin"
bdd$"Sexe du medcin traitant"[which(bdd$"Sexe du medcin traitant"=="45")]<-NA_character_

#homogeneisation Medecin adresseur
bdd$"Medecin adresseur"[which(bdd$"Medecin adresseur"=="generilste")]<-"Généraliste"

#repositionnement des colonnes (jugé plus logique de procéder ainsi) :

bdd %>% relocate("Nombre d'examen complémentaires : [Echographie]", .after = "Type examen complementaire : [echo]")
bdd %>% relocate("Nombre d'examen complémentaires : [Scanner]", .after = "Type examen complementaire : [tdm]")
bdd %>% relocate("Nombre d'examen complémentaires : [IRM]", .after = "Type examen complementaire : [IRM]")

#"N/A" != NA l'un est un string l'autre est un logical
# conversion des NA string en logical :
bdd[bdd == "N/A"]<-NA

#on retourne le fichier nettoyé
write.csv2(bdd,"bddFinal.csv")

