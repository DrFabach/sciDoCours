
# Ceci est un commentaire

?install.packages()
# installation du package readxl, une seul fois
# install.packages("readxl")

#Chargement du package
library("readxl")
# Chemin du fichier, remplacer \ par /
read_excel("C:/Users/enseignant/Desktop/sciDoCours-master/data1.xlsx")
data1 <- read_excel("C:/Users/enseignant/Desktop/sciDoCours-master/data1.xlsx",
                   1,na =c(" ","","N/A","NA"))
data2 <- read_excel(path ="C:/Users/enseignant/Desktop/sciDoCours-master/data1.xlsx",
                    sheet = 2)

library(dplyr)


str(data1)

dput(names(data1))

names(data1)<-
c("ID de la réponse", "Date de soumission", "Dernière page", 
  "Langue de départ", "Tête de série", "Date de lancement", "Date de la dernière action", 
  "...8", "ID :", "Sexe du medcin traitant", "Date de la première consultation", 
  "Délais de prise en charge (mois)", "Délais de RDV (mois)", "Medecin adresseur", 
  "Pathologie lié au travail ? [AT]", "Pathologie lié au travail ? [AT: Commentaire]", 
  "Pathologie lié au travail ? [AT non reconnu]", "Pathologie lié au travail ? [AT non reconnu : Commentaire]", 
  "Pathologie lié au travail ? [Autre]", "Pathologie lié au travail ? [Autre : Commentaire]", 
  "Sexe du medecin adresseur", "Adressé par  [Rhumato]", "Quel est le diagnostique posé avant la consultation ?", 
  "Nombre d'examen complémentaires : [Echographie]", "Nombre d'examen complémentaires : [Scanner]", 
  "Nombre d'examen complémentaires : [IRM]", "Nombre d'examen complémentaires : [Scintigraphie]", 
  "Nombre d'examen complémentaires : [radio]", "Nombre d'examen complémentaires : [Bio]", 
  "Nombre d'examen complémentaires : [Biopsie]", "Type examen complementaire : [echo]", 
  "Type examen complementaire : [echo : Commentaire]", "Type examen complementaire : [tdm]", 
  "Type examen complementaire : [tdm: Commentaire]", "Type examen complementaire : [IRM]", 
  "Type examen complementaire : [IRM: Commentaire]", "Nombre de consultation : [Chir rachis]", 
  "Nombre de consultation : [Rhumato]", "Nombre de consultation : [Chir Ortho]", 
  "Nombre de consultation : [Neuro]", "Nombre de consultation : [MPR]", 
  "Nombre de consultation : [Onco]", "Nombre de consultation : [Neurochir]", 
  "Nombre de consultation : [Interniste]", "Quel autre spécialiste consulté"
)


# notion de vecteur
c(1,2,3,4)

c("a","b","c")

c("a",1,"c")

vecteur<- c("a","b","c")
vecteur2<- vecteur
vecteur3<- c(vecteur, vecteur2)


data1$`Pathologie lié au travail ? [AT]`
#Modification du nom de la variable pour être tidy
make.names(" le nom de la variable")

names(data1)<- make.names(names(data1))

data1$Pathologie.lié.au.travail....AT..Commentaire.

#verifier nombre de patient et variable
dim(data1)

#Selection des variables
## base R
data1$ID.de.la.réponse
data1[1,1:3]
data1[,1:3]

data1[,-1]
data1[,-c(1,3,4)]

data1[,c("ID.de.la.réponse")]
names(data1)=="ID.de.la.réponse"

data1[,!(names(data1)=="ID.de.la.réponse")]

#head(data1)

data1[,!(names(data1)%in%c("ID.de.la.réponse", "Date.de.soumission", "Dernière.page", 
                         "Langue.de.départ", "Tête.de.série", "Date.de.lancement", "Date.de.la.dernière.action", 
                         "...8"))]

# allerauboulot(preparer(dejeuner(jemeleve(moi))))
# var1<- jemeleve(moi)
# va2<- dejeuner(var1)
# .
# .

## Version dplyr
#moi%>%jemeleve%>%dejeuner%>%preparer%>%allerauboulot
data1%>%dim
data1%>%names%>%dput

data1 %>% select(ID.de.la.réponse)
data1 %>% select(c(Nombre.de.consultation....Neurochir.,Type.examen.complementaire....IRM.))
data1%>%select(-ID.de.la.réponse)
# Déselection des variables sans interêt
data1<-data1%>%select(-c("ID.de.la.réponse", "Date.de.soumission", "Dernière.page", 
                  "Langue.de.départ", "Tête.de.série", "Date.de.lancement", "Date.de.la.dernière.action", 
                  "...8"))



# Changer le type de données :
## base R
as.numeric(c('1','2','3'))
as.character(c(1,2,3))
as.factor(c(2,3,4))

# Attention pas de as.numeric directement sur un as.factor
as.factor(c(4,3,2))%>%as.character()%>%as.numeric()


data1$Sexe.du.medcin.traitant<-as.factor(data1$Sexe.du.medcin.traitant)
data1$Medecin.adresseur<- as.factor(data1$Medecin.adresseur)
## dplyr
data1<-data1%>%mutate(Sexe.du.medcin.traitant=as.factor(Sexe.du.medcin.traitant),
                      Medecin.adresseur = as.factor(Medecin.adresseur))

data1<-data1%>%mutate_at(c("Sexe.du.medcin.traitant","Medecin.adresseur"),
                  as.factor)


# data1%>%mutate_at(c("varnumeric1","varnumeric2"),
#                   as.numeric)

str(data1)


#Modification des variables
#Base R
data1$Sexe.du.medcin.traitant[which(data1$Sexe.du.medcin.traitant==45)]<- NA
data1$Sexe.du.medcin.traitant%>%droplevels()
data1$Sexe.du.medcin.traitant[which(data1$Sexe.du.medcin.traitant=="f")]<- "Féminin"


#dplyr
data1$Sexe.du.medcin.traitant<- as.character(data1$Sexe.du.medcin.traitant)
data1<-data1%>%mutate(Sexe.du.medcin.traitant = case_when(Sexe.du.medcin.traitant==45~NA_character_,
                                                   Sexe.du.medcin.traitant=="f"~"Féminin",
                                                   Sexe.du.medcin.traitant=="m"~"Masculin",
                                                   TRUE ~Sexe.du.medcin.traitant)%>%as.factor)


#Remplacement des données manquantes
#base R
data1[,c("Nombre.d.examen.complémentaires....Scintigraphie.", 
         "Nombre.d.examen.complémentaires....radio.", "Nombre.d.examen.complémentaires....Bio.", 
         "Nombre.d.examen.complémentaires....Biopsie.")][is.na(data1[,c("Nombre.d.examen.complémentaires....Scintigraphie.", 
                                                                  "Nombre.d.examen.complémentaires....radio.", "Nombre.d.examen.complémentaires....Bio.", 
                                                                  "Nombre.d.examen.complémentaires....Biopsie.")])]<-0

# data[,indice][is.na(data[,indice])]<-0
data1$Nombre.de.consultation....Interniste.[is.na(data1$Nombre.de.consultation....Interniste.)]<-0

##dplyr 

data1%>%mutate_at(c("Nombre.d.examen.complémentaires....Scintigraphie.", 
                    "Nombre.d.examen.complémentaires....radio.", "Nombre.d.examen.complémentaires....Bio.", 
                    "Nombre.d.examen.complémentaires....Biopsie."), 
                  function(x) x[is.na(x)]<-0)%>%select(Nombre.d.examen.complémentaires....Scintigraphie.)



## extraction de termes
library(stringr)
data1$fibromyalgie <-str_detect(data1$Quel.est.le.diagnostique.posé.avant.la.consultation..,"fibromyalgie")


## Joindre base de données
basefinal<-data1%>%left_join(data2,by =c("ID.." = "ID : "))

write.csv2(basefinal,"C:/Users/enseignant/Desktop/basefinal.csv")
