
#Chargement du package
library("readxl")
# Ouverture des fichier
data1 <- read_excel("Cours 2 Datamanagement/data1.xlsx",
                    1,na =c(" ","","N/A","NA"))
data2 <- read_excel(path ="Cours 2 Datamanagement/data1.xlsx",
                    sheet = 2)

library(dplyr)

names(data1)<-c("ID de la réponse", "Date de soumission", "Dernière page", 
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


names(data1)<- data1%>%names%>%make.names()
# Suppression des variables non intÃ©ressante
data1<-data1%>%select(-c("ID.de.la.réponse", "Date.de.soumission", "Dernière.page", 
                         "Langue.de.départ", "Tête.de.série", "Date.de.lancement", "Date.de.la.dernière.action", 
                         "...8"))

BDD<-data1%>%left_join(data2,by =c("ID.." = "ID : "))


BDD<- BDD%>%select(-ID..)

BDD<-BDD%>%mutate(Sexe.du.medcin.traitant = case_when(Sexe.du.medcin.traitant==45~NA_character_,
                                                          Sexe.du.medcin.traitant=="f"~"Féminin",
                                                          Sexe.du.medcin.traitant=="m"~"Masculin",
                                                          TRUE ~Sexe.du.medcin.traitant)%>%as.factor)

library(stringr)
BDD$Délais.de.RDV..mois.<- str_extract(BDD$Délais.de.prise.en.charge..mois.,"[1-9]*")%>%as.numeric()

levels(as.factor(BDD$Medecin.adresseur))
BDD$Medecin.adresseur<- ifelse(BDD$Medecin.adresseur=="generilste", "Généraliste", BDD$Medecin.adresseur)


BDD$Quel.est.le.diagnostique.posé.avant.la.consultation..%>%strsplit("\\+")%>%unlist()%>%table
#ajouter douleur neuropathique
#tendinite/tendinopatihe
#algodsystrophie
#céphalées
#fibromyalgie

BDD$douleur_Neuropathique<- ifelse(str_detect(BDD$Quel.est.le.diagnostique.posé.avant.la.consultation..%>%tolower(),
                                              "neuropath"),"oui","non")
BDD$douleur_tendineuses<- ifelse(str_detect(BDD$Quel.est.le.diagnostique.posé.avant.la.consultation..%>%tolower(),
                                              "tendin"),"oui","non")
BDD$algodsystrophie<- ifelse(str_detect(BDD$Quel.est.le.diagnostique.posé.avant.la.consultation..%>%tolower(),
                                            "algodsystrop"),"oui","non")
BDD$cephalee<- ifelse(str_detect(BDD$Quel.est.le.diagnostique.posé.avant.la.consultation..%>%tolower(),
                                        "c[ée]phal"),"oui","non")
BDD$fibromyalgie<- ifelse(str_detect(BDD$Quel.est.le.diagnostique.posé.avant.la.consultation..%>%tolower(),
                                 "fibrom[yi]algi"),"oui","non")
BDD<- BDD%>%select(-Quel.est.le.diagnostique.posé.avant.la.consultation..)



BDD<-BDD%>%mutate_at(c("Nombre.d.examen.complémentaires....Echographie.", 
                       "Nombre.d.examen.complémentaires....Scanner.", "Nombre.d.examen.complémentaires....IRM.", 
                       "Nombre.d.examen.complémentaires....Scintigraphie.", "Nombre.d.examen.complémentaires....radio.", 
                       "Nombre.d.examen.complémentaires....Bio.", "Nombre.d.examen.complémentaires....Biopsie.",
                       "Nombre.de.consultation....Chir.rachis.", "Nombre.de.consultation....Rhumato.", 
                       "Nombre.de.consultation....Chir.Ortho.", "Nombre.de.consultation....Neuro.", 
                       "Nombre.de.consultation....MPR.", "Nombre.de.consultation....Onco.", 
                       "Nombre.de.consultation....Neurochir.", "Nombre.de.consultation....Interniste."), 
                  function(x) x[is.na(x)]<-0)




decoupe<-  function (x, char = ";", nom = NULL){
  require(stringr)
  require(purrr)
  require(dplyr)
  decoupe_df<- function(nom, df, char=char){
    if(sum(str_detect(df[,nom],pattern = char))>0){
      return(decoupe(df[,nom], nom =nom, char = char))
      
    }else{
      return(df%>%select(nom))
      
    }
  }
  
  
  if( is.data.frame(x)){
    
    
    return(map_dfc(names(x), function(y) decoupe_df( x[,y], char)))
  }else{
    if(is.null(nom)) nom<- gsub(".*?\\$","",deparse(substitute(x)))
    LL <- strsplit(as.character(x), char)
    modalites <- names(table(unlist(LL)))
    ncolmax <- length(modalites)
    MAT <- matrix(0, nrow = length(LL), ncol = ncolmax)
    colnames(MAT) <- modalites
    for (i in 1:length(LL)) {
      MAT[i, match(LL[[i]], colnames(MAT))] <- 1
    }
    res<-as.data.frame(MAT)
    names(res)<-paste(nom, colnames(MAT), sep="_")
    return(res)
  }
  
}

var_a_decoupe<- c("Type.examen.complementaire....echo...Commentaire.",
                  "Type.examen.complementaire....tdm..Commentaire.",
                  "Type.examen.complementaire....IRM..Commentaire.")

BDD_a_decoupe<- BDD%>%select(var_a_decoupe)
BDD<- BDD%>%select(-var_a_decoupe)
BDD_a_decoupe<- decoupe(BDD_a_decoupe$Type.examen.complementaire....echo...Commentaire.,char = ", ?")
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
data1$Sexe.du.medcin.traitant[which(data1$Sexe.du.medcin.traitant=="f")]<- "F?minin"


#dplyr
data1$Sexe.du.medcin.traitant<- as.character(data1$Sexe.du.medcin.traitant)
data1<-data1%>%mutate(Sexe.du.medcin.traitant = case_when(Sexe.du.medcin.traitant==45~NA_character_,
                                                          Sexe.du.medcin.traitant=="f"~"F?minin",
                                                          Sexe.du.medcin.traitant=="m"~"Masculin",
                                                          TRUE ~Sexe.du.medcin.traitant)%>%as.factor)


#Remplacement des donn?es manquantes
#base R
data1[,c("Nombre.d.examen.compl?mentaires....Scintigraphie.", 
         "Nombre.d.examen.compl?mentaires....radio.", "Nombre.d.examen.compl?mentaires....Bio.", 
         "Nombre.d.examen.compl?mentaires....Biopsie.")][is.na(data1[,c("Nombre.d.examen.compl?mentaires....Scintigraphie.", 
                                                                        "Nombre.d.examen.compl?mentaires....radio.", "Nombre.d.examen.compl?mentaires....Bio.", 
                                                                        "Nombre.d.examen.compl?mentaires....Biopsie.")])]<-0

# data[,indice][is.na(data[,indice])]<-0
data1$Nombre.de.consultation....Interniste.[is.na(data1$Nombre.de.consultation....Interniste.)]<-0

##dplyr 

data1%>%mutate_at(c("Nombre.d.examen.compl?mentaires....Scintigraphie.", 
                    "Nombre.d.examen.compl?mentaires....radio.", "Nombre.d.examen.compl?mentaires....Bio.", 
                    "Nombre.d.examen.compl?mentaires....Biopsie."), 
                  function(x) x[is.na(x)]<-0)%>%select(Nombre.d.examen.compl?mentaires....Scintigraphie.)



## extraction de termes
library(stringr)
data1$fibromyalgie <-str_detect(data1$Quel.est.le.diagnostique.pos?.avant.la.consultation..,"fibromyalgie")


## Joindre base de donn?es
basefinal<-data1%>%left_join(data2,by =c("ID.." = "ID :?"))

write.csv2(basefinal,"C:/Users/enseignant/Desktop/basefinal.csv")
