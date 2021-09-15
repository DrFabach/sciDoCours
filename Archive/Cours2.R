library("xlsx")

decoupe<-  function (x, char = ";", nom = NULL){
  require(stringr)
  require(purrr)
  require(dplyr)
  decoupe_df<- function(nom, df, char){
    if(sum(str_detect(df[,nom],pattern = char))>0){
      return(decoupe(df[,nom], nom =nom, char = char))
      
    }else{
      return(df%>%select(nom))
      
    }
  }
  
  
  if( is.data.frame(x)){
    
    
    return(map_dfc(names(x), function(y) decoupe_df(y, x, char)))
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





df2<-read.xlsx("data1.xlsx",sheetIndex = 1,check.names = F)


table(names(df2)%in% names(df1))

names(df1)[!  (names(df1)%in% names(df2))]

library(tidyverse)
df<- rbind(df1, df2)

for(i in 1:(dim(df)[2])) df[,i]<-ifelse(df[,i]=="N/A",NA,df[,i])

names(df)<- c("ID de la réponse", "Date de soumission", "Dernière page", 
              "Langue de départ", "Tête de série", "Date de lancement", "Date de la dernière action", 
              "ID : ", "Sexe", "Age", "Sexe du medcin traitant ", "Date de la première consultation", 
              "Délais de prise en charge (mois)", "Délais de RDV (mois)", "Medecin adresseur", 
              "Pathologie lié au travail ? [AT]", "Pathologie lié au travail ? [Commentaire]", 
              "Pathologie lié au travail ? [AT non reconnu]", "Pathologie lié au travail ? [Commentaire]", 
              "Pathologie lié au travail ? [Autre]", "Pathologie lié au travail ? [Commentaire]", 
              "Sexe du medecin adresseur", "Adressé par  [Rhumato]", "Adressé par  [Commentaire]", 
              "Adressé par  [Chir Ortho]", "Adressé par  [Commentaire]", "Adressé par  [Neuro]", 
              "Adressé par  [Commentaire]", "Adressé par  [MPR]", "Adressé par  [Commentaire]", 
              "Adressé par  [Oncologie]", "Adressé par  [Commentaire]", "Adressé par  [Autre]", 
              "Adressé par  [Commentaire]", "Quel est le diagnostique posé avant la consultation ?", 
              "Diagnostic après la première consultation (séparé par un plus)", 
              "Type de syndrome douloureux :  [Nociceptif]", "Type de syndrome douloureux :  [Commentaire]", 
              "Type de syndrome douloureux :  [Nociplastique]", "Type de syndrome douloureux :  [Commentaire]", 
              "Type de syndrome douloureux :  [Neuropathique]", "Type de syndrome douloureux :  [Commentaire]", 
              "Type de syndrome douloureux :  [Autre]", "Type de syndrome douloureux :  [Commentaire]", 
              "Nombre d'examen complémentaires : [Echographie]", "Nombre d'examen complémentaires : [Scanner]", 
              "Nombre d'examen complémentaires : [IRM]", "Nombre d'examen complémentaires : [Scintigraphie]", 
              "Nombre d'examen complémentaires : [radio]", "Nombre d'examen complémentaires : [Bio]", 
              "Nombre d'examen complémentaires : [Biopsie]", "Type examen complementaire : [echo]", 
              "Type examen complementaire : [Commentaire]", "Type examen complementaire : [tdm]", 
              "Type examen complementaire : [Commentaire]", "Type examen complementaire : [IRM]", 
              "Type examen complementaire : [Commentaire]", "Type examen complementaire : [scinti]", 
              "Type examen complementaire : [Commentaire]", "Type examen complementaire : [radio]", 
              "Type examen complementaire : [Commentaire]", "Type examen complementaire : [Bio]", 
              "Type examen complementaire : [Commentaire]", "Type examen complementaire : [Biopsie]", 
              "Type examen complementaire : [Commentaire]", "Quels autre examens complémentaires ?", 
              "Nombre examen éléctrophysiologique [EMG]", "Nombre examen éléctrophysiologique [ECG]", 
              "Nombre examen éléctrophysiologique [Epreuve d'effort]", "Nombre examen éléctrophysiologique [EEG]", 
              "Nombre examen éléctrophysiologique [polysomno]", "Nombre examen éléctrophysiologique [Autres]", 
              "Quels autres examens electrophysiologique ? ", "Quels autres examens ? ", 
              "Nombre de consultation : [Chir rachis]", "Nombre de consultation : [Rhumato]", 
              "Nombre de consultation : [Chir Ortho]", "Nombre de consultation : [Neuro]", 
              "Nombre de consultation : [MPR]", "Nombre de consultation : [Onco]", 
              "Nombre de consultation : [Neurochir]", "Nombre de consultation : [Interniste]", 
              "Quel autre spécialiste consulté")


df<- df[,c( "ID : ","Sexe", "Age", "Sexe du medcin traitant ", 
            "Délais de prise en charge (mois)", "Délais de RDV (mois)", "Medecin adresseur", 
            "Pathologie lié au travail ? [AT]", "Pathologie lié au travail ? [Commentaire]", 
            "Pathologie lié au travail ? [AT non reconnu]", "Pathologie lié au travail ? [Commentaire]", 
            "Pathologie lié au travail ? [Autre]", "Pathologie lié au travail ? [Commentaire]", 
            "Sexe du medecin adresseur", "Adressé par  [Rhumato]", "Adressé par  [Commentaire]", 
            "Adressé par  [Chir Ortho]", "Adressé par  [Commentaire]", "Adressé par  [Neuro]", 
            "Adressé par  [Commentaire]", "Adressé par  [MPR]", "Adressé par  [Commentaire]", 
            "Adressé par  [Oncologie]", "Adressé par  [Commentaire]", "Adressé par  [Autre]", 
            "Adressé par  [Commentaire]", "Quel est le diagnostique posé avant la consultation ?", 
            "Diagnostic après la première consultation (séparé par un plus)", 
            "Type de syndrome douloureux :  [Nociceptif]", "Type de syndrome douloureux :  [Commentaire]", 
            "Type de syndrome douloureux :  [Nociplastique]", "Type de syndrome douloureux :  [Commentaire]", 
            "Type de syndrome douloureux :  [Neuropathique]", "Type de syndrome douloureux :  [Commentaire]", 
            "Type de syndrome douloureux :  [Autre]", "Type de syndrome douloureux :  [Commentaire]", 
            "Nombre d'examen complémentaires : [Echographie]", "Nombre d'examen complémentaires : [Scanner]", 
            "Nombre d'examen complémentaires : [IRM]", "Nombre d'examen complémentaires : [Scintigraphie]", 
            "Nombre d'examen complémentaires : [radio]", "Nombre d'examen complémentaires : [Bio]", 
            "Nombre d'examen complémentaires : [Biopsie]", "Type examen complementaire : [echo]", 
            "Type examen complementaire : [Commentaire]", "Type examen complementaire : [tdm]", 
            "Type examen complementaire : [Commentaire]", "Type examen complementaire : [IRM]", 
            "Type examen complementaire : [Commentaire]", "Type examen complementaire : [scinti]", 
            "Type examen complementaire : [Commentaire]", "Type examen complementaire : [radio]", 
            "Type examen complementaire : [Commentaire]", "Type examen complementaire : [Bio]", 
            "Type examen complementaire : [Commentaire]", "Type examen complementaire : [Biopsie]", 
            "Type examen complementaire : [Commentaire]", "Quels autre examens complémentaires ?", 
            "Nombre examen éléctrophysiologique [EMG]", "Nombre examen éléctrophysiologique [ECG]", 
            "Nombre examen éléctrophysiologique [Epreuve d'effort]", "Nombre examen éléctrophysiologique [EEG]", 
            "Nombre examen éléctrophysiologique [polysomno]", "Nombre examen éléctrophysiologique [Autres]", 
            "Quels autres examens electrophysiologique ? ", "Quels autres examens ? ", 
            "Nombre de consultation : [Chir rachis]", "Nombre de consultation : [Rhumato]", 
            "Nombre de consultation : [Chir Ortho]", "Nombre de consultation : [Neuro]", 
            "Nombre de consultation : [MPR]", "Nombre de consultation : [Onco]", 
            "Nombre de consultation : [Neurochir]", "Nombre de consultation : [Interniste]", 
            "Quel autre spécialiste consulté")]


df$nombre_examen_compl_autre<-decoupe(  df$`Quels autre examens complémentaires ?`,char = c("\\+|,|\\n"),"hh")%>%
  apply(1,sum)
df$total_examen_compl<- df%>%select(contains("Nombre d'examen comp"))%>%apply(1,function(x) sum(x,na.rm = T))+df$nombre_examen_compl_autre
df$nombre_exam_comp_diff<- df%>%select(contains("Nombre d'examen comp"))%>%apply(1,function(x) sum(x>0,na.rm = T))+df$nombre_examen_compl_autre


df$nombre_examen_elecro_autre<-   decoupe(  df$`Quels autres examens electrophysiologique ? `,char = c("\\+|,|\\n"),"hh")%>%
  apply(1,sum)

df$total_examen_physio<- df%>%select(contains("Nombre examen éléctroph"))%>%apply(1,function(x) sum(x,na.rm = T))+df$nombre_examen_elecro_autre

df$nombre_examen_physio_diff<- df%>%select(contains("Nombre examen éléctroph"))%>%apply(1,function(x) sum(x>0,na.rm = T))+df$nombre_examen_elecro_autre




a<-decoupe(  df$`Quel autre spécialiste consulté`,char = c("\\+|,|\\n"),"hh")

b<-str_extract(names(a),"[0-9]+")
b[is.na(b)]<-1
b<-as.numeric(b)

df$nombre_special_autre<-as.matrix(a)%*%b
df$nombre_special_tot<- df%>%select(contains("mbre de consultation"))%>%apply(1,function(x) sum(x,na.rm = T))+df$nombre_examen_elecro_autre

df$nombre_special_diff<- df%>%select(contains("mbre de consultation"))%>%apply(1,function(x) sum(x>0,na.rm = T))+a%>%apply(1,sum)



df<- df%>%select(-contains("Commentaire"))

df$`Délais de RDV (mois)`<-df$`Délais de RDV (mois)`%>%gsub(",",".",.)%>%as.numeric()

df<- df%>%filter(!is.na(`ID : `))%>%select(-`ID : `)



###############

B.prop2<- function(y,x,n.iter = 105000, n.burnin = 5000, n.thin = 1, 
                   prior.beta = c(1, 1, 1, 1), prec = 3){
  
  a<- 1
  b<- 1
  
  prior.beta = c(a, b, a, b)
  
  ylevel<-levels(as.factor(y))
  if(!length(ylevel)==2) stop("Plus de deux modalités")
  
  sink(file="rdata.txt")
  
  a<-B.prop(x[which(y==ylevel[1])], unlist(x)[which(y==ylevel[2])], n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin, prior.beta = prior.beta , prec = prec )
  sink()
  return(a)
}

tableQuanti<- function(BDD){
  resquanti<-BDD%>%desctable(list(n= length, moyenne = mean,sd = sd,fquar =  function(x) quantile(x,probs=0.25, na.rm = T),lquar =  function(x) quantile(x,probs=0.75, na.rm = T), median = median),
                             list(.default = ~no.test))
  
  
  resquanti[[2]]<-resquanti[[2]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  resquanti[[3]]<-resquanti[[3]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  resquanti$tests<- NULL
  
  
  matriceQuanti<-matrix(unlist(resquanti),ncol = 7)
  colnames(matriceQuanti)<- c("Variable","n","moy (sd)","median (1-3 quart)","n","moy (sd)","median (1-3 quart)")
  return(list(matriceQuanti= matriceQuanti, noms=names(resquanti)))
}


B.mean2<-function(x,y,n.iter = 105000, n.thin = 1, n.burnin = 5000){
  prior.norm = rep(c(mean(x, na.rm = T), 4/(max(x, na.rm = T)-min(x, na.rm = T))),2)
  mean(x, na.rm = T)
  ylevel<-levels(as.factor(y))
  if(!length(ylevel)==2) stop("Plus de deux modalités")
  sink("rdata.txt")
  a<-B.mean(x[which(y==ylevel[1])], unlist(x)[which(y==ylevel[2])],prior.norm = prior.norm , n.burnin = n.burnin, n.thin = n.thin)
  sink()
  return(a)
  
}

theme_elegant <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  
  half_line <- base_size/2
  
  theme_minimal(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size,
                base_rect_size = base_rect_size) %+replace%
    theme(
      ## Panel grid ##
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      ## Title ##
      plot.title = element_text(size = rel(1.3),
                                face = "plain", margin = margin(0, 0, 20, 0)),
      ## Axis ##
      axis.line = element_line(colour = "black", size = rel(0.5)),
      axis.title.y = element_text(margin = margin(t = 0, r = rel(20), b = 0, l = 0),
                                  angle = 90),
      axis.title.x = element_text(margin = margin(t = rel(20), r = 0, b = 0, l = 0)),
      axis.title = element_text(size = rel(1.2),
                                face = "plain"),
      axis.text = element_text(size = rel(.8)),
      axis.ticks = element_blank(),
      ## Legend ##
      legend.key = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = rel(1.1)),
      legend.title = element_text(size = rel(1.1)),
      legend.spacing.x = unit(2, "pt"),
      ## Background ##
      strip.background = element_blank(),
      plot.tag = element_text(size = rel(1.3), face = "bold"),
      strip.text = element_text(face = "bold")
    )
}
tableResQuanti<-function(x){
  
  x<-
    data.frame(Variable = row.names(x),`Diff. moyenne` = x[,1], `IC95%` = paste(x[,3], ";",x[,5]),check.names = F,row.names = NULL)
  x[1,3]<-NA
  return(x)
}


tableQuali<- function(BDD){
  
  resQuali<- BDD%>%desctable( stats =  list(n= length, `%` = percent),
                              list(.default = ~no.test))
  resQuali[[2]]<-resQuali[[2]]%>%round(1)%>%mutate(`n(%)`= ifelse(is.na(`%`),n,paste(n," (",`%`,"%)", sep = "" )))%>%select(`n(%)`)
  resQuali[[3]]<-resQuali[[3]]%>%round(1)%>%mutate(`n(%)`= ifelse(is.na(`%`),n,paste(n," (",`%`,"%)", sep = "" )))%>%select(`n(%)`)
  
  
  resQuali$Variables$Variables<-gsub(".*: ","",resQuali$Variables$Variables)
  resQuali$tests<- NULL
  matriceQuali<-matrix(unlist(resQuali),ncol = 3)
  matriceQuali<-matriceQuali[-which(str_detect( matriceQuali[,1],"\\*Non\\*")),]
  colnames(matriceQuali)<- (names(resQuali))
  aIndente<- which(!str_detect(matriceQuali[,1],"\\*\\*"))
  # return(list(matriceQuali=matriceQuali, aIndente = aIndente))
  return(matriceQuali)
}


tableQuanti<- function(BDD){
  resquanti<-BDD%>%desctable(list(n= length, moyenne = mean,sd = sd,fquar =  function(x) quantile(x,probs=0.25, na.rm = T),lquar =  function(x) quantile(x,probs=0.75, na.rm = T), median = median),
                             list(.default = ~no.test))
  
  
  resquanti[[2]]<-resquanti[[2]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  resquanti[[3]]<-resquanti[[3]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  resquanti$tests<- NULL
  
  
  matriceQuanti<-matrix(unlist(resquanti),ncol = 7)
  colnames(matriceQuanti)<- c("Variable","n","moy (sd)","median (1-3 quart)","n","moy (sd)","median (1-3 quart)")
  return(list(matriceQuanti= matriceQuanti, noms=names(resquanti)))
}


tableQuanti1<- function(BDD){
  resquanti<-BDD%>%desctable(list(n= length, moyenne = mean,sd = sd,fquar =  function(x) quantile(x,probs=0.25, na.rm = T),lquar =  function(x) quantile(x,probs=0.75, na.rm = T), median = median),
                             list(.default = ~no.test))
  
  
  resquanti[[2]]<-resquanti[[2]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  resquanti$tests<- NULL
  
  
  matriceQuanti<-matrix(unlist(resquanti),ncol = 4)
  colnames(matriceQuanti)<- c("Variable","n","moy (sd)","median (1-3 quart)")
  return(matriceQuanti)
}



tableQuanti2<- function(BDD){
  resquanti<-BDD%>%desctable(list(n= length, moyenne = mean,sd = sd,fquar =  function(x) quantile(x,probs=0.25, na.rm = T),lquar =  function(x) quantile(x,probs=0.75, na.rm = T), median = median),
                             
  )
  
  
  resquanti[[2]]<-resquanti[[2]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  resquanti[[3]]<-resquanti[[3]]%>%round(1)%>%
    mutate(`moy (sd)`= paste(moyenne," (",`sd`,")", sep = "" ),
           `median (1-3 quart)`=paste(median," (",`fquar`,"-",lquar,")", sep = "" ) )%>%select(n,`moy (sd)`,`median (1-3 quart)`)
  # resquanti$tests<- NULL
  # resquanti[[4]]<-resquanti[[4]]%>%mutate_if(is.numeric,function(x) round(x,3))
  
  matriceQuanti<-matrix(unlist(resquanti),ncol = 9)
  colnames(matriceQuanti)<- c("Variable","n","moy (sd)","median (1-3 quart)","n","moy (sd)","median (1-3 quart)","p","test")
  return(list(matriceQuanti= matriceQuanti, noms=names(resquanti)))
}

tableQuali1<- function(BDD){
  
  resQuali<- BDD%>%desctable( stats =  list(n= length, `%` = percent),
                              list(.default = ~no.test))
  resQuali[[2]]<-resQuali[[2]]%>%round(1)%>%mutate(`n(%)`= ifelse(is.na(`%`),n,paste(n," (",`%`,"%)", sep = "" )))%>%select(`n(%)`)
  
  resQuali$Variables$Variables<-gsub(".*: ","",resQuali$Variables$Variables)
  resQuali$tests<- NULL
  matriceQuali<-matrix(unlist(resQuali),ncol = 2)
  modliteNon<- which(str_detect(matriceQuali[,1],"\\*Non\\*"))
  if (length(modliteNon)>1){
    matriceQuali<-matriceQuali[-modliteNon,]
  }
  colnames(matriceQuali)<- (names(resQuali))
  aIndente<- which(!str_detect(matriceQuali[,1],"\\*\\*"))
  # return(list(matriceQuali=matriceQuali, aIndente = aIndente))
  return(matriceQuali)
}
tableQuali<- function(BDD){
  
  resQuali<- BDD%>%desctable( stats =  list(n= length, `%` = percent),
  )
  resQuali[[2]]<-resQuali[[2]]%>%round(1)%>%mutate(`n(%)`= ifelse(is.na(`%`),n,paste(n," (",`%`,"%)", sep = "" )))%>%select(`n(%)`)
  resQuali[[3]]<-resQuali[[3]]%>%round(1)%>%mutate(`n(%)`= ifelse(is.na(`%`),n,paste(n," (",`%`,"%)", sep = "" )))%>%select(`n(%)`)
  # resQuali[[4]]<-resQuali[[4]]%>%mutate_if(is.numeric,function(x) round(x,3))
  
  resQuali$Variables$Variables<-gsub(".*: ","",resQuali$Variables$Variables)
  resQuali$tests
  matriceQuali<-matrix(unlist(resQuali),ncol = 5)
  modliteNon<- which(str_detect(matriceQuali[,1],"\\*Non\\*"))
  if (length(modliteNon)>1){
    matriceQuali<-matriceQuali[-modliteNon,]
  }
  colnames(matriceQuali)<- c(names(resQuali)[1:3],"p","test")
  aIndente<- which(!str_detect(matriceQuali[,1],"\\*\\*"))
  # return(list(matriceQuali=matriceQuali, aIndente = aIndente))
  return(matriceQuali)
}


library(desctable)




