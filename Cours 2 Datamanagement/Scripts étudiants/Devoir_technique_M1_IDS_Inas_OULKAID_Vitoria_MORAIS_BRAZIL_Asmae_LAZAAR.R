#IMPORTATION ET OUVERTURE DU FICHIER XLSX:

library("readxl") #on fait appel au dictionnaire readxl afin d'avoir accès à ses fonctions.
read_excel("/home/inas_olk/data1.xlsx") #permet de lire les fichiers xlsx.
data1 <-  read_excel("/home/inas_olk/data1.xlsx",1) #on assigne le premier feuillet du fichier à "data1"
data2 <- read_excel("/home/inas_olk/data1.xlsx",2) #on assigne le premier feuillet du fichier à "data2"

#MODIFICATION DU FICHIER POUR FAIRE PLUS PROPRE:

data1 <- data1[,-2] #supprime la deuxième colonne de le première feuille car l'information n'est pas interessante.
data1$`Délais de prise en charge (mois)` #permet de visioner la variable "Delais de prise en charge" dans la colonne
data1$`Délais de prise en charge (mois)` <- gsub(" +mois","",data1$`Délais de prise en charge (mois)`) #Nous remarquons dans la varibale une ligne contenant le mot "mois" or il nous faut des valeurs sans unités. Donc nous remplaçons "mois" par un espace vide.
data1$`Délais de prise en charge (mois)`
mode(data1$`Date de lancement`) #la fonction "mode()" permet de connaître le type de la variable. La fonction affiche ici qu'il s'agit de character alors qu'il s'agit de dates.
data1$`Date de lancement` <- as.Date(data1$`Date de lancement`) #On convertie en date avec as.Date()
str(data1$`Date de lancement`) #On verifie si la conversion c'est bien faite. str() permet aussi de verifier le type d'une variable.
data1$`Sexe du medcin traitant` <- gsub("f","Féminin",data1$`Sexe du medcin traitant`) #remplace tout les "f " par le mot "Féminin"dans la variable "Sexe du medcin traitant".
data1$`Sexe du medcin traitant` #on vérifie que la variable à été changé.
data1$`Sexe du medcin traitant`
ifelse(data1$`Sexe du medcin traitant`=='m','Masculin',data1$`Sexe du medcin traitant`) #nous pouvons egalement utiliser cette synthaxe pour plus de simplicité. Fais la meme chose que dans la ligne 12.

#FUSION DES DEUX FEUILLET ET EXPORTATION EN FORMAT XLSX:

data1bis <- data.frame(data1) # nous créons des dataframes pour nos deux feuillets afin de pouvoir les fusuionner.
data2bis <- data.frame(data2)
basefinale <- merge(data1bis,data2bis,by.x ='Sexe.du.medcin.traitant', by.y='Sexe') # 'basefinale" est le support final où les deux feuillet serons fusionné.
basefinale #affiche basefinale
library("writexl")
install.packages("writexl") #installation du pack writexl
library("writexl")#on fait appel au dictionnaire writexl afin d'avoir accès à ses fonctions.
write_xlsx(basefinale,"/home/inas_olk/base_finale.xlsx") #importation de "basefinale" en format xlsx (excel)

