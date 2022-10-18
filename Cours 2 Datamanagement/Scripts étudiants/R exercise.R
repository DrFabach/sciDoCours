#Installing Readxl package first so we can import an .XLSX format file into R Studio.
install.packages("tidyverse")
library("tidyverse")
install.packages("janitor")
library("janitor")
library("stringr")
library(readxl)
install.packages("readxl")
#Checking directory of our imports, since it's in the default cloud project, we can import it easily with renaming it.
getwd()
Sheet1 <- read_excel("data1.xlsx", 1,na =c(" ","","N/A","NA"))
Sheet2 <- read_excel("data1.xlsx", 2,na =c(" ","","N/A","NA"))

#can't merge tables, error column names.
#https://www.marsja.se/how-to-rename-column-or-columns-in-r-with-dplyr/
#Renaming each column on its own takes alot of time, we decided to use Janitor function.
#https://www.rdocumentation.org/packages/janitor/versions/2.1.0
names(Sheet1)
Sheet1<- clean_names(Sheet1) 
Sheet2<- clean_names(Sheet2)
Sheet1 %>% get_dupes #Checks for duplicated data, we need to specify column.

#Deleting empty columns
emptycols <- sapply(Sheet1, function (k) all(is.na(k)))
Sheet1 <- Sheet1[!emptycols]

#How to find unique values in each column, to replace them by correct values later
unique(Sheet1$sexe_du_medcin_traitant)
unique(Sheet1$sexe_du_medecin_adresseur)
unique(Sheet1$delais_de_prise_en_charge_mois)
unique(Sheet1$medecin_adresseur)

Sheet1$sexe_du_medcin_traitant[Sheet1$sexe_du_medcin_traitant=="f"]<-"Féminin"
Sheet1$sexe_du_medcin_traitant[Sheet1$sexe_du_medcin_traitant=="m"]<-"Masculin"
Sheet1$sexe_du_medcin_traitant[Sheet1$sexe_du_medcin_traitant=="45"]<-""
Sheet1 <- Sheet1 %>% mutate_all(na_if,"")

#Another way to replace values
#https://www.tutorialspoint.com/how-to-remove-a-character-in-an-r-data-frame-column
Sheet1$delais_de_prise_en_charge_mois<-gsub("4. mois","4",as.character(Sheet1$delais_de_prise_en_charge_mois))
Sheet1$medecin_adresseur<-gsub("generilste","Généraliste",as.character(Sheet1$medecin_adresseur))




#merging sheet1 and sheet2
data_final<-dplyr::inner_join(Sheet1, Sheet2, by="id")

write.csv2(data_final,"/cloud/project/data_final.csv")
