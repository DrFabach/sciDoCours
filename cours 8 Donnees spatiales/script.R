# Import de la library sf
library(sf)

# Import du shape files
carte<- st_read("./cours 8 Donnees spatiales/departements-20140306-5m-shp/departements-20140306-5m.shp")

# Structure des données
str(carte)
# Affichage du polygone1
plot(st_geometry(carte[1,]))
carte[1,]

#Affichage du bas-rhin
plot(st_geometry(carte[carte$code_insee==67,]))

#Afficahe de la france
plot(st_geometry(carte))

str(carte$code_insee)

#conversion du code insee en numériqque
carte$code_insee <- as.numeric(carte$code_insee)

#Selection france metropolitaine
carte_metrop<- carte[which(carte$code_insee<100),]
plot(st_geometry(carte_metrop))

# affichage france metropolitaine
plot(carte_metrop)

#import donnnées population
pop<- read.csv2("./cours 8 Donnees spatiales/Pop.csv")
pop$maille_code<- as.numeric(pop$maille_code)

# structure de pop
str(pop)

#Import de la library dplyr
library(dplyr)
#Jointure shapefile, données pop.
carte_pop<- left_join(carte_metrop, pop, by=c("code_insee"="maille_code"))

#structure de l'objet carte_pop
str(carte_pop)

#Affichage par défaut de la carte avec la pop
plot(cart_pop["Pop"])

# Decoupage de la population en 5 classes équirépartie
carte_pop$pop_5 = cut(carte_pop$Pop, 5)

#Affichage par défaut de la carte avec la pop en 5 classes

plot(carte_pop["pop_5"], axes = TRUE, pal = sf.colors(10))

#import de la librarie cartographie
library(cartography)

#remise à 0 de la fenetre des graphiques pour avoir un graphique à la fois
par(mfrow=c(1,1))

# carte cloropleth basé ue le fichier cart_pop, montrant la variable "Pop"
choroLayer(x = carte_pop,var = "Pop")

# Affichage des contours des départements
plot(st_geometry(carte_pop), axes = TRUE)
#Ajout de cercles de taille proportionelle à la population
# argument  :
# -legend.pos : position de la legend
# - inches : tailles des cercles
propSymbolsLayer(x = carte_pop,var = "Pop", 
                 legend.pos = "bottomleft", inches = 0.1)


## Ajout d'un point sur la ville de strabourg
#Crétion d'un dataframe avec le nom et les coordonnées de la ville
strasbourg<- data.frame(nom= "Strasbourg",
                        latitude =48.5734053 ,
                        longitude = 7.7521113) 

# Conversion de la df en objet spatial
# crs = spécifie la projection, ici en wgs84
place_sf <- st_as_sf(strasbourg,coords = c("longitude",
                                           "latitude"),
                     crs=4326)

# Ajout de strasbourg sur la carte
plot(st_geometry(place_sf),pch=20,cex=4,col="green",add=T)


## Représentation covid

# import des données sur la covid
hospit_covid <- read.csv2("./cours 8 Donnees spatiales/donnees-hospitalieres-covid19-2020-11-09-19h00.csv")
# structure des données covid
str(hospit_covid)

#conversion du jour en date
hospit_covid$jour <- as.Date(hospit_covid$jour, "%Y-%m-%d")
#Recherche du dernier jour des données
max(hospit_covid$jour)

#Selection des dernières données
hospit_covid_hier<-hospit_covid[hospit_covid$jour=="2020-11-09",]

#Selection de l'ensemble des cas, sexe = 0
hospit_covid_hier<- hospit_covid_hier[hospit_covid_hier$sexe==0,]

#Conversion en numéric du code insee
hospit_covid_hier$dep <- as.numeric(hospit_covid_hier$dep)
#jointure avec les données spatiales
hospit_covid_g <- left_join(carte_pop,hospit_covid_hier,
                            by=c("code_insee"="dep"))

#structure des données
str(hospit_covid_g)                            
                            
# calcul du "taux d'incidence" d'hospitalisation
hospit_covid_g$incidence<- hospit_covid_g$hosp/hospit_covid_g$Pop*100000

#Création d'une carte chloropleth avec le taux "taux d'incidence" d'hospitalisation
choroLayer(x=hospit_covid_g,var="incidence",
           legend.pos = 'topleft')
# Rajout de cercle de taille et de couleurs dépendant du nombre de patient en réanimation
propSymbolsChoroLayer(x = hospit_covid_g,var = "rea", 
                      legend.var.pos = "topright",
                      inches = 0.2,
                      var2 = "rea",col = sf.colors(10), 
                      legend.var2.pos = "bottomright")

#exemple avec ggplot
library(ggplot2)
ggplot(hospit_covid_g)+geom_sf()+geom_sf(aes(fill=incidence))


# exemple avec leaflet
library(leaflet)
library(viridis)


pal2 <- colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), 
                     domain = c(0,(
                       (max(hospit_covid_g$incidence)))))

labels <- sprintf(
  "<strong>%s</strong><br/>%g hospit pour 100 000 habitants",
  hospit_covid_g$code_insee, hospit_covid_g$incidence
) %>% lapply(htmltools::HTML)

leaflet(data = hospit_covid_g) %>%
  addTiles()%>%
  setView(0, 30, zoom = 3) %>%
  
  addPolygons(fillColor = ~pal2(incidence),
              layerId = ~code_insee,
              fillOpacity = 1,
              color = "#BDBDC3",
              weight = 1,
              
              opacity = 1,
              
              dashArray = "3",
              
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              
              
              
  )%>% 
  addLegend(pal = pal2, values = ~incidence, opacity = 0.7, title = NULL,
            position = "bottomright")


