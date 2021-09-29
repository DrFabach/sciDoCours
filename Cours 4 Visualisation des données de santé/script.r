## Visualisation des données
install.packages("ggplot2")
library(ggplot2)
mtcars


## Principe de base ggplot2
# ggplot(mtcars, aes(x=x,y=y, color=color...))+geom_bar()+
# theme_classic()+labs(x="x", y="y")+
# xlim(c(0,10))


# Nuage de points
ggplot(mtcars, aes(x= mpg, y = drat))+geom_point()
ggplot(mtcars)+geom_point(aes(x= mpg, y = drat),shape=14)
ggplot(mtcars)+geom_point(aes(x= mpg, y = drat,
shape = factor(cyl)))



# histogramme
ggplot(mtcars, aes(x= mpg))+
geom_histogram(bins=sqrt(nrow(mtcars)))

# diagramme en barre
ggplot(mtcars, aes(y = mpg))+
geom_boxplot()


# diagramme en barre
ggplot(mtcars, aes(y = mpg, x=1))+
geom_violin()


# Diagramme en barre
ggplot(mtcars, aes(x = factor(cyl)))+
geom_bar()+xlab("cyl")

# Quanti Quali
ggplot(mtcars, aes(x=factor(cyl),y=mpg ))+
geom_boxplot()+xlab("cyl")

ggplot(mtcars, aes(x=factor(cyl),y=mpg ))+
geom_violin()+xlab("cyl")


ggplot(mtcars, aes(x=factor(cyl),y=mpg ))+
geom_point()+xlab("cyl")



ggplot(mtcars, aes(x=factor(cyl),y=mpg ))+
geom_point()+geom_boxplot(fill =rgb(0,0,0,0))+geom_violin(fill =rgb(0,0,0,0))+xlab("cyl")


ggplot(mtcars, aes(x=factor(cyl),y=mpg ))+
geom_jitter()+geom_boxplot(fill =rgb(0,0,0,0))+geom_violin(fill =rgb(0,0,0,0))+xlab("cyl")

## Quanti Quanti
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))+
geom_bar(position = "dodge")

## Encodage redondant

ggplot(mtcars,aes(x= mpg, y= drat, size = mpg, color = mpg))+
geom_point()


## Violin plot Consommation Cylindre
p1<- ggplot(mtcars,aes(x = factor(cyl), y =mpg, fill=factor(cyl)))+geom_violin()
p1<- p1+labs(x = "cylindre", y = "m/g")
p1+theme_minimal()+labs(fill = "cyl")+
scale_fill_manual(values = c("#67eff5","#87e985","#d05959"))



#Exemples de graphique