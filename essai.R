```{r}
data("mtcars")

ggplot(data = mtcars, 
       aes(x = as.factor(cyl),
           y = hp)) + 
  geom_jitter(width = .2)
```


## utiliser l'encodage multiple sur le nombre de cylindres

```{r}
ggplot(data = mtcars, 
       aes(x = as.factor(cyl),
           y = hp, 
           size = cyl,
           color = cyl)) + 
  geom_jitter(width = .2, alpha = .6) +
  theme_minimal() +
  theme(legend.position = "none")
```

## ajouter l'information du nombre de carburateurs

```{r}

ggplot(data = mtcars, 
       aes(x = as.factor(cyl),
           y = carb, 
           size = hp,
           color = hp)) + 
  geom_jitter(width = .2, alpha = .6) +
  theme_minimal() 
#facet_grid(~as.factor(carb))
#theme(legend.position = "none")
```

## Paufiner le plot (axes, titres, thème)

```{r}

ggplot(data = mtcars, 
       aes(x = as.factor(cyl),
           y = carb, 
           size = hp,
           color = hp)) + 
  geom_jitter(width = .2, alpha = .6) +
  theme_minimal() + 
  labs(x = "Cylinders", 
       y = "Carburators")
```

## représenter la distribution du nombre de miles per gallon en histogramme

```{r}
ggplot(mtcars, 
       aes(x= mpg)) + 
  geom_histogram(bins = sqrt(nrow(mtcars)))
```

## représenter la distribution du nombre de miles per gallon en boxplot

```{r}
ggplot(mtcars, 
       aes(x= 1, y= mpg)) + 
  geom_boxplot()
```

## representer la distribution du nombre de miles per gallon en fonction du nombre de cylindres

```{r}
ggplot(mtcars, 
       aes(x= as.factor(cyl), y= mpg)) + 
  geom_violin(fill = "grey70")
```

## ajouter les points par dessus la distribution

```{r}
ggplot(mtcars, 
       aes(x= as.factor(cyl), y= mpg)) + 
  geom_violin(fill = "grey70") + 
  geom_jitter(aes(color = cyl),width = .15) 
```


## paufiner le plot (axes, titres, thème)

