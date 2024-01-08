
library(dplyr)
library(tidyr)
infert %>%
  summarise_at(vars(age, parity), list(
    Moyenne = ~ mean(.),
    Mediane = ~ median(.),
    min = ~min(.),
    max = ~max(.),
    sd = ~sd (.),
    var = ~var(.),
    etendu = ~max(.)-min(.)
  )) %>% pivot_longer(everything(),names_sep = "_", names_to = c("Var","type"))%>%
  pivot_wider(names_from = type,values_from = value)
  



  infert %>%
  summarise_at(vars(education, induced,case), list(
    n = ~ list(table(.)),
    percent = ~ list(table(.)/n())))%>% pivot_longer(everything(),names_sep = "_", names_to = c("Var","type"))%>%
  unnest(value)%>%mutate(modalite = names(value), value = value)%>%
  pivot_wider(names_from = type,values_from = value)


  
  
  
  infert %>%group_by(case)%>% 
    summarise_at(vars(age, parity), list(
    Moyenne = ~ mean(.),
    Mediane = ~ median(.),
    min = ~min(.),
    max = ~max(.),
    sd = ~sd (.),
    var = ~var(.),
    etendu = ~max(.)-min(.)
  )) %>%  pivot_longer(c(everything(),-case),names_sep = "_", names_to = c("Var","type"))%>%
    pivot_wider(names_from = type,values_from = value)%>%
    pivot_wider(names_from = case,values_from = c(everything(),-Var, -case), names_glue = "{case}_{.value}")
  
  

  
  
