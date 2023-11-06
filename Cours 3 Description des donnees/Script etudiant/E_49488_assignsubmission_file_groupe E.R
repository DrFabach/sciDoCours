head(infert)
str(infert)
dim(infert)

###description des variables
#1 education = variable qualitative ordinale
#2 age = variable quantitative continue
#3 parity = variable quantitative discrète
#4 induced = variable quantitative discrète
#5 case = variable qualitative nominale binaire
#6 spontaneous = variable quantitative discrète
#7 stratum = variable quantitative discrète
#8 pooled.stratum = variable quantitative discrète

###création de 2 variables
#variable groupe témoin
case_0<-subset(infert,case==0)
table(case_0$education)
prop.table(table(case_0$education))*100
#variable groupe expérimental
case_1<-subset(infert,case==1)
table(case_1$education)
prop.table(table(case_1$education))*100

dim(case_0)
###1 education
#fréquence relative
table(infert$education)
prop.table(table(infert$education))*100

###2 age
##groupé témoin
#paramètre de position
mean(case_0$age)
median((case_0$age))
#paramètre de dispersion
min(case_0$age)
max(case_0$age)
var(case_0$age)
sd(case_0$age)
#coefficient de variation
(sd(case_0$age)/mean(case_0$age))*100
##groupe expérimental
#paramètre de position
mean(case_1$age)
median((case_1$age))
#paramètre de dispersion
min(case_1$age)
max(case_1$age)
var(case_1$age)
sd(case_1$age)
#coefficient de variation
(sd(case_1$age)/mean(case_1$age))*100

###3 parity
#groupe témoin
#paramètre de position
mean(case_0$parity)
median((case_0$parity))
#paramètre de dispersion
min(case_0$parity)
max(case_0$parity)
var(case_0$parity)
sd(case_0$parity)
#coefficient de variation
(sd(case_0$parity)/mean(case_0$parity))*100
##groupe expérimental
#paramètre de position
mean(case_1$parity)
median((case_1$parity))
#paramètre de dispersion
min(case_1$parity)
max(case_1$parity)
var(case_1$parity)
sd(infert$parity)
#coefficient de variation
(sd(case_1$parity)/mean(case_1$parity))*100

###4 induced
##groupe témoin
#paramètre de position
mean(case_0$induced)
median((case_0$induced))
#paramètre de dispersion
min(case_0$induced)
max(case_0$induced)
var(case_0$induced)
sd(case_0$induced)
#coefficient de variation
(sd(case_0$induced)/mean(case_0$induced))*100
##groupe expérimental
mean(case_1$induced)
median((case_1$induced))
#paramètre de dispersion
min(case_1$induced)
max(case_1$induced)
var(case_1$induced)
sd(case_1$induced)
#coefficient de variation
(sd(case_1$induced)/mean(case_1$induced))*100

###5 case
#fréquence relative
table(infert$case)
prop.table(table(infert$case))*100

###6 spontaneous
##groupe témoin
#paramètre de position
mean(case_0$spontaneous)
median((case_0$spontaneous))
#paramètre de dispersion
min(case_0$spontaneous)
max(case_0$spontaneous)
var(case_0$spontaneous)
sd(case_0$spontaneous)
#coefficient de variation
(sd(case_0$spontaneous)/mean(case_0$spontaneous))*100
##groupe expérimental
mean(case_1$spontaneous)
median((case_1$spontaneous))
#paramètre de dispersion
min(case_1$spontaneous)
max(case_1$spontaneous)
var(case_1$spontaneous)
sd(case_1$spontaneous)
#coefficient de variation
(sd(case_1$spontaneous)/mean(case_1$spontaneous))*100

###7 stratum
#paramètre de position
#groupe témoin
mean(case_0$stratum)
median((case_0$stratum))
#paramètre de dispersion
min(case_0$stratum)
max(case_0$stratum)
var(case_0$stratum)
sd(case_0$stratum)
#coefficient de variation
(sd(case_0$stratum)/mean(case_0$stratum))*100
##groupe expérimental
mean(case_1$stratum)
median((case_1$stratum))
#paramètre de dispersion
min(case_1$stratum)
max(case_1$stratum)
var(case_1$stratum)
sd(case_1$stratum)
#coefficient de variation
(sd(case_1$stratum)/mean(case_1$stratum))*100

###8 pooled.stratum
##groupe témoin
#paramètre de position
mean(case_0$pooled.stratum)
median((case_0$pooled.stratum))
#paramètre de dispersion
min(case_0$pooled.stratum)
max(case_0$pooled.stratum)
var(case_0$pooled.stratum)
sd(case_0$pooled.stratum)
#coefficient de variation
(sd(case_0$pooled.stratum)/mean(case_0$pooled.stratum))*100
##groupe expérimentale
#paramètre de position
mean(case_1$pooled.stratum)
median((case_1$pooled.stratum))
#paramètre de dispersion
min(case_1$pooled.stratum)
max(case_1$pooled.stratum)
var(case_1$pooled.stratum)
sd(case_1$pooled.stratum)
#coefficient de variation
(sd(case_1$pooled.stratum)/mean(case_1$pooled.stratum))*100


###recherche de corrélation
cor(infert$case,infert$induced)
cor(infert$case,infert$spontaneous)
cor(infert$case,infert$stratum)
cor(infert$case,infert$pooled.stratum)
cor(infert$age,infert$parity)
cor(infert$case,infert$age)

