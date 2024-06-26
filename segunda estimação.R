rm(list=ls()) 

library(plm)
library(dplyr)
library(prodest)
library(estprod)
library(broom)
library(readxl)
library(lmtest)
library(stargazer)
library(foreign)
library(scales)
library(ggplot2)
library(panelr)
library(lmtest)
library(sandwich)

#preparando os dados
setwd("C:\\Users\\Yago\\Documents\\IC")
painel_final <- read_excel("dados cavalcanti2xlsx.xlsx")
dados <- pdata.frame(painel_final, index = c("Estado", "Ano"), drop.index=TRUE, row.names=TRUE )


View(dados)
pdim(dados)
is.pbalanced(dados) #painel balanceado
descritiva <- summary(dados)

write.table(descritiva, file='arquivo.csv', sep=';', dec=',', row.names=FALSE)


#verificando a distribui��o
par(mfrow=c(1,2))
hist(dados$lnA,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(dados$�ndice, breaks=10, xlab="Normalized Data", col="lightblue", main="")


elasticidades <- plm(log(PIB) ~ log(Kl) + log(L), data = dados, model = 'within')
summary(elasticidades)


###sobre coeftest: The fixed effects case, as already observed in Section 6.4 on serial correlation, is complicated
## by the fact that the demeaning induces serial correlation in the errors. The original White
## estimator (white1) turns out to be inconsistent for fixed T as n grows, so in this case it is
## advisable to use the arellano version


#modelagem 

####despesas

#despesa judici�ria 

jud <- plm(log(�ndice) ~ log(despjud) + educ + log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
           data=dados,
           effect = "twoways",
           model = "within")
summary(jud)


corr<- round(cor(dados),
      digits = 2 # rounded to 2 decimals    #tira pop
)


### propor��o

judp <- plm(log(�ndice) ~ log(despjud/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
            data=dados,
            effect = "twoways",
            model = "within")
summary(judp)  

##testando diferentes especifica��es para erro robusto

coeftest(jud, vcovHC) #erros robustos



coeftest(jud, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") ####erros robustos 
coeftest(judp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") ####erros robustos 





##devo usar lag (ptf, 1) ou lag (minmax, 1)?
jud2 <- plm(log(�ndice) ~ lag((minmax), 1) + log(despjud) + log(pop) + log(H) + log(KL) + log(L) + log(IMPORT),
            data=dados,
            effect = "twoways",
            model = "within")
summary(jud2)

coeftest(jud2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") 




#despesa administrativa
adm <- plm(log(�ndice) ~ log(despadm) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
           data=dados,
           effect = "twoways",
           model = "within")
summary(adm)



##propor��o 

admp <- plm(log(�ndice) ~ log(despadm/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
            data=dados,
            effect = "twoways",
            model = "within")
summary(admp)


coeftest(adm, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(admp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos








adm2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despadm) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(adm2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #



#despeca com educa��o
educ <- plm(log(�ndice) ~ log(despeduc) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
            data=dados,
            effect = "twoways",
            model = "within")
summary(educ)


educp <- plm(log(�ndice) ~  log(despeduc/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
             data=dados,
             effect = "twoways",
             model = "within")
summary(educp)


coeftest(educ, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(educp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos



educ2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despeduc) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
             data=dados,
             effect = "twoways",
             model = "within")
summary(educ2)
coeftest(educ2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos

#despesa legislativa
leg <- plm(log(�ndice) ~ log(despleg) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
           data=dados,
           effect = "twoways",
           model = "within")
summary(leg)

legp <- plm(log(�ndice) ~  log(despleg/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(leg, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group")
coeftest(legp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group")



leg2 <- plm((minmax) ~ lag(log(ptf), 1) + log(despleg) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
            data=dados,
            effect = "twoways",
            model = "within")
coeftest(leg2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos




#ind�stria, com�rcio e servi�os
ics <- plm(log(�ndice) ~  log(despics) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
           data=dados,
           effect = "twoways",
           model = "within")
summary(ics)

icsp <- plm(log(�ndice) ~  log(despics/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(ics, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(icsp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos




ics2 <- plm((minmax) ~ lag(log(ptf), 1) + log(despics) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
            data=dados,
            effect = "twoways",
            model = "within")
coeftest(ics2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos


#tec
tec <- plm(log(�ndice) ~  log(desptec) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
           data=dados,
           effect = "twoways",
           model = "within")
summary(tec)

tecp <- plm(log(�ndice) ~  log(desptec/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(tec, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(tecp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos




tec2 <- plm((minmax) ~ lag(log(ptf), 1) + log(desptec) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
            data=dados,
            effect = "twoways",
            model = "within")


#assistencia
assist <- plm(log(�ndice) ~  log(despass) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
              data=dados,
              effect = "twoways",
              model = "within")
summary(assist)

assistp <- plm(log(�ndice) ~  log(despass/pop) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
               data=dados,
               effect = "twoways",
               model = "within")

coeftest(assist, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(assistp, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos


assist2 <- plm((minmax) ~ lag(log(ptf), 1) + log(despass) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
               data=dados,
               effect = "twoways",
               model = "within")

coeftest(assist2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos


#saude
saude <- plm(log(�ndice) ~  log(despjud) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
             data=dados,
             effect = "twoways",
             model = "within")
summary(saude)

saudep <- plm(log(�ndice) ~  log(despjud) + educ + +log (L) + log(K) + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens),
              data=dados,
              effect = "twoways",
              model = "within")

coeftest(saude, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(saudep, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos



saude2 <- plm((minmax) ~ lag(log(ptf), 1) + log(despsaude) + log(pop) + educ25 + log(KL) + log(L) + log(IMPORT),
              data=dados,
              effect = "twoways",
              model = "within")

coeftest(saude2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos










