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
painel_final <- read_excel("dados cavalcanti.xlsx")
dados <- pdata.frame(painel_final, index = c("Estado", "Ano"), drop.index=TRUE, row.names=TRUE )


View(dados)
pdim(dados)
is.pbalanced(dados) #painel balanceado
descritiva <- summary(dados)

write.table(descritiva, file='arquivo.csv', sep=';', dec=',', row.names=FALSE)


#verificando a distribuição
par(mfrow=c(1,2))
hist(dados$lnA,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(dados$Índice, breaks=10, xlab="Normalized Data", col="lightblue", main="")


elasticidades <- plm(log(PIB) ~ log(Kl) + log(L), data = dados, model = 'within')
summary(elasticidades)


###sobre coeftest: The fixed effects case, as already observed in Section 6.4 on serial correlation, is complicated
## by the fact that the demeaning induces serial correlation in the errors. The original White
## estimator (white1) turns out to be inconsistent for fixed T as n grows, so in this case it is
## advisable to use the arellano version


#modelagem 

####despesas

#despesa judiciária 

jud <- plm(log(Índice) ~ log(despjud)  + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
           data=dados,
           effect = "twoways",
           model = "within")
summary(jud)





coeftest(jud, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") ####erros robustos 





#despesa administrativa
adm <- plm(log(Índice) ~ log(despadm) + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
           data=dados,
           effect = "twoways",
           model = "within")
summary(adm)




coeftest(adm, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos





#despeca com educação
educ <- plm(log(Índice) ~ log(despeduc)  + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
            data=dados,
            effect = "twoways",
            model = "within")
summary(educ)



coeftest(educ, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos




#despesa legislativa
leg <- plm(log(Índice) ~ log(despleg)  + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
           data=dados,
           effect = "twoways",
           model = "within")
summary(leg)


coeftest(leg, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group")




#indústria, comércio e serviços
ics <- plm(log(Índice) ~  log(despics)  + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
           data=dados,
           effect = "twoways",
           model = "within")
summary(ics)



coeftest(ics, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos



#tec
tec <- plm(log(Índice) ~  log(desptec)   + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
           data=dados,
           effect = "twoways",
           model = "within")
summary(tec)


coeftest(tec, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos



#assistencia
assist <- plm(log(Índice) ~  log(despass)  + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
              data=dados,
              effect = "twoways",
              model = "within")
summary(assist)



coeftest(assist, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos





#saude
saude <- plm((ptf) ~  log(despsaude) + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),
             data=dados,
             effect = "twoways",
             model = "within")
summary(saude)




coeftest(saude, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos














