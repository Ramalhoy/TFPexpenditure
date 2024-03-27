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
painel_final <- read_excel("dados em painel.xlsx")
dados <- pdata.frame(painel_final, index = c("Estado", "Ano"), drop.index=TRUE, row.names=TRUE )


View(painel_final)
pdim(dados)
is.pbalanced(dados) #painel balanceado
summary(dados)





#verificando a distribuição
par(mfrow=c(1,2))
hist(dados$lnA,          breaks=10, xlab="Data",            col="lightblue", main="")
hist(dados$Índice, breaks=10, xlab="Normalized Data", col="lightblue", main="")




###sobre coeftest: The fixed effects case, as already observed in Section 6.4 on serial correlation, is complicated
## by the fact that the demeaning induces serial correlation in the errors. The original White
## estimator (white1) turns out to be inconsistent for fixed T as n grows, so in this case it is
## advisable to use the arellano version


#modelagem 

####despesas

#despesa judiciária 
jud <- plm(log(ptf) ~ log(despjud) + log(pop) + educ + log(K) + log(GW),
            data=dados,
            effect = "twoways",
            model = "within")
summary(jud)
summary(fixef(jud, effect = "time"))

pbgtest(jud, order = 2)

coeftest(jud, vcovHC) #erros robustos
coeftest(jud, vcov=vcovHC, method = 'white1' ,type="HC0") #erros robustos 


coeftest(jud, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos 


jud3 <- plm(log(ptf) ~ log(despjud) + log(pop) + educ + log(K),
           data=dados,
                     model = "pooling")

pFtest(jud, jud3)  ##efeito fixo é preferível

jud4 <- plm(log(ptf) ~ log(despjud) + log(pop) + educ + log(K),
            data=dados,
            model = "random")
phtest(jud, jud4) ##efeito fixo é preferível


jud2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despjud) + log(pop) + educ + log(K),
           data=dados,
           effect = "twoways",
           model = "within")
summary(jud2)

coeftest(jud2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") 

#despesa administrativa
adm <- plm(log(ptf) ~ log(despadm) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(adm)
summary(fixef(adm))


coeftest(adm, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(adm, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos








adm2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despadm) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(adm2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #

#despeca com educação
educ <- plm(log(ptf) ~ log(despeduc) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(educ)
summary(fixef(educ))


coeftest(educ, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(educ, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos


educ2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despeduc) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(educ2)
coeftest(educ2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos

#despesa legislativa
leg <- plm(log(ptf) ~ log(despleg) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(leg)

coeftest(leg, vcovHC)

leg2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despleg) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
coeftest(leg2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos

#indústria, comércio e serviços
ics <- plm(log(ptf) ~ log(despics) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(ics)

coeftest(ics, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(ics, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos



ics2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despics) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
coeftest(ics2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos

#tec
tec <- plm(log(ptf) ~ log(desptec) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(tec)

coeftest(tec, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(tec, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos



tec2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(desptec) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
coeftest(tec, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos

#assistencia
assist <- plm(log(ptf) ~ log(despass) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(assist)

coeftest(assist, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(assist, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos


assist2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despass) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(assist2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos

#saude
saude <- plm(log(ptf) ~ log(despsaude) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")
summary(saude)
summary(fixef(saude, effect = "time"))

coeftest(saude, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
coeftest(saude, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "time") #erros robustos



saude2 <- plm(log(ptf) ~ lag(log(ptf), 1) + log(despsaude) + log(pop) + educ + log(K),
            data=dados,
            effect = "twoways",
            model = "within")

coeftest(saude2, vcov=vcovHC, method = 'arellano' ,type="HC0", cluster = "group") #erros robustos
