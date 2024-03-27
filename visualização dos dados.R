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
library(kableExtra)

setwd("C:\\Users\\Yago\\Documents\\IC")

painel_final <- read_excel("dados cavalcanti.xlsx")


data <- panel_data(painel_final, id = Estado, wave = Ano)

View(data)
glimpse(data)


#gráficos das ptfs

tiff("test.tiff", units="in", width=12, height=8, res=300)
describe_panel_by_year  <- summary(data, ptf, by.wave = FALSE, by.id = TRUE)
describe_panel_by_year %>% 
  kable() %>%
  kable_styling()

data %>% 
  line_plot(ptf)

data %>% 
  line_plot(ptf, 
            overlay = FALSE,
            add.mean = TRUE)
dev.off()








describe_panel_by_year  <- summary(data, minmax, by.wave = FALSE, by.id = TRUE)
describe_panel_by_year %>% 
  kable() %>%
  kable_styling()

data %>% 
  line_plot(minmax)

data %>% 
  line_plot(minmax, 
            overlay = FALSE,
            add.mean = TRUE)




