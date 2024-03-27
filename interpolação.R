library(dplyr)
library(writexl)
library(zoo)


library(imputeTS)

setwd("C:\\Users\\Yago\\Documents\\IC")

estado <- read_excel("roraima.xlsx")
View(estado)


# Linear Interpolation

df <- estado %>%
  mutate(despjud = na.approx(despjud))

write_xlsx(df,"C:\\Users\\Yago\\Documents\\IC\\interpol.xlsx")
