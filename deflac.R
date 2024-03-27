library(readxl)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(lubridate)
library(sidrar)

despesa_assist <- read_excel("Dados/despesa função/despesa assist.xls")


ipca = get_sidra(IPCA, api = '/t/1737/n1/all/v/2265/p/200312,200412,200512,200612,200712,200812,200912,201012,201112,201212,201312,201412,201512,201612,201712,201812/d/v2265%202')

ipca %<>% mutate(date = parse_date_time(`Mês (Código)`, 'ym'))

