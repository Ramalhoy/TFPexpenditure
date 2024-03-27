# Total factor productivity and public spending of Brazilian states: a panel data application


# 1. Abstract
This work aimed to verify the relationships between the total factor
productivity (TFP) of the Brazilian states and their public expenditures by functions,
namely: judiciary; legislative; administration/planning; education and culture;
industry, commerce and services; science and technology; social assistance and
pension; and, health. The analyzes covered the period from 2003 to 2018, and, for the
econometric estimates, the panel data methodology was adopted. Control variables
were used to give robustness to the results. The results reveal that most expenditures
did not have significant relations on total factor productivity, with the exception of
expenditures on health and the judiciary, but which had relatively small correlations.

# 2. The problem
The main reasoning behind this research is that, while there is some evidence about how the TFP behaves in regards to public expenditure, the evidences for the correlations between the TFP and public expenditure by function in state-level analysis are very limited or nonexistent, at least for the brazilian case. In this regard, our main interest revolved around estimating this relation, while the method for TFP calculation needed some workarounds. Some concessions regarding data availabity had to be made as well.

# 3. Data manipulation

A long panel data was used with information from all brazilian states (except Distrito Federal, since there's a considerable lack of data), including control variables. 
The few cases of missing data that we encountered where solved using interpolation through the `imputeTS` package. 

```
#Linear Interpolation

df <- estado %>%
  mutate(despesa = na.approx(despesa))   # where 'despesa' is any expenditure used

write_xlsx(df,"filepath") 

```

We also used a deflator for every variable that appeared in monetary terms, using the `sidrar` package in R. 

```
ipca = get_sidra(IPCA, api = '/t/1737/n1/all/v/2265/p/200312,200412,200512,200612,200712,200812,200912,201012,201112,201212,201312,201412,201512,201612,201712,201812/d/v2265%202')

ipca %<>% mutate(date = parse_date_time(`Mês (Código)`, 'ym'))

```


The general aspects of the codes used for both cases can be seen on the files "interpolação.R" and "deflac.R", respectivelly. 


# 4. TFP calculation

Notice how we talk about calculation instead of estimation - that's deliberate. Cavalcanti Ferreira's article entitled "Eficiência e Produtividade Total dos Fatores em
Minas Gerais" was used as reference, where the author used the perpetual inventory method instead of using statistical methods for TFP estimation. The Capital series is calculated through recursive methods, Human Capital is given as a mincerian function, therefore getting the inputs was an easy task.
Besides the TFP presented, we also tested for:

1. Use a proxy for the TFP, in this case the energy consumption in GWh for each state - the results not very consistent in this case;
2. Use a minmax methodology for the data already used, which resulted in very similar estimations;
3. Also very similar, we tried to calculate the TFP with other specifications for the same variables (for example, GDP and not GDP _per worker_).

As shown below, the graphs are in conformity with what the literature says about the subject and the time period considered.   

```
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

```

<img src="ptf monografia.jpg">


# 5. Estimating the relation between TPF and public expenditure

The `plm` package was of utmost importance for this project. Most of the code is a variation of the following: 

```
plm(log(ptf) ~ log(despesa)  + educ + log(IMPORT) + log(agua) + log(energiapc)+ log(popdens) + log(vak),   # repeat for every expenditure
           data=dados,
           effect = "twoways",
           model = "within")

```

There was also an effort to use lagged variables or creating indexes, but there was a lack of variability between the results. 
