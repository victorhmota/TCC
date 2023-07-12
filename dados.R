library("tidyverse")
library("basedosdados")
library('ipeadatar')


#set_billing_id("tcc-demografia-e-crescimento")
#query <- bdplyr(c("br_ibge_censo_demografico.microdados_domicilio_1980"))
#df <- bd_collect(query)

ipeadatar::search_series(terms = 'População total')

#População
pop <- ipeadata('ADH_PESOTOT')
pop_mun <- pop %>% filter(pop$uname == 'Municipality')
