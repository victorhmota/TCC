
#Carregando pacotes

library(tidyverse)
library(basedosdados)
library(ipeadatar)
library(plm)

######## Obtenção dos dados ###################

####### Renda per capita #########
censo_91 <- read_dta('C:\\Users\\WALCIR\\Documents\\VICTOR HUGO\\ECONOMIA\\TCC')
rpc <- 