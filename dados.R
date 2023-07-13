library("tidyverse")
library("basedosdados")
library('ipeadatar')


#Taxa de crescimento da população
pop <- read.csv('pop.csv', skip = 1 )
pop_mun <- pop %>% select(-c('X1996','X2007', 'X'))

cresc_pop <- pop_mun
cresc_pop <- cresc_pop %>% mutate(X1980 = X1980/X1970 - 1, 
                                  X1991 = X1991/pop_mun$X1980 - 1, 
                                  X2000 = X2000/pop_mun$X1991 - 1, 
                                  X2010 = X2010/pop_mun$X2000 - 1)

cresc_pop <- cresc_pop %>% select(-'X1970')

df_pop <- pivot_longer(cresc_pop, 
                       cols = starts_with('X'), 
                       names_to = "ano",
                       values_to = "cresc_pop")


#Força de trabalho

pea <- read.csv('PEA.csv', skip = 1)

#Estoque de capital

capital <- read.csv('capital.csv', skip = 1)

capital_trabalho <- inner_join(capital, pea, by = 'Código')
capital_trabalho <- capital_trabalho %>% mutate(k_1970 = X1970.x/X1970.y,
                           k_1980 = X1980.x/X1980.y,
                           k_1991 = X1991.x/X1991.y,
                           k_2000 = X2000.x/X2000.y)
capital_trabalho <- capital_trabalho %>% select(Sigla.x, Código, Município.x,
                                               k_1970, k_1980, k_1991, k_2000)
colnames(capital_trabalho) <- c('Sigla', 'Código', 'Município', 'X1970',
                                'X1980',
                                'X1991', 'X2000')

cresc_capital <- capital_trabalho
cresc_capital <- cresc_capital %>% mutate(X1980 = X1980/X1970 - 1, 
                                      X1991 = X1991/capital_trabalho$X1980 - 1, 
                                      X2000 = X2000/capital_trabalho$X1991 - 1)
df_capital <- pivot_longer(cresc_capital, 
                           cols = starts_with('X'), 
                           names_to = "ano",
                           values_to = "cresc_capital")
