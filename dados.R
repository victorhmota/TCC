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

df_pop <- pivot_longer(cresc_pop, 
                       cols = starts_with('X'), 
                       names_to = "ano",
                       values_to = "cresc_pop")

