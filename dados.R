library("tidyverse")
library("basedosdados")
library('ipeadatar')


#Taxa de crescimento da população
pop <- read.csv('pop.csv', skip = 1 )
pop_mun <- pop %>% select(-c('X1996','X2007', 'X'))

cresc_pop <- pop_mun
cresc_pop <- cresc_pop %>% mutate(X1980 = log(X1980/X1970), 
                                  X1991 = log(X1991/pop_mun$X1980), 
                                  X2000 = log(X2000/pop_mun$X1991), 
                                  X2010 = log(X2010/pop_mun$X2000))

cresc_pop <- cresc_pop %>% select(-'X1970')

df_cresc_pop <- pivot_longer(cresc_pop, 
                       cols = starts_with('X'), 
                       names_to = "ano",
                       values_to = "cresc_pop")


#Força de trabalho

pea <- read.csv('PEA.csv', skip = 1)
pea <- pea %>% select(-X)

cresc_pea <- pea
cresc_pea <- cresc_pea %>% mutate(X1980 = log(X1980/X1970), 
                                  X1991 = log(X1991/pea$X1980), 
                                  X2000 = log(X2000/pea$X1991))

cresc_pea <- cresc_pea %>% select(-c(X, X1970))
                                  
df_cresc_pea <- pivot_longer(cresc_pea, 
                       cols = starts_with('X'), 
                       names_to = "ano",
                       values_to = "cresc_pea")

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
cresc_capital <- cresc_capital %>% mutate(X1980 = log(X1980/X1970), 
                                      X1991 = log(X1991/capital_trabalho$X1980), 
                                      X2000 = log(X2000/capital_trabalho$X1991))

cresc_capital <- cresc_capital %>% select(-X1970)

df_capital <- pivot_longer(cresc_capital, 
                           cols = starts_with('X'), 
                           names_to = "ano",
                           values_to = "cresc_capital")
#Escolaridade

escolaridade <- read.csv('escolaridade.csv', skip = 1)

var_escolaridade <- escolaridade
var_escolaridade <- var_escolaridade %>% mutate(log(X1980 = X1980/X1970), 
                                                X1991 = log(X1991/escolaridade$X1980))

escolaridade <- escolaridade %>% select(-c(X1970, X))
var_escolaridade <- var_escolaridade %>% select(-c(X, X1970))


df_escolaridade <- pivot_longer(escolaridade, 
                           cols = starts_with('X'), 
                           names_to = "ano",
                           values_to = "escolaridade")

df_var_escolaridade <-  pivot_longer(var_escolaridade, 
                                     cols = starts_with('X'), 
                                     names_to = "ano",
                                     values_to = "var_escolaridade")
# População 15-64 anos

rd <- read.csv('RD.csv', skip = 1)

rd <- rd %>% select(-X)

df_wa <- rd %>% pivot_longer(cols = starts_with('X'),
                              names_to = "ano",
                              values_to = "rd") %>%
                mutate(wa = 1/(rd+1))

#Taxa de participação


df_pea <- pea %>% pivot_longer(cols = starts_with('X'),
                               names_to = "ano",
                               values_to = "pea")
df_pop <- pop_mun %>% pivot_longer(cols = starts_with('X'),
                               names_to = "ano",
                               values_to = "pop")

df_trab_pop <- left_join(df_pop, df_pea) %>% mutate(trab_pop = pea/pop)

df_prate <- left_join(df_trab_pop, df_wa) %>% mutate(prate = trab_pop/wa)
 
#PIB (renda)

renda <- read.csv('renda_per_capita.csv', skip = 1)

cresc_renda <- renda %>% select(-X)

cresc_renda <- cresc_renda %>% mutate(X2000 = log(X2000/X1991), 
                                      X2010 = log(X2010/renda$X2000)) %>% select(-X1991)
                                    

df_renda <- renda %>% select(-X) %>% pivot_longer(cols = starts_with('X'),
                         names_to = 'ano',
                         values_to = 'renda_per_capita')

df_cresc_renda <- cresc_renda %>% pivot_longer(cols = starts_with('X'),
                                                        names_to = 'ano',
                                                        values_to = 'cresc_renda')

df_renda_final <- left_join(df_renda, df_cresc_renda)

#Base de dados final

df_1 <- left_join(df_prate, df_cresc_pea)
df_2 <- left_join(df_1, df_cresc_pop)
df_3 <- left_join(df_2, df_capital)
df_final <- left_join(df_3, df_escolaridade)

