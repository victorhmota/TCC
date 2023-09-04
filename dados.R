library(tidyverse)
library(basedosdados)
library(ipeadatar)
library(plm)
library(haven)

###### Obtendo os dados ##########

### Taxa de crescimento da população ###

pop <- read.csv('Dados IPEA/pop.csv', skip = 1 )
pop_mun <- pop %>% select(-c('X1996','X2007', 'X'))

pop_cresc <-  pop_mun %>% mutate(Y1970_cresc = NA,
                               Y1980_cresc = log(X1980/X1970), 
                               Y1991_cresc = log(X1991/pop_mun$X1980), 
                               Y2000_cresc = log(X2000/pop_mun$X1991), 
                               Y2010_cresc = log(X2010/pop_mun$X2000)) %>% select(c(Sigla, Código, Município, Y1970_cresc, Y1980_cresc, Y1991_cresc, Y2000_cresc, Y2010_cresc))


df_pop <- pivot_longer(pop_mun, 
                       cols = starts_with('X'), 
                       names_to = "ano",
                       values_to = "pop")

df_pop['ano'][df_pop['ano'] == "X1970"] <- '1970'
df_pop['ano'][df_pop['ano'] == "X1980"] <- '1980'
df_pop['ano'][df_pop['ano'] == "X1991"] <- '1991'
df_pop['ano'][df_pop['ano'] == "X2000"] <- '2000'
df_pop['ano'][df_pop['ano'] == "X2010"] <- '2010'

df_cresc_pop <- pivot_longer(pop_cresc, 
                             cols = starts_with('Y'), 
                             names_to = "ano",
                             values_to = "cresc_pop")

df_cresc_pop['ano'][df_cresc_pop['ano'] == "Y1970_cresc"] <- '1970'
df_cresc_pop['ano'][df_cresc_pop['ano'] == "Y1980_cresc"] <- '1980'
df_cresc_pop['ano'][df_cresc_pop['ano'] == "Y1991_cresc"] <- '1991'
df_cresc_pop['ano'][df_cresc_pop['ano'] == "Y2000_cresc"] <- '2000'
df_cresc_pop['ano'][df_cresc_pop['ano'] == "Y2010_cresc"] <- '2010'


df_pop_final <- left_join(df_pop, df_cresc_pop)

### Força de trabalho ###

pea <- read.csv('Dados IPEA/PEA.csv', skip = 1)
pea <- pea %>% select(-X)

df_suporte <- pea %>% mutate(Y1970_cresc = NA,
                            Y1980_cresc = log(X1980/X1970), 
                            Y1991_cresc = log(X1991/X1980), 
                            Y2000_cresc = log(X2000/X1991)) 

df_suporte$Y2010_cresc <- rowMeans(df_suporte[, 9:11], na.rm = TRUE)
df_suporte$X2010 <- (1+df_suporte$Y2010_cresc)*df_suporte$X2000

df_pea <- df_suporte %>% select(c(Sigla, 
                                  Código, 
                                  Município,
                                  X1970,
                                  X1980,
                                  X1991, 
                                  X2000, 
                                  X2010))

df_cresc_pea <- df_suporte %>% select(c(Sigla, 
                                        Código, 
                                        Município,
                                        Y1970_cresc,
                                        Y1980_cresc,
                                        Y1991_cresc, 
                                        Y2000_cresc, 
                                        Y2010_cresc))
df_pea_final <- pivot_longer(df_pea, 
                             cols = starts_with('X'), 
                             names_to = "ano",
                             values_to = "pea")


df_pea_final['ano'][df_pea_final['ano'] == "X1970"] <- '1970'
df_pea_final['ano'][df_pea_final['ano'] == "X1980"] <- '1980'
df_pea_final['ano'][df_pea_final['ano'] == "X1991"] <- '1991'
df_pea_final['ano'][df_pea_final['ano'] == "X2000"] <- '2000'
df_pea_final['ano'][df_pea_final['ano'] == "X2010"] <- '2010'

df_cresc_pea_final <- pivot_longer(df_cresc_pea, 
                                   cols = starts_with('Y'), 
                                   names_to = "ano",
                                   values_to = "cresc_pea")

df_cresc_pea_final['ano'][df_cresc_pea_final['ano'] == "Y1970_cresc"] <- '1970'
df_cresc_pea_final['ano'][df_cresc_pea_final['ano'] == "Y1980_cresc"] <- '1980'
df_cresc_pea_final['ano'][df_cresc_pea_final['ano'] == "Y1991_cresc"] <- '1991'
df_cresc_pea_final['ano'][df_cresc_pea_final['ano'] == "Y2000_cresc"] <- '2000'
df_cresc_pea_final['ano'][df_cresc_pea_final['ano'] == "Y2010_cresc"] <- '2010'

df_dados_pea <- left_join(df_pea_final, df_cresc_pea_final)


### Estoque de capital ### 

capital <- read.csv('Dados IPEA/capital.csv', skip = 1)

df_suporte2 <- capital %>% mutate(Y1970_cresc = NA,
                                  Y1980_cresc = log(X1980/X1970), 
                                  Y1991_cresc = log(X1991/X1980), 
                                  Y2000_cresc = log(X2000/X1991))

df_suporte2$Y2010_cresc <- rowsMean(df_suporte2[, 10:12], rm.na = TRUE)




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

escolaridade <- read.csv('Dados IPEA/escolaridade.csv', skip = 1)

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
df_4 <- left_join(df_3, df_renda_final)
df_final <- left_join(df_4, df_escolaridade)

#Estimação sys-GMM
df_painel <- pdata.frame(df_final, index = c("Código", "ano"))


modelo_teste <- pgmm(cresc_renda ~ escolaridade + wa + cresc_capita, df_painel,
                     model = "twosteps",
                     transformation = "ld") 

#https://search.r-project.org/CRAN/refmans/plm/html/pgmm.html

