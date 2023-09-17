library(tidyverse)
library(basedosdados)
library(ipeadatar)
library(plm)
library(haven)
library(gmm)


###### Obtendo os dados ##########

### Taxa de crescimento da população ###

pop <- read.csv('Dados IPEA/pop.csv', skip = 1 )
pop_mun <- pop %>% select(-c('X1996','X2007', 'X')) %>% drop_na()

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
pea <- pea %>% select(-X) %>% drop_na()

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

df_suporte2$Y2010_cresc <- rowMeans(df_suporte2[, 10:12], na.rm = TRUE)
capital$X2010 <- (1+df_suporte2$Y2010_cresc)*capital$X2000
capital <- capital %>% select(-X) %>% drop_na()

df_capital <- pivot_longer(capital, 
                             cols = starts_with('X'), 
                             names_to = "ano",
                             values_to = "capital")

df_capital['ano'][df_capital['ano'] == "X1970"] <- '1970'
df_capital['ano'][df_capital['ano'] == "X1980"] <- '1980'
df_capital['ano'][df_capital['ano'] == "X1991"] <- '1991'
df_capital['ano'][df_capital['ano'] == "X2000"] <- '2000'
df_capital['ano'][df_capital['ano'] == "X2010"] <- '2010'

capital_trabalho <- left_join(df_dados_pea, df_capital)
capital_trabalho$capital_trab <- capital_trabalho$capital/capital_trabalho$pea

df_suporte3 <- capital_trabalho %>% select(c('Sigla','Código', 'Município', 'ano', 'capital_trab'))

df_suporte3 <- df_suporte3 %>% pivot_wider(names_from = 'ano',
                                           values_from = 'capital_trab')

colnames(df_suporte3)[4:8] <- c('X1970','X1980', 'X1991', 'X2000', 'X2010')


cresc_cap_trab <- df_suporte3 %>% mutate(Y1970_cresc = NA,
                                         Y1980_cresc = log(X1980/X1970), 
                                         Y1991_cresc = log(X1991/X1980), 
                                         Y2000_cresc = log(X2000/X1991),
                                         Y2010_cresc = log(X2010/X2000))                      

df_cresc_cap_trab <- cresc_cap_trab %>% select(-c('X1970','X1980','X1991','X2000', 'X2010')) %>% 
                                                  pivot_longer(cols = starts_with('Y'), 
                                                  names_to = "ano",
                                                  values_to = "cresc_cap_trab")

df_cresc_cap_trab['ano'][df_cresc_cap_trab ['ano'] == "Y1970_cresc"] <- '1970'
df_cresc_cap_trab['ano'][df_cresc_cap_trab ['ano'] == "Y1980_cresc"] <- '1980'
df_cresc_cap_trab['ano'][df_cresc_cap_trab ['ano'] == "Y1991_cresc"] <- '1991'
df_cresc_cap_trab['ano'][df_cresc_cap_trab ['ano'] == "Y2000_cresc"] <- '2000'
df_cresc_cap_trab['ano'][df_cresc_cap_trab ['ano'] == "Y2010_cresc"] <- '2010'

df_capital_final <- left_join(capital_trabalho, df_cresc_cap_trab)

### Escolaridade ###

estados <- c("RO" = '11', "AC" = "12", 'AM' = '13', 'RO' = '14', 'PA' = '15', 'AP' = '16',
             'TO' = '17', 'MA' = '21', 'PI' = '22', 'CE' = '23', 'RN' = '24', 'PB' = '25',
             'PE' = '26', 'AL' = '27', 'SE' = '28', 'BA' = '29', 'MG' = '31', 'ES'= '32', 
             'RJ' = '33','SP' = '35', 'PR' = '41', 'SC' = '42', 'RS' = '43', 'MS' = '50',
             'MT' = '51', 'GO' = '52', 'DF' = '53')

estados2 <- c('11' = "RO", '12' ="AC", '13' = 'AM', '14' = 'RO', '15' = 'PA', '16' ='AP',
             '17' = 'TO', '21' = 'MA', '22' = 'PI', '23'= 'CE', '24' = 'RN', '25' = 'PB',
             '26' = 'PE', '27' = 'AL', '28' = 'SE', '29' = 'BA', '31' = 'MG', '32' = 'ES', 
             '33' = 'RJ','35'= 'SP', '41' = 'PR', '42' = 'SC', '43' = 'RS', '50' = 'MS',
             '51' = 'MT', '52' = 'GO', '53' = 'DF')


escolaridade <- read.csv('Dados IPEA/escolaridade.csv', skip = 1) %>% select(-X) %>% drop_na()
dados_educ2010 <- read_dta('Dados/2010_completo.dta')
dados_educ2000 <- read_dta('Dados/2000_completo.dta')

escolaridade2010 <- dados_educ2010 %>% select(c('UF','codigo7', 'anos_est'))
escolaridade2010$Sigla <- str_replace_all(escolaridade2010$UF, pattern = estados2)
colnames(escolaridade2010) <- c('UF', 'Código', 'X2010', 'Sigla')
escolaridade2010$UF <- as.character(escolaridade2010$UF)

escolaridade2000 <- dados_educ2000 %>% select(c('UF','codigo7', 'anos_est'))
escolaridade2000$Sigla <- str_replace_all(escolaridade2000$UF, pattern = estados2)
colnames(escolaridade2000) <- c('UF', 'Código', 'X2000', 'Sigla')
escolaridade2000$UF <- as.character(escolaridade2000$UF)

escolaridade$UF <- str_replace_all(escolaridade$Sigla, pattern = estados)


dados_escolaridade1 <- left_join(escolaridade, escolaridade2000)
df_escolaridade <- left_join(dados_escolaridade1, escolaridade2010)

df_suporte4 <- df_escolaridade %>% mutate(Y1970_cresc = NA,
                                         Y1980_cresc = X1980 - X1970, 
                                         Y1991_cresc = X1991 - X1980, 
                                         Y2000_cresc = X2000 - X1991,
                                         Y2010_cresc = X2010 - X2000) 

df_esc <- df_suporte4 %>% select(c(Sigla, 
                                  Código, 
                                  Município,
                                  X1970,
                                  X1980,
                                  X1991, 
                                  X2000, 
                                  X2010))

df_cresc_esc<- df_suporte4 %>% select(c(Sigla, 
                                        Código, 
                                        Município,
                                        Y1970_cresc,
                                        Y1980_cresc,
                                        Y1991_cresc, 
                                        Y2000_cresc, 
                                        Y2010_cresc))
df_esc_final <- pivot_longer(df_esc, 
                             cols = starts_with('X'), 
                             names_to = "ano",
                             values_to = "escolaridade")


df_esc_final['ano'][df_esc_final['ano'] == "X1970"] <- '1970'
df_esc_final['ano'][df_esc_final['ano'] == "X1980"] <- '1980'
df_esc_final['ano'][df_esc_final['ano'] == "X1991"] <- '1991'
df_esc_final['ano'][df_esc_final['ano'] == "X2000"] <- '2000'
df_esc_final['ano'][df_esc_final['ano'] == "X2010"] <- '2010'

df_cresc_esc_final <- pivot_longer(df_cresc_esc, 
                                   cols = starts_with('Y'), 
                                   names_to = "ano",
                                   values_to = "cresc_escolaridade")

df_cresc_esc_final['ano'][df_cresc_esc_final['ano'] == "Y1970_cresc"] <- '1970'
df_cresc_esc_final['ano'][df_cresc_esc_final['ano'] == "Y1980_cresc"] <- '1980'
df_cresc_esc_final['ano'][df_cresc_esc_final['ano'] == "Y1991_cresc"] <- '1991'
df_cresc_esc_final['ano'][df_cresc_esc_final['ano'] == "Y2000_cresc"] <- '2000'
df_cresc_esc_final['ano'][df_cresc_esc_final['ano'] == "Y2010_cresc"] <- '2010'

df_dados_esc <- left_join(df_esc_final, df_cresc_esc_final)



# População 15-64 anos

rd <- read.csv('Dados IPEA/RD.csv', skip = 1)

rd <- rd %>% select(-X) %>% drop_na()

df_wa <- rd %>% pivot_longer(cols = starts_with('X'),
                              names_to = "ano",
                              values_to = "rd") %>%
                mutate(wa = 1/(rd/100+1))

df_wa['ano'][df_wa['ano'] == 'X1970'] <- '1970'
df_wa['ano'][df_wa['ano'] == 'X1980'] <- '1980'
df_wa['ano'][df_wa['ano'] == 'X1991'] <- '1991'
df_wa['ano'][df_wa['ano'] == 'X2000'] <- '2000'
df_wa['ano'][df_wa['ano'] == 'X2010'] <- '2010'


### Taxa de participação ###


df_pea <- pea %>% pivot_longer(cols = starts_with('X'),
                               names_to = "ano",
                               values_to = "pea")
df_pop <- pop_mun %>% pivot_longer(cols = starts_with('X'),
                               names_to = "ano",
                               values_to = "pop")

df_trab_pop <- left_join(df_pop_final, df_dados_pea) %>% mutate(trab_pop = pea/pop)

df_prate <- left_join(df_trab_pop, df_wa) %>% mutate(prate = trab_pop/wa)
 
### Renda per capita ###

renda <- read.csv('Dados IPEA/renda_per_capita.csv', skip = 1) %>% select(-X) %>% drop_na()
renda$X1970 <- NA
renda$X1980 <- NA


renda_cresc <-  renda %>% mutate(Y1970_cresc = NA,
                                 Y1980_cresc = log(X1980/X1970), 
                                 Y1991_cresc = log(X1991/X1980), 
                                 Y2000_cresc = log(X2000/X1991), 
                                 Y2010_cresc = log(X2010/X2000)) %>% select(c(Sigla, Código, Município, Y1970_cresc, Y1980_cresc, Y1991_cresc, Y2000_cresc, Y2010_cresc))


df_renda <- pivot_longer(renda, 
                       cols = starts_with('X'), 
                       names_to = "ano",
                       values_to = "renda")

df_renda['ano'][df_renda['ano'] == "X1970"] <- '1970'
df_renda['ano'][df_renda['ano'] == "X1980"] <- '1980'
df_renda['ano'][df_renda['ano'] == "X1991"] <- '1991'
df_renda['ano'][df_renda['ano'] == "X2000"] <- '2000'
df_renda['ano'][df_renda['ano'] == "X2010"] <- '2010'

df_renda_cresc <- pivot_longer(renda_cresc, 
                             cols = starts_with('Y'), 
                             names_to = "ano",
                             values_to = "cresc_renda")

df_renda_cresc['ano'][df_renda_cresc['ano'] == "Y1970_cresc"] <- '1970'
df_renda_cresc['ano'][df_renda_cresc['ano'] == "Y1980_cresc"] <- '1980'
df_renda_cresc['ano'][df_renda_cresc['ano'] == "Y1991_cresc"] <- '1991'
df_renda_cresc['ano'][df_renda_cresc['ano'] == "Y2000_cresc"] <- '2000'
df_renda_cresc['ano'][df_renda_cresc['ano'] == "Y2010_cresc"] <- '2010'


df_renda_final <- left_join(df_renda, df_renda_cresc)


### Base de dados final ####

df_1 <- left_join(df_prate, df_capital_final)
df_2 <- left_join(df_1, df_dados_esc)
df_final <- left_join(df_2, df_renda_final)

df_final2 <- df_final %>% filter((df_final$ano != "1970") & (df_final$ano != "1980"))

df_final3 <- table(df_final2)

#Estimação sys-GMM

df_painel <- pdata.frame(df_final3, index = c("Código", "ano"))



modelo <- pgmm(log(renda) ~ log(lag(renda)) + lag(escolaridade) + log(lag(prate)) + log(lag(wa)) + cresc_escolaridade + cresc_cap_trab + cresc_pop + cresc_pea | log(lag(renda)) + lag(escolaridade) , 
                     data = df_painel,
                     model = 'twosteps',
                     transformation = 'ld')

g <- renda ~  escolaridade + prate + wa
h <- ~ renda + escolaridade

modelo_teste <- sysGmm(g, h, 
                     data = df_painel)

write_dta(
  df_final2,
  dados.dta,
  version = 14,
  label = attr(df_final2, "label"),
  strl_threshold = 2045,
  adjust_tz = TRUE
)

summary(modelo_teste)
#https://search.r-project.org/CRAN/refmans/plm/html/pgmm.html

