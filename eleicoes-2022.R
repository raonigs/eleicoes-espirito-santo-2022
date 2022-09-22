library(dplyr)
library(ggplot2)
library(basedosdados)
library(bigrquery)
library(DBI)

# Definindo local de trabalho
setwd('c:/Users/raoni.souza/Desktop/Analises no RSTUDIO/project-eleicoes-rgs/')

# Autenticando o bigquery com a chave .json gerada no google cloud
bq_auth(path = 'project-eleicoes-rgs-e1f02702cf7d.json')

# Criando a variável para conexão do bigquery por meio do google cloud
con <- dbConnect(bigquery(), billing = 'project-eleicoes-rgs', project = 'basedosdados')

# Criando a variável de leitura utilizando a linguagem padrão SQL
query = "SELECT * FROM `basedosdados.br_poder360_pesquisas.microdados`"

# Obtendo a base de dados por meio da variável de coneção e de leitura do bigquery
df.sim = dbGetQuery(con, query)

# Analise do bigquery, seleção de variáveis, filtros, agrupamento, estatística simples e visualização #

db <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                  quantidade_entrevistas, margem_mais, margem_menos,
                                  tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                  sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES')



################################################## PRESIDENTE ###################################################

#### PESQUISA IPEC ES PRESIDENTE ESTIMULADA 1º TURNO ####

espresipecest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                        quantidade_entrevistas, margem_mais, margem_menos,
                        tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                        sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='presidente', instituto=='Ipec', tipo=='estimulada', 
         nome_candidato %in% c('Bolsonaro', 'Lula', 'branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(espresipecest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA IPEC ES PRESIDENTE ESPONTÂNEA 1º TURNO ####

espresipecesp <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                   quantidade_entrevistas, margem_mais, margem_menos,
                                   tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                   sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='presidente', instituto=='Ipec', tipo=='espontânea', 
         nome_candidato %in% c('Bolsonaro', 'Lula', 'branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(espresipecesp, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA INSTITUTO PERFIL ES PRESIDENTE ESTIMULADA 1º TURNO ####

espresperfilest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                        quantidade_entrevistas, margem_mais, margem_menos,
                        tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                        sigla_partido, percentual) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='presidente', instituto=='Instituto Perfil', 
         nome_candidato %in% c('Bolsonaro','Lula', 'branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(espresperfilest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### MÉDIA DAS PESQUISAS PRESIDENCIAIS ####
espressum <- espresperfilest
espressum$percentual <- (espresipecesp$percentual+espresipecest$percentual+espresperfilest$percentual)/3
ggplot(espressum, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)

################################################# SENADO ########################################################

#### PESQUISA IPEC SENADO ES ESPONTÂNEA ####

essenadoipecesp <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                            quantidade_entrevistas, margem_mais, margem_menos,
                            tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                            sigla_partido, percentual) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='senador', instituto=='Ipec', tipo=='espontânea',
         nome_candidato %in% c('Magno Malta','Rose de Freitas','não sabe / não respondeu',
                               'branco / nulo', 'Erick Musso')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(essenadoipecesp, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA IPEC SENADO ES ESTIMULADO ####

essenadoipecest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                     quantidade_entrevistas, margem_mais, margem_menos,
                                     tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                     sigla_partido, percentual) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='senador', instituto=='Ipec', tipo=='estimulada',
         nome_candidato %in% c('Magno Malta','Rose de Freitas','não sabe / não respondeu',
                               'branco / nulo', 'Erick Musso')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(essenadoipecest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)



#### PESQUISA INSTITUTO PERFIL SENADO ES ESTIMULADO ####

essenadoipest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                     quantidade_entrevistas, margem_mais, margem_menos,
                                     tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                     sigla_partido, percentual) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='senador', instituto=='Instituto Perfil',
         nome_candidato %in% c('Magno Malta','Rose de Freitas','indeciso',
                               'branco / nulo', 'indeciso / não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(essenadoipest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)



#### PESQUISA REAL TIME BIGDATA SENADO ES ESTIMULADO ####

essenadortbdest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                   quantidade_entrevistas, margem_mais, margem_menos,
                                   tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                   sigla_partido, percentual) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='senador', instituto=='Real Time Big Data', tipo=='estimulada',
         nome_candidato %in% c('Magno Malta','Rose de Freitas','não sabe / não respondeu',
                               'branco / nulo')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(essenadortbdest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### MÉDIA DAS PESQUISAS DO SENADO ####
senadosumipec <- estsenadoesipec
senadosumipec$percentual <- (estsenadoesipec$percentual+espsenadoesipec$percentual)/2

ggplot(senadosumipec, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)



################################################### GOVERNADOR #################################################

#### PESQUISA IPEC ES GOVERNADOR ESTIMULADA 1º TURNO ####

esgovipecest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                   quantidade_entrevistas, margem_mais, margem_menos,
                                   tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                   sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='governador', instituto=='Ipec', tipo=='estimulada', 
         nome_candidato %in% c('Renato Casagrande', 'Carlos Manato', 
                               'Audifax Barcelos','branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(esgovipecest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA IPEC ES GOVERNADOR ESPONTÂNEA 1º TURNO ####

esgovipecesp <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                  quantidade_entrevistas, margem_mais, margem_menos,
                                  tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                  sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='governador', instituto=='Ipec', tipo=='espontânea', 
         nome_candidato %in% c('Renato Casagrande', 'Carlos Manato', 
                               'Audifax Barcelos','branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(esgovipecesp, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA REALTIME BIGDATA ES GOVERNADOR ESPONTÂNEA 1º TURNO ####

esgovrtbdesp <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                  quantidade_entrevistas, margem_mais, margem_menos,
                                  tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                  sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='governador', instituto=='Real Time Big Data', tipo=='espontânea', 
         nome_candidato %in% c('Renato Casagrande', 'Carlos Manato', 
                               'Audifax Barcelos','branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(esgovrtbdesp, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA REALTIME BIGDATA ES GOVERNADOR ESTIMULADA 1º TURNO ####

esgovrtbdest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                  quantidade_entrevistas, margem_mais, margem_menos,
                                  tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                  sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='governador', instituto=='Real Time Big Data', tipo=='estimulada', 
         nome_candidato %in% c('Renato Casagrande', 'Carlos Manato', 
                               'Audifax Barcelos','branco / nulo', 'não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(esgovrtbdest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA INSTITUTO PERFIL ES GOVERNADOR ESTIMULADA 1º TURNO ####

esgovipest <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                  quantidade_entrevistas, margem_mais, margem_menos,
                                  tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                  sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='governador', instituto=='Instituto Perfil', tipo=='estimulada', 
         nome_candidato %in% c('Renato Casagrande', 'Carlos Manato', 
                               'Audifax Barcelos','branco / nulo', 'indeciso / não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(esgovipest, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### PESQUISA INSTITUTO PERFIL ES GOVERNADOR ESPONTÂNEA 1º TURNO ####

esgovipesp <- df.sim %>% select(ano, sigla_uf, cargo, instituto, contratante, 
                                  quantidade_entrevistas, margem_mais, margem_menos,
                                  tipo, turno, tipo_voto, descricao_cenario, nome_candidato,
                                  sigla_partido, percentual, data_referencia) %>% 
  filter(ano==2022, sigla_uf=='ES', cargo=='governador', instituto=='Instituto Perfil', tipo=='espontânea', 
         nome_candidato %in% c('Renato Casagrande', 'Carlos Manato', 
                               'Audifax Barcelos','branco / nulo', 'indeciso / não sabe / não respondeu')) %>% 
  group_by(nome_candidato) %>% 
  summarise(percentual=mean(percentual))

ggplot(esgovipesp, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)


#### MEDIA DAS PESQUISAS DE GOVERNADOR DO ES ####

medesgov <- esgovipest
medesgov$percentual <- (esgovipecest$percentual+esgovipecesp$percentual+esgovrtbdesp$percentual+esgovrtbdest$percentual)/4

ggplot(medesgov, aes(x=nome_candidato, y=percentual, fill=nome_candidato))+
  geom_col(show.legend = F)+
  geom_text(aes(label=round(percentual,1)), vjust = -0.3, size = 3.5)










