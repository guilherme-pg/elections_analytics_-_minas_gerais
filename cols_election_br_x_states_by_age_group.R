# electoral profile: MG x BR ----- BY age group





# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais")


library(dplyr)
library(ggplot2)


options(scipen=999)



# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

general_electoral_profile_2022 <- read.table("dados/perfil_eleitorado_2022.csv", header=TRUE, sep=";")





# ~~~~ function ~~~~ SET PROPORTIONS AND PERCENTS BY TOTAL

set_proportions_and_percents <- function(df) {
  
  df <- df %>%
    group_by(DS_FAIXA_ETARIA) %>%
    summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))
  
  # ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
  df$TOTAL_VOTES <- sum(df$QT_ELEITORES_PERFIL)
  
  # ADD PROPORTION
  df$PROPORTION <- df$QT_ELEITORES_PERFIL/ df$TOTAL_VOTES
  
  # ADD PERCENT
  df$PERCENT <- df$PROPORTION *100
  
  # FORMAT PERCENT
  df$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df$PERCENT))
  
  df$VOTES_MEASURE <- "Total Votes"
  
  return(df)
}





# ~~~~~~~~~~~~~~~~  reorder data  ~~~~~~~~~~~~~~~~

# group data
br_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))


# group data by State
sp_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="SP") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

mg_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="MG") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

rj_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="RJ") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

ba_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="BA") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

pr_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="PR") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

rs_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="RS") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

pe_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="PE") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))









br_electoral_profile_2022_grouped <- set_proportions_and_percents(br_electoral_profile_2022_grouped)
sp_electoral_profile_2022_grouped <- set_proportions_and_percents(sp_electoral_profile_2022_grouped)
mg_electoral_profile_2022_grouped <- set_proportions_and_percents(mg_electoral_profile_2022_grouped)
rj_electoral_profile_2022_grouped <- set_proportions_and_percents(rj_electoral_profile_2022_grouped)
ba_electoral_profile_2022_grouped <- set_proportions_and_percents(ba_electoral_profile_2022_grouped)
pr_electoral_profile_2022_grouped <- set_proportions_and_percents(pr_electoral_profile_2022_grouped)
rs_electoral_profile_2022_grouped <- set_proportions_and_percents(rs_electoral_profile_2022_grouped)
pe_electoral_profile_2022_grouped <- set_proportions_and_percents(pe_electoral_profile_2022_grouped)



# set scope difference Brasil X Minas Gerais
br_electoral_profile_2022_grouped$SG_UF <- "Brasil"
mg_electoral_profile_2022_grouped$SG_UF <- "Minas Gerais"
sp_electoral_profile_2022_grouped$SG_UF <- "São Paulo"
rj_electoral_profile_2022_grouped$SG_UF <- "Rio de Janeiro"
ba_electoral_profile_2022_grouped$SG_UF <- "Bahia"
pr_electoral_profile_2022_grouped$SG_UF <- "Paraná"
rs_electoral_profile_2022_grouped$SG_UF <- "Rio Grande do Sul"
pe_electoral_profile_2022_grouped$SG_UF <- "Pernambuco"


# append datasets
electoral_profile_2022_grouped_merged <- rbind(
  br_electoral_profile_2022_grouped,
  sp_electoral_profile_2022_grouped,
  mg_electoral_profile_2022_grouped,
  rj_electoral_profile_2022_grouped,
  ba_electoral_profile_2022_grouped,
  pr_electoral_profile_2022_grouped,
  rs_electoral_profile_2022_grouped,
  pe_electoral_profile_2022_grouped
)


# adjust ages values
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "16 anos                       "] <- "16 e 17 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "17 anos                       "] <- "16 e 17 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "18 anos                       "] <- "18 a 20 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "19 anos                       "] <- "18 a 20 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "20 anos                       "] <- "20 a 29 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "21 a 24 anos                  "] <- "20 a 29 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "25 a 29 anos                  "] <- "20 a 29 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "30 a 34 anos                  "] <- "30 a 39 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "35 a 39 anos                  "] <- "30 a 39 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "40 a 44 anos                  "] <- "40 a 49 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "45 a 49 anos                  "] <- "40 a 49 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "50 a 54 anos                  "] <- "50 a 59 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "55 a 59 anos                  "] <- "50 a 59 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "60 a 64 anos                  "] <- "60 a 69 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "65 a 69 anos                  "] <- "60 a 69 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "70 a 74 anos                  "] <- "70 a 79 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "75 a 79 anos                  "] <- "70 a 79 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "80 a 84 anos                  "] <- "80 a 89 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "85 a 89 anos                  "] <- "80 a 89 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "90 a 94 anos                  "] <- "acima de 90 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "95 a 99 anos                  "] <- "acima de 90 anos"
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA[electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA == "100 anos ou mais              "] <- "acima de 90 anos"


table(electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA)


# ~~~~~~~~~~~~~~~~  general plot variables  ~~~~~~~~~~~~~~~~

schooling_colors <- c(
  ""="",
  ""="",
  ""="",
  ""="",
  ""="",
  ""="",
  ""="",
  ""="",
  ""="",
  ""=""
)

# set order to school level
electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA <- factor(electoral_profile_2022_grouped_merged$DS_FAIXA_ETARIA, levels=c(
  "Analfabeto",
  "Lê e Escreve",
  "Ensino Fundamental Incompleto",
  "Ensino Fundamental Completo",
  "Ensino Médio Incompleto",
  "Ensino Médio Completo",
  "Superior Incompleto",
  "Superior Completo",
  "Não Informado"
))

# set order to state by population
electoral_profile_2022_grouped_merged$SG_UF <- factor(electoral_profile_2022_grouped_merged$SG_UF, levels=c(
  "Brasil",
  "São Paulo",
  "Minas Gerais",
  "Rio de Janeiro",
  "Bahia",
  "Paraná",
  "Rio Grande do Sul",
  "Pernambuco"
))






# ~~~~~~~~~~~~~~~~  plot  ~~~~~~~~~~~~~~~~

electoral_profile_2022_grouped_merged %>%
  ggplot(aes(x=SG_UF,
             y=PERCENT,
             fill=DS_FAIXA_ETARIA)) +
  geom_col()









