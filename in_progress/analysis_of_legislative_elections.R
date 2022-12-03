# analysis of legislative elections


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais/in_progress")


library(dplyr)
library(ggplot2)
library(cowplot)


options(scipen=999)



# general variables
by_states <- data.frame(matrix(ncol = 13, nrow = 0))
by_federals <- data.frame(matrix(ncol = 13, nrow = 0))

columns_names <- c(
  'ANO_ELEICAO', 'NR_TURNO', 'SG_UF', 'NM_UE', 'CD_MUNICIPIO',
  'NM_MUNICIPIO', 'DS_CARGO', 'NM_CANDIDATO', 'NM_URNA_CANDIDATO',
  'SG_PARTIDO', 'NM_PARTIDO', 'QT_VOTOS_NOMINAIS_VALIDOS',
  'DS_SIT_TOT_TURNO'
)

colnames(by_states) <- columns_names
colnames(by_federals) <- columns_names





# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ REDUCE VARIABLES  ~~~~~~~~~~~~~~~~

reduce_variables <- function(df) {
  
  df <- df %>%
    select(ANO_ELEICAO, NR_TURNO, SG_UF, NM_UE, CD_MUNICIPIO,
           NM_MUNICIPIO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
           SG_PARTIDO, NM_PARTIDO, QT_VOTOS_NOMINAIS_VALIDOS,
           DS_SIT_TOT_TURNO)
  
  return(df)
}



# ~~~~ function ~~~~ SEPARATE BY POSITION  ~~~~~~~~~~~~~~~~

separate_by_position <- function(df) {
  
  state_representative <- df %>%
    filter(DS_CARGO == "Deputado Estadual")
  
  federal_representative <- df %>%
    filter(DS_CARGO == "Deputado Federal")
  
  representative_df_list <- list(
    state_representative,
    federal_representative
  )
  
  return(representative_df_list)
}



# ~~~~ function ~~~~ select elected

select_elected <- function(df) {
  
  df <- df %>%
    filter(DS_SIT_TOT_TURNO == "ELEITO ELEITO POR MÉDIA" || DS_SIT_TOT_TURNO == "ELEITO POR QP")
  
  return(df)
}



# ~~~~ function ~~~~ AUTOMATION

automatic_analysis <-function(df_list) {
  
  for(df in df_list) {
    
    df <- reduce_variables(df)
    
    df_representatives <- separate_by_position(df) # return a list[1 e 2]
    
    state_rep <- select_elected(df_representatives[[1]])
    federal_rep <- select_elected(df_representatives[[2]])
    
    by_states <- rbind(by_states, state_rep)
    by_federals <- rbind(by_federals, federal_rep)
  }
  
  # PLOT for each cargo
  # SAVE para cada cargo
  
}







# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

#mg_2010 <- read.table("../dados/***************", header=TRUE, sep=";")
#mg_2014 <- read.table("../dados/***************", header=TRUE, sep=";")
mg_2018 <- read.table("../dados/votacao_candidato_munzona_2018_MG.csv", header=TRUE, sep=";")
mg_2022 <- read.table("../dados/votacao_candidato_munzona_2022_MG.csv", header=TRUE, sep=";")

all_dataframes <- list(
  mg_2022,
  mg_2018
)

table(mg_2018$DS_SIT_TOT_TURNO)


# ~~~~~~~~~~~~~~~~  AUTOMATION  ~~~~~~~~~~~~~~~~

automatic_analysis(all_dataframes)









