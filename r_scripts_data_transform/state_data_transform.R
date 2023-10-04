
setwd("C:/Users/guilhermevmmpg/Documents/DEV/projetos/elections_analytics_-_minas_gerais")

Sys.setlocale("LC_ALL","English")

library(dplyr)





# ~~~~~~~~~~~~~~~~  LINKS FOR THE DATASETS  ~~~~~~~~~~~~~~~~ 
df_gov <- read.table("data_processed/governador_votacao_secao_2010_2022.csv", header=TRUE, sep=",")
df_sen <- read.table("data_processed/senador_votacao_secao_2010_2022.csv", header=TRUE, sep=",")
df_df <- read.table("data_processed/df_votacao_secao_2010_2022.csv", header=TRUE, sep=",")
df_de <- read.table("data_processed/de_votacao_secao_2010_2022.csv", header=TRUE, sep=",")




# ~~~~~~~~~~~~~~~~  DECLARING VARIABLES  ~~~~~~~~~~~~~~~~ 

# path for importing data
dataframes_vectors_path <- c(
  "raw_data/votacao_secao_2010_MG.csv",
  "raw_data/votacao_secao_2014_MG.csv",
  "raw_data/votacao_secao_2018_MG.csv",
  "raw_data/votacao_secao_2022_MG.csv"
)

dataframe_names <- c(
  "data_processed/governador_votacao_secao_2010_2022.csv", 
  "data_processed/senador_votacao_secao_2010_2022.csv", 
  "data_processed/df_votacao_secao_2010_2022.csv", 
  "data_processed/de_votacao_secao_2010_2022.csv"
)

# state = "MG"

cargos = c(
  "Governador",
  "Senador",
  "Deputado Federal",
  "Deputado Estadual"
)

selected_columns <- c(
  "ANO_ELEICAO", 
  "NR_TURNO", 
  "SG_UF", 
  "CD_MUNICIPIO", 
  "NM_MUNICIPIO", 
  "DS_CARGO", 
  "NR_VOTAVEL",
  "NM_VOTAVEL",
  "QT_VOTOS"
)




# ~~~~~~~~~~~~~~~~  PROCESSING, SENADOR, DF & DE GOVERNADOR DATA  ~~~~~~~~~~~~~~~~ 

# Here the number of columns will be reduced and the dataframes will be joined

for (y in 1:length(cargos)) {
  
  for (x in 1:length(dataframes_vectors_path)) {
    
    # read the dataframe
    df_votes <- read.table(dataframes_vectors_path[x], header=TRUE, sep=";")
    
    # select by state
    # votes_state <- df_votes[df_votes$SG_UF == state, ]
    
    # select by cargo
    votes_state_cargo <- df_votes[df_votes$DS_CARGO == cargos[y], ]
    
    # reduce the number of columns (only what is necessary)
    votes_state_columns_reduced <- votes_state_cargo %>%
      select(selected_columns)
    
    # group by municipio, votavel, ano, turno
    votes_aggregated <- votes_state_columns_reduced %>%
      group_by(ANO_ELEICAO, NR_TURNO, NM_MUNICIPIO, NM_VOTAVEL) %>%
      summarise(TOTAL_VOTES = sum(QT_VOTOS)) %>%
      mutate(PERCENTAGE = (TOTAL_VOTES / sum(TOTAL_VOTES)) * 100)
    
    # join dataframes
    if (exists("votes_state_years")) {
      votes_state_years <- rbind(votes_state_years, votes_aggregated)
      
    } else {
      votes_state_years <- votes_aggregated
    }
  }
  
  # save dataframe
  write.csv(votes_state_years, dataframe_names[y], row.names = FALSE)
}





