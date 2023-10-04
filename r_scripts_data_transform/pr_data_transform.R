
setwd("C:/Users/guilhermevmmpg/Documents/DEV/projetos/elections_analytics_-_minas_gerais")

Sys.setlocale("LC_ALL","English")

library(dplyr)





# ~~~~~~~~~~~~~~~~  LINKS FOR THE DATASETS  ~~~~~~~~~~~~~~~~ 

# https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2010_BR.zip
# https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2014_BR.zip
# https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2018_BR.zip
# https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2022_BR.zip





# ~~~~~~~~~~~~~~~~  DECLARING VARIABLES  ~~~~~~~~~~~~~~~~ 

dataframes_vectors_path <- c(
  "raw_data/votacao_secao_2010_BR.csv",
  "raw_data/votacao_secao_2014_BR.csv",
  "raw_data/votacao_secao_2018_BR.csv",
  "raw_data/votacao_secao_2022_BR.csv"
)

dataframe_name <- "data_processed/presidente_votacao_secao_2010_2022_BR.csv"

state = "MG"

selected_columns <- c("ANO_ELEICAO", 
                      "NR_TURNO", 
                      "SG_UF", 
                      "CD_MUNICIPIO", 
                      "NM_MUNICIPIO", 
                      "NR_ZONA", 
                      "NR_SECAO", 
                      "DS_CARGO", 
                      "NM_VOTAVEL", 
                      "QT_VOTOS")





# ~~~~~~~~~~~~~~~~  PROCESSING PRESIDENTE DATA  ~~~~~~~~~~~~~~~~ 

# Here the number of columns will be reduced and the datasets will be joined

for (x in 1:length(dataframes_vectors_path)) {
  print("loop total")
  
  # read the dataframe
  votes <- read.table(dataframes_vectors_path[x], header=TRUE, sep=";")
  
  # select the state
  votes_state <- votes[votes$SG_UF == state, ]
  
  # reduce the number of columns (only what is necessary)
  votes_state_columns_reduced <- votes_state[, selected_columns]
  
  # group by municipio, votavel, ano, turno
  votes_aggregated <- votes_state_columns_reduced %>%
    group_by(ANO_ELEICAO, NR_TURNO, NM_MUNICIPIO, NM_VOTAVEL) %>%
    summarise(TOTAL_VOTES = sum(QT_VOTOS)) %>%
    mutate(PERCENTAGE = (TOTAL_VOTES / sum(TOTAL_VOTES)) * 100)
  
  # join dataframes
  if (exists("votes_state_years")) {
    votes_state_years <- rbind(votes_state_years, votes_aggregated)
    print("loop if")
    
  } else {
    print("loop else")
    votes_state_years <- votes_aggregated
  }
}




# ~~~~~~~~~~~~~~~~ SAVE DATAFRAMES  ~~~~~~~~~~~~~~~~

write.csv(votes_state_years, dataframe_name, row.names = FALSE)


