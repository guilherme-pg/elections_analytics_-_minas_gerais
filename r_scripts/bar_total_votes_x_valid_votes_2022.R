# COLS - TOTAL VOTES MG x BR


# SET MAIN DIRECTORY
setwd("C:/Users/guilhermevmmpg/Documents/DEV/projetos/elections_analytics_-_minas_gerais")

Sys.setlocale("LC_ALL","English")

library(dplyr)
library(ggplot2)


options(scipen=999)


# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

pr_votes_2010_ufs <- read.table("raw_data/votacao_secao_2010_BR.csv", header=TRUE, sep=";")
pr_votes_2014_ufs <- read.table("raw_data/votacao_secao_2014_BR.csv", header=TRUE, sep=";")
pr_votes_2018_ufs <- read.table("raw_data/votacao_secao_2018_BR.csv", header=TRUE, sep=";")
pr_votes_2022_ufs <- read.table("raw_data/votacao_secao_2022_BR.csv", header=TRUE, sep=";")





# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ set br df column sg_uf as 'br'
separate_mg_from_br <- function(df) {
  
  df_mg <- df %>%
    filter(SG_UF == 'MG')
  df_mg$SG_UF <- "Minas Gerais"
  
  df_br <- df %>%
    group_by(NM_VOTAVEL, ANO_ELEICAO, DS_CARGO) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  df_br$SG_UF <- "Brasil"
  
  df_br_and_mg <- list(df_br, df_mg)
  
  return(df_br_and_mg)
}


# ~~~~ function ~~~~ DATAFRAMES SELECTIONS

select_second_turn <- function(df) {
  df_2_turn <- df %>%
    filter(NR_TURNO == 2)
  
  df_to_return <- df_2_turn %>%
    group_by(NM_VOTAVEL, ANO_ELEICAO, DS_CARGO, SG_UF) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  return(df_to_return)
}



# ~~~~ function ~~~~ SET PROPORTIONS AND PERCENTS BY TOTAL

set_total_votes_proportions_and_percents <- function(df) {
  # GROUP VOTES BY MUNICIPALITY to check the total votes
  total_votos_mun <- df %>%
    group_by(NM_VOTAVEL) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
  df$TOTAL_VOTES <- sum(df$QT_VOTOS)
  
  # ADD PROPORTION
  df$PROPORTION <- df$QT_VOTOS/ df$TOTAL_VOTES
  
  # ADD PERCENT
  df$PERCENT <- df$PROPORTION *100
  
  # FORMAT PERCENT
  df$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df$PERCENT), "%")
  
  df$VOTES_MEASURE <- "Total Votes"
  
  return(df)
}





# ~~~~ function ~~~~ SET VALID VOTES AND.. PROPORTIONS AND PERCENTS

select_valid_votes <- function(df) {
  
  # FILTER BY VALID VOTES
  df_valid_votes <- filter(df, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO" & NM_VOTAVEL != "VOTO ANULADO E APURADO EM SEPARADO")
  
  #df_valid_votes <- select(df_valid_votes, CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL, ANO_ELEICAO, DS_CARGO, QT_VOTOS)
  
  # GROUP TOTAL VOTES BY MUNICIPALITY
  total_votos_val <- df_valid_votes %>%
    group_by(NM_VOTAVEL) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # MATCH counties ????????????WW
  df_valid_votes$TOTAL_VOTES <- sum(df_valid_votes$QT_VOTOS)
  
  # ADD PROPORTION
  df_valid_votes$PROPORTION <- df_valid_votes$QT_VOTOS / df_valid_votes$TOTAL_VOTES
  
  # ADD PERCENT
  df_valid_votes$PERCENT <- df_valid_votes$PROPORTION *100
  
  # FORMAT PERCENT
  df_valid_votes$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_valid_votes$PERCENT), "%")
  
  df_valid_votes$VOTES_MEASURE <- "Valid Votes"
  
  return(df_valid_votes)
  
}




# ~~~~~~~~~~~~~~~~  SELECT ONLY SECOND TURN  ~~~~~~~~~~~~~~~~

pr_votes_2010_all_2_turn <- select_second_turn(pr_votes_2010_ufs)
pr_votes_2014_all_2_turn <- select_second_turn(pr_votes_2014_ufs)
pr_votes_2018_all_2_turn <- select_second_turn(pr_votes_2018_ufs)
pr_votes_2022_all_2_turn <- select_second_turn(pr_votes_2022_ufs)



# ~~~~~~~~~~~~~~~~  separate MG and group the total for BR  ~~~~~~~~~~~~~~~~

list_2010_br_and_mg <- separate_mg_from_br(pr_votes_2010_all_2_turn)
pr_votes_2010_br <- list_2010_br_and_mg[[1]]
pr_votes_2010_mg <- list_2010_br_and_mg[[2]]

list_2014_br_and_mg <- separate_mg_from_br(pr_votes_2014_all_2_turn)
pr_votes_2014_br <- list_2014_br_and_mg[[1]]
pr_votes_2014_mg <- list_2014_br_and_mg[[2]]

list_2018_br_and_mg <- separate_mg_from_br(pr_votes_2018_all_2_turn)
pr_votes_2018_br <- list_2018_br_and_mg[[1]]
pr_votes_2018_mg <- list_2018_br_and_mg[[2]]

list_2022_br_and_mg <- separate_mg_from_br(pr_votes_2022_all_2_turn)
pr_votes_2022_br <- list_2022_br_and_mg[[1]]
pr_votes_2022_mg <- list_2022_br_and_mg[[2]]



# ~~~~~~~~~~~~~~~~  SET PROPORTION AND PERCENT from TOTAL VOTES  ~~~~~~~~~~~~~~~~

# tv_pr_votes_2010_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2010_mg)
# tv_pr_votes_2014_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2014_mg)
# tv_pr_votes_2018_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2018_mg)
# tv_pr_votes_2022_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2022_mg)

# tv_pr_votes_2010_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2010_br)
# tv_pr_votes_2014_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2014_br)
# tv_pr_votes_2018_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2018_br)
# tv_pr_votes_2022_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2022_br)



# ~~~~~~~~~~~~~~~~  SET PROPORTION AND PERCENT from VALID VOTES  ~~~~~~~~~~~~~~~~

vv_pr_votes_2010_mg_2_turn <- select_valid_votes(pr_votes_2010_mg)
vv_pr_votes_2014_mg_2_turn <- select_valid_votes(pr_votes_2014_mg)
vv_pr_votes_2018_mg_2_turn <- select_valid_votes(pr_votes_2018_mg)
vv_pr_votes_2022_mg_2_turn <- select_valid_votes(pr_votes_2022_mg)

vv_pr_votes_2010_br_2_turn <- select_valid_votes(pr_votes_2010_br)
vv_pr_votes_2014_br_2_turn <- select_valid_votes(pr_votes_2014_br)
vv_pr_votes_2018_br_2_turn <- select_valid_votes(pr_votes_2018_br)
vv_pr_votes_2022_br_2_turn <- select_valid_votes(pr_votes_2022_br)




# ~~~~~~~~~~~~~~~~  MERGE DATAFRAMES  ~~~~~~~~~~~~~~~~

pr_vv_2_turn_merged <- rbind(
  vv_pr_votes_2010_mg_2_turn,
  vv_pr_votes_2014_mg_2_turn,
  vv_pr_votes_2018_mg_2_turn,
  vv_pr_votes_2022_mg_2_turn,
  vv_pr_votes_2010_br_2_turn,
  vv_pr_votes_2014_br_2_turn,
  vv_pr_votes_2018_br_2_turn,
  vv_pr_votes_2022_br_2_turn
)





# ~~~~~~~~~~~~~~~~  SET PARTY COLUMNS  ~~~~~~~~~~~~~~~~

# set party according candidate
pr_vv_2_turn_merged$PARTY <- NA

pr_vv_2_turn_merged$PARTY[ pr_vv_2_turn_merged$NM_VOTAVEL == "AÉCIO NEVES DA CUNHA" ] <- "PSDB"
pr_vv_2_turn_merged$PARTY[ pr_vv_2_turn_merged$NM_VOTAVEL == "JOSÉ SERRA" ] <- "PSDB"
pr_vv_2_turn_merged$PARTY[ pr_vv_2_turn_merged$NM_VOTAVEL == "DILMA VANA ROUSSEFF" ] <- "PT"
pr_vv_2_turn_merged$PARTY[ pr_vv_2_turn_merged$NM_VOTAVEL == "FERNANDO HADDAD" ] <- "PT"
pr_vv_2_turn_merged$PARTY[ pr_vv_2_turn_merged$NM_VOTAVEL == "JAIR MESSIAS BOLSONARO" ] <- "PL"
pr_vv_2_turn_merged$PARTY[ pr_vv_2_turn_merged$NM_VOTAVEL == "LUIZ INÁCIO LULA DA SILVA" ] <- "PT"





# ~~~~~~~~~~~~~~~~  general dataset adjusts  ~~~~~~~~~~~~~~~~

# change and convert names to title case
pr_vv_2_turn_merged$NM_VOTAVEL[ pr_vv_2_turn_merged$NM_VOTAVEL == "AÉCIO NEVES DA CUNHA" ] <- "Aécio Neves"
pr_vv_2_turn_merged$NM_VOTAVEL[ pr_vv_2_turn_merged$NM_VOTAVEL == "JOSÉ SERRA" ] <- "José Serra"
pr_vv_2_turn_merged$NM_VOTAVEL[ pr_vv_2_turn_merged$NM_VOTAVEL == "DILMA VANA ROUSSEFF" ] <- "Dilma Rousseff"
pr_vv_2_turn_merged$NM_VOTAVEL[ pr_vv_2_turn_merged$NM_VOTAVEL == "FERNANDO HADDAD" ] <- "Fernando Haddad"
pr_vv_2_turn_merged$NM_VOTAVEL[ pr_vv_2_turn_merged$NM_VOTAVEL == "JAIR MESSIAS BOLSONARO" ] <- "Jair Bolsonaro"
pr_vv_2_turn_merged$NM_VOTAVEL[ pr_vv_2_turn_merged$NM_VOTAVEL == "LUIZ INÁCIO LULA DA SILVA" ] <- "Lula"



# set order to candidates names
pr_vv_2_turn_merged$NM_VOTAVEL <- factor(pr_vv_2_turn_merged$NM_VOTAVEL, levels=c(
  "Dilma Rousseff",
  "Fernando Haddad",
  "Lula",
  "Aécio Neves",
  "José Serra",
  "Jair Bolsonaro"
))

# pr_vv_2_turn_merged$SG_UF[pr_vv_2_turn_merged$SG_UF=="MG"] <- "Minas Gerais"




# ~~~~~~~~~~~~~~~~  PLOT GENERAL VARIABLES  ~~~~~~~~~~~~~~~~

party_colors <- c(
  "José Serra"="royalblue",
  "Aécio Neves"="royalblue",
  "Dilma Rousseff"="firebrick",
  "Fernando Haddad"="firebrick",
  "Jair Bolsonaro"="blue",
  "Lula"="firebrick"
)








# ~~~~~~~~~~~~~~~~  PLOT THE STATE and SAVE  ~~~~~~~~~~~~~~~~

# require: add 2022 data

pr_vv_2_turn_merged %>%
  ggplot(aes(x=NM_VOTAVEL,
             y=PERCENT, 
             fill=NM_VOTAVEL)) +
  geom_col() +
  geom_text(aes(label=PERCENT_FORMAT),
            vjust =-1) +
  facet_grid(vars(SG_UF), vars(ANO_ELEICAO), scales = "free_x") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(margin = margin(0, 0, 20, 0)),
    plot.background = element_rect(fill="grey95", color=NA),
    panel.grid.major = element_line(colour = "grey80", linetype='dashed'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle=45, vjust=1.1, hjust=1.1),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(values = party_colors) +
  labs(
    title="Valid votes for President in the 2nd round: Brazil X Minas Gerais"
  ) +
  lims(
    y=c(0, 75)
       )
  
  

ggsave("saved_charts/cols_election_BR_valid_votes_br_x_minas_gerais.jpg",
       units = "cm",
       width = 15,
       height = 10
)
  


