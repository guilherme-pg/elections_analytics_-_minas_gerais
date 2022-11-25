# total votes vs valid votes




# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais")


library(dplyr)
library(ggplot2)


options(scipen=999)


# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

pr_votes_2010_mg <- read.table("dados/pr_votes_2010_mg.csv", header=TRUE, sep=",")
pr_votes_2014_mg <- read.table("dados/pr_votes_2014_mg.csv", header=TRUE, sep=",")
pr_votes_2018_mg <- read.table("dados/pr_votes_2018_mg.csv", header=TRUE, sep=",")

pr_votes_2010_br <- read.table("dados/votacao_secao_2010_BR.csv", header=TRUE, sep=";")
pr_votes_2014_br <- read.table("dados/votacao_secao_2014_BR.csv", header=TRUE, sep=";")
pr_votes_2018_br <- read.table("dados/votacao_secao_2018_BR.csv", header=TRUE, sep=";")






# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ DATAFRAMES SELECTIONS

select_second_turn <- function(df) {
  df_2_turn <- df %>%
    filter(NR_TURNO == 2)
  
  df_to_return <- df_2_turn %>%
    group_by(CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL, ANO_ELEICAO, DS_CARGO, SG_UF) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  return(df_to_return)
}



# ~~~~ function ~~~~ SET PROPORTIONS AND PERCENTS BY TOTAL

set_total_votes_proportions_and_percents <- function(df) {
  # GROUP VOTES BY MUNICIPALITY to check the total votes
  total_votos_mun <- df %>%
    group_by(CD_MUNICIPIO) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
  df$TOTAL_VOTES <- total_votos_mun$QT_VOTOS[ match(df$CD_MUNICIPIO, total_votos_mun$CD_MUNICIPIO)  ]
  
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
  df_valid_votes <- filter(df, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")
  
  df_valid_votes <- select(df_valid_votes, CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL, ANO_ELEICAO, DS_CARGO, QT_VOTOS)
  
  # GROUP TOTAL VOTES BY MUNICIPALITY
  total_votos_val <- df_valid_votes %>%
    group_by(CD_MUNICIPIO) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # MATCH counties 
  df_valid_votes$TOTAL_VOTES <- total_votos_val$QT_VOTOS[ match(df_valid_votes$CD_MUNICIPIO, total_votos_val$CD_MUNICIPIO) ]
  
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

pr_votes_2010_mg_2_turn <- select_second_turn(pr_votes_2010_mg)
pr_votes_2014_mg_2_turn <- select_second_turn(pr_votes_2014_mg)
pr_votes_2018_mg_2_turn <- select_second_turn(pr_votes_2018_mg)

pr_votes_2010_br_2_turn <- select_second_turn(pr_votes_2010_br)
pr_votes_2014_br_2_turn <- select_second_turn(pr_votes_2014_br)
pr_votes_2018_br_2_turn <- select_second_turn(pr_votes_2018_br)





# ~~~~~~~~~~~~~~~~  SET PROPORTION AND PERCENT from TOTAL VOTES  ~~~~~~~~~~~~~~~~

tv_pr_votes_2010_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2010_mg_2_turn)
tv_pr_votes_2014_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2014_mg_2_turn)
tv_pr_votes_2018_mg_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2018_mg_2_turn)

tv_pr_votes_2010_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2010_br_2_turn)
tv_pr_votes_2014_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2014_br_2_turn)
tv_pr_votes_2018_br_2_turn <- set_total_votes_proportions_and_percents(pr_votes_2018_br_2_turn)





# ~~~~~~~~~~~~~~~~  SET PROPORTION AND PERCENT from VALID VOTES  ~~~~~~~~~~~~~~~~

vv_pr_votes_2010_mg_2_turn <- select_valid_votes(pr_votes_2010_mg_2_turn)
vv_pr_votes_2014_mg_2_turn <- select_valid_votes(pr_votes_2014_mg_2_turn)
vv_pr_votes_2018_mg_2_turn <- select_valid_votes(pr_votes_2018_mg_2_turn)

vv_pr_votes_2010_br_2_turn <- select_valid_votes(pr_votes_2010_br_2_turn)
vv_pr_votes_2014_br_2_turn <- select_valid_votes(pr_votes_2014_br_2_turn)
vv_pr_votes_2018_br_2_turn <- select_valid_votes(pr_votes_2018_br_2_turn)









# ~~~~~~~~~~~~~~~~  PLOT DIFFERENCE: VALID x TOTAL VOTES  ~~~~~~~~~~~~~~~~

tv_pr_grouped_br <- tv_pr_votes_2014_br_2_turn %>%
  group_by(NM_VOTAVEL, VOTES_MEASURE) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

tv_pr_grouped_br$TOTAL_VOTES <- sum(tv_pr_grouped_br$QT_VOTOS)
tv_pr_grouped_br$PROPORTION <- tv_pr_grouped_br$QT_VOTOS/ tv_pr_grouped_br$TOTAL_VOTES
tv_pr_grouped_br$PERCENT <- tv_pr_grouped_br$PROPORTION *100
tv_pr_grouped_br$PERCENT_FORMAT <- paste0(sprintf("%4.2f", tv_pr_grouped_br$PERCENT), "%")


vv_pr_grouped_br <- vv_pr_votes_2014_br_2_turn %>%
  group_by(NM_VOTAVEL, VOTES_MEASURE) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

vv_pr_grouped_br$TOTAL_VOTES <- sum(vv_pr_grouped_br$QT_VOTOS)
vv_pr_grouped_br$PROPORTION <- vv_pr_grouped_br$QT_VOTOS/ vv_pr_grouped_br$TOTAL_VOTES
vv_pr_grouped_br$PERCENT <- vv_pr_grouped_br$PROPORTION *100
vv_pr_grouped_br$PERCENT_FORMAT <- paste0(sprintf("%4.2f", vv_pr_grouped_br$PERCENT), "%")

compare_pr_votes_2014_br_2_turn <- rbind(tv_pr_grouped_br,
                                         vv_pr_grouped_br)

compare_pr_votes_2014_br_2_turn$NM_VOTAVEL[compare_pr_votes_2014_br_2_turn$NM_VOTAVEL == "AÉCIO NEVES DA CUNHA"] <- "AÉCIO NEVES"
compare_pr_votes_2014_br_2_turn$NM_VOTAVEL[compare_pr_votes_2014_br_2_turn$NM_VOTAVEL == "DILMA VANA ROUSSEFF"] <- "DILMA ROUSSEFF"

compare_pr_votes_2014_br_2_turn$NM_VOTAVEL <- stringr::str_to_title(compare_pr_votes_2014_br_2_turn$NM_VOTAVEL)

comparasion_colores <- c(
  "Aécio Neves"="royalblue",
  "Dilma Rousseff"="firebrick",
  "Voto Nulo"="grey55",
  "Voto Branco"="grey77"
)



compare_pr_votes_2014_br_2_turn %>%
  ggplot(aes(x=forcats::fct_reorder(NM_VOTAVEL, PERCENT, .desc=TRUE), y=PERCENT, fill=NM_VOTAVEL)) +
  geom_col(
    position = position_dodge(0.9)
  ) +
  facet_grid(~VOTES_MEASURE, scales="free_x") +
  geom_text(
    aes(label=PERCENT_FORMAT), 
    position = position_dodge(0.9),
    color="black",vjust =-1, size=3
  ) +
  theme(
    plot.background = element_rect(fill="grey95", color=NA),
    plot.margin = margin(.5,.5,.5,.5, "cm"),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    panel.background = element_rect(colour = "grey95", fill=NA),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour="grey66", linetype="dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_manual(
    values=comparasion_colores
  ) +
  labs(
    title="Difference in percentage of Total Votes and Valid Votes",
    subtitle="Votes for President in the second round of 2014"
  ) +
  lims(y=c(0, 60))

# REQUIRE: reduce "valid votes" column width 

ggsave("cols_election_BR_total_votes_X_valid_votes.jpg",
       units = "cm",
       width = 15,
       height = 10
)












