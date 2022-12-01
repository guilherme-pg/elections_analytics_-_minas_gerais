# CORRELATION VOTES PRESIDENT X GOVERNADOR IN DECISIVE BRAZILIAN STATES

# TO IMPROVE: IN PROGRESS - automation
# TO IMPROVE: set a loop for each dataset automatically
# TO IMPROVE: 



# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais/in_progress")


library(dplyr)
library(ggplot2)
library(cowplot)


options(scipen=999)





# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ SET PROPORTIONS AND PERCENTS

set_proportions_and_percents <- function(df_votes) {
  
  votes_df <- df_votes %>%
    group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))

  # GROUP VOTES BY MUNICIPALITY to check the total votes
  total_votos_mun <- votes_df %>%
    group_by(NM_MUNICIPIO) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
  votes_df$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(votes_df$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]
  
  # ADD PROPORTION
  votes_df$PROPORTION <- votes_df$QT_VOTOS/ votes_df$TOTAL_VOTOS
  
  # ADD PERCENT
  votes_df$PERCENT <- votes_df$PROPORTION *100
  
  # FORMAT PERCENT
  votes_df$PERCENT_FORMAT <- paste0(sprintf("%4.2f", votes_df$PERCENT), "%")
  
  if ("PRESIDENTE" %in% df_votes$DS_CARGO) {
    colnames(votes_df)[colnames(votes_df) == "PERCENT"] <- "PERCENT_PR"
    
  } else {
    colnames(votes_df)[colnames(votes_df) == "PERCENT"] <- "PERCENT_GOV"
  }
  
  return(votes_df)
}



# ~~~~ function ~~~~ SELECT ELECTION ROUND
election_round_selection <- function(df){
  
  round_1 <- df %>%
    filter(NR_TURNO == 1)
  
  round_2 <- data.frame()
  
  if (2 %in% df$NR_TURNO){
    round_2 <- df %>%
      filter(NR_TURNO == 2)
  }
  
  two_rounds <- list(round_1, round_2)
  
  return(two_rounds)
}




# ~~~~ function ~~~~ MERGE DATASETS: GOV AND PR

merge_gov_and_pr <- function(pr_df, gov_df, pr_candidate, gov_candidate){
  
  # select ONLY PRESIDENT VOTES BY CITY
  pr_candidate_df <- filter(pr_df, NM_VOTAVEL==pr_candidate)
  
  # select ONLY GOVERNOR VOTES BY CITY
  gov_candidate_df <- filter(gov_df, NM_VOTAVEL==gov_candidate)
  
  GOV_X_PRE <- merge(pr_candidate_df[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], gov_candidate_df[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)
  
  return(GOV_X_PRE)
}




# ~~~~ function ~~~~ PLOT MAP AND SAVE

plot_scatter <- function(merged_GOV_x_PR, labels_by_year){
  
  if ("Lula" %in% PERCENT_PR) {
    candidate_pr_color = "red"
    n = 1
  } else {
    candidate_pr_color = "blue"
    n = 2
  }
  
  merged_GOV_x_PR %>%
    ggplot() +
    geom_point(aes(x=PERCENT_GOV, 
                   y=PERCENT_PR, 
                   size=TOTAL_VOTOS), 
               alpha=0.3, 
               colour=candidate_pr_color) +
    geom_abline(slope = 1, intercept =0, lwd=.5, colour="grey35", linetype='dashed') +
    lims(x=c(0, 100), y=c(0, 100)) +
    theme(
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill="grey95", color=NA),
      plot.title = element_text(vjust=3, hjust=.5),
      plot.subtitle = element_text(vjust=3, hjust=.5),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(vjust=-2),
      axis.title.y = element_text(vjust=3),
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    labs(x= labels_by_year[[3]][n],  # CUSTOMIZE
         y= candidate_pr,   # CUSTOMIZE
    ) +
    scale_size_continuous(range=c(.5, 10),
                          breaks = c(25000, 100000, 500000, 1500000),
                          labels = c("25 mil", "100 mil", "500 mill", "1.5 milhões")
    )
}






# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

#VOTES_MG_2010 <- read.table("../dados/pr_votes_2010_mg.csv", header=TRUE, sep=",")
#VOTES_BR_2010 <- read.table("../dados/votacao_secao_2010_BR.csv", header=TRUE, sep=";")

#VOTES_MG_2014 <- read.table("../dados/pr_votes_2014_mg.csv", header=TRUE, sep=",")
#VOTES_BR_2014 <- read.table("../dados/votacao_secao_2014_BR.csv", header=TRUE, sep=";")

#VOTES_MG_2018 <- read.table("../dados/pr_votes_2018_mg.csv", header=TRUE, sep=",")
#VOTES_BR_2018 <- read.table("../dados/votacao_secao_2014_BR.csv", header=TRUE, sep=";")

VOTES_MG_2022 <- read.table("../dados/pr_votes_2022_mg.csv", header=TRUE, sep=",")
VOTES_BR_2022 <- read.table("../dados/votacao_secao_2022_BR.csv", header=TRUE, sep=",")





# ~~~~~~~~~~~~~~~~  PREVIOUS DATASETS ADJUSTMENTS  ~~~~~~~~~~~~~~~~

# CUSTOM: SELECT STATE
mg_pr_2022 <- filter(VOTES_BR_2022, SG_UF=="MG")
mg_gov_2022 <- filter(VOTES_MG_2022, DS_CARGO=="GOVERNADOR")





#  ~~~~~~~~~~~~~~~~ CUSTOMIZE CANDIDATES  ~~~~~~~~~~~~~~~~ GOVERNOR

# labels_2010 <- list(
#   2010,
#   "1st round of the 2010 election in Minas Gerais, Brazil",
#   c("JOSÉ SERRA", "DILMA VANA ROUSSEFF"),
#   c("candidato gov", "candidato gov"),
#   c("color1", "color2")
# )

# labels_2014 <- list(
#   2014,
#   "1st round of the 2014 election in Minas Gerais, Brazil",
#   c("AÉCIO NEVES DA CUNHA", "DILMA VANA ROUSSEFF"),
#   c("candidato gov", "candidato gov"),
#   c("color1", "color2")
# )

# labels_2018 <- list(
#   2018,
#   "1st round of the 2018 election in Minas Gerais, Brazil",
#   c("JAIR MESSIAS BOLSONARO", "FERNANDO HADDAD"),
#   c("candidato gov", "candidato gov"),
#   c("color1", "color2")
# )

labels_2022 <- list(
  2022,
  "1st round of the 2022 election in Minas Gerais, Brazil",
  c("Bolsonaro", "Lula"),
  c("Kalil", "Zema"),
  c("red", "blue")
)





# ~~~~~~~~~~~~~~~~  distinguish first from second round  ~~~~~~~~~~~~~~~~

two_round_pr_list <- election_round_selection(mg_pr_2022)

mg_pr_2022_1r <- two_round_pr_list[[1]]
if (2 %in% mg_pr_2022$NR_TURNO){
  mg_pr_2022_2r <- two_round_pr_list[[2]]
}


two_round_gov_list <- election_round_selection(mg_gov_2022)

mg_gov_2022_1r <- two_round_gov_list[[1]]
if (2 %in% mg_gov_2022$NR_TURNO){
  mg_gov_2022_2r <- two_round_gov_list[[2]]
}






# ~~~~~~~~~~~~~~~~  APPLY PROPORTION AND PERCENTS  ~~~~~~~~~~~~~~~~

votes_mg_pr_2022_1r <- set_proportions_and_percents(mg_pr_2022_1r)
if (2 %in% mg_pr_2022$NR_TURNO){
  votes_mg_pr_2022_2r <- set_proportions_and_percents(mg_pr_2022_2r)
}


votes_mg_gov_2022_1r <- set_proportions_and_percents(mg_gov_2022_1r)
if (2 %in% mg_gov_2022$NR_TURNO){
  votes_mg_gov_2022_2r <- set_proportions_and_percents(mg_gov_2022_2r)
}





# ~~~~~~~~~~~~~~~~  ADJUST CANDIDATES NAMES  ~~~~~~~~~~~~~~~~
votes_mg_pr_2022_1r$NM_VOTAVEL[votes_mg_pr_2022_1r$NM_VOTAVEL == "LUIZ INÁCIO LULA DA SILVA"] <- "Lula"
votes_mg_pr_2022_1r$NM_VOTAVEL[votes_mg_pr_2022_1r$NM_VOTAVEL == "JAIR MESSIAS BOLSONARO"] <- "Bolsonaro"

votes_mg_gov_2022_1r$NM_VOTAVEL[votes_mg_gov_2022_1r$NM_VOTAVEL == "ALEXANDRE KALIL"] <- "Kalil"
votes_mg_gov_2022_1r$NM_VOTAVEL[votes_mg_gov_2022_1r$NM_VOTAVEL == "ROMEU ZEMA NETO"] <- "Zema"





# ~~~~~~~~~~~~~~~~  MERGE GOV AND PRESIDENT DATASETS  ~~~~~~~~~~~~~~~~
#  require : diferenciar 1 e 2 turno, caso tenha 2 turno
GOV_x_PR <- merge_gov_and_pr(
  votes_mg_pr_2022_1r,
  votes_mg_gov_2022_1r,
  "Lula",
  "Zema"
)





# ~~~~~~~~~~~~~~~~  PLOT THE STATE and SAVE  ~~~~~~~~~~~~~~~~

plot_scatter(GOV_x_PR, labels_2022)



















####################################### to delete
president_state_by_city <- president_state %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- president_state_by_city %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))
# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
president_state_by_city$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(president_state_by_city$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]
# ADD PROPORTION
president_state_by_city$PROPORCAO <- president_state_by_city$QT_VOTOS/ president_state_by_city$TOTAL_VOTOS
# ADD PERCENT
president_state_by_city$PERCENT <- president_state_by_city$PROPORCAO *100
# FORMAT PERCENT
president_state_by_city$PERCENT_FORMAT <- paste0(sprintf("%4.2f", president_state_by_city$PERCENT), "%")
colnames(president_state_by_city)[colnames(president_state_by_city) == "PERCENT"] <- "PERCENT_PR"

governador <- filter(VOTES_state, DS_CARGO=="GOVERNADOR")

governador_by_city <- governador %>%
  group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))
# GROUP VOTES BY MUNICIPALITY
total_votos_mun <- governador_by_city %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))
# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
governador_by_city$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(governador_by_city$NM_MUNICIPIO, total_votos_mun$NM_MUNICIPIO)  ]
# ADD PROPORTION
governador_by_city$PROPORCAO <- governador_by_city$QT_VOTOS/ governador_by_city$TOTAL_VOTOS
# ADD PERCENT
governador_by_city$PERCENT <- governador_by_city$PROPORCAO *100
# FORMAT PERCENT
governador_by_city$PERCENT_FORMAT <- paste0(sprintf("%4.2f", governador_by_city$PERCENT), "%")
colnames(governador_by_city)[colnames(governador_by_city) == "PERCENT"] <- "PERCENT_GOV"




# CUSTOMIZE CANDIDATES  ~~~~~~~~~~~~ PRESIDENT

candidate_pr = "Bolsonaro"
candidate_gov = "Zema"
candidate_pr_color = "deepskyblue3"

# select ONLY PRESIDENT VOTES BY CITY
pr_candidate <- filter(president_state_by_city, NM_VOTAVEL=="JAIR MESSIAS BOLSONARO")

# select ONLY GOVERNOR VOTES BY CITY
gov_candidate <- filter(governador_by_city, NM_VOTAVEL=="ROMEU ZEMA NETO")


GOV_X_PRE <- merge(pr_candidate[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], gov_candidate[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)


bol_x_ze <- GOV_X_PRE %>%
  ggplot() +
  geom_point(aes(x=PERCENT_GOV, y=PERCENT_PR, size=TOTAL_VOTOS), alpha=0.3, colour=candidate_pr_color) +
  geom_abline(slope = 1, intercept =0, lwd=.5, colour="grey35", linetype='dashed') +
  lims(x=c(0, 100), y=c(0, 100)) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill="grey95", color=NA),
    plot.title = element_text(vjust=3, hjust=.5),
    plot.subtitle = element_text(vjust=3, hjust=.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust=3),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(x= candidate_gov,  # CUSTOMIZE
       y= candidate_pr,   # CUSTOMIZE
       ) +
  scale_size_continuous(range=c(.5, 10),
                        breaks = c(25000, 100000, 500000, 1500000),
                        labels = c("25 mil", "100 mil", "500 mill", "1.5 milhões")
                        )



candidate_pr = "Lula"
candidate_gov = "Zema"
candidate_pr_color = "red"
pr_candidate <- filter(president_state_by_city, NM_VOTAVEL=="LUIZ INÁCIO LULA DA SILVA")

rm(GOV_XPRE)
GOV_X_PRE <- merge(pr_candidate[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], gov_candidate[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)


lula_x_ze <- GOV_X_PRE %>%
  ggplot() +
  geom_point(aes(x=PERCENT_GOV, y=PERCENT_PR, size=TOTAL_VOTOS), alpha=0.3, colour=candidate_pr_color) +
  geom_abline(slope = 1, intercept =0, lwd=.5, colour="grey35", linetype='dashed') +
  lims(x=c(0, 100), y=c(0, 100)) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill="grey95", color=NA),
    plot.title = element_text(vjust=3, hjust=.5),
    plot.subtitle = element_text(vjust=3, hjust=.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust=3),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(x= candidate_gov,  # CUSTOMIZE
       y= candidate_pr,   # CUSTOMIZE
  ) +
  scale_size_continuous(range=c(.5, 10),
                        breaks = c(25000, 100000, 500000, 1500000),
                        labels = c("25 mil", "100 mil", "500 mill", "1.5 milhões")
  )









#  ~~~~~~~~~~~~~~~~ CUSTOMIZE CANDIDATES  ~~~~~~~~~~~~~~~~ GOVERNOR




candidate_pr = "Bolsonaro"
candidate_gov = "Kalil"
candidate_pr_color = "deepskyblue3"

# select ONLY PRESIDENT VOTES BY CITY
pr_candidate <- filter(president_state_by_city, NM_VOTAVEL=="JAIR MESSIAS BOLSONARO")


# select ONLY GOVERNOR VOTES BY CITY
gov_candidate <- filter(governador_by_city, NM_VOTAVEL=="ALEXANDRE KALIL")



GOV_X_PRE <- merge(pr_candidate[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], gov_candidate[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)




bol_x_kl <- GOV_X_PRE %>%
  ggplot() +
  geom_point(aes(x=PERCENT_GOV, y=PERCENT_PR, size=TOTAL_VOTOS), alpha=0.3, colour=candidate_pr_color) +
  geom_abline(slope = 1, intercept =0, lwd=.5, colour="grey35", linetype='dashed') +
  lims(x=c(0, 100), y=c(0, 100)) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill="grey95", color=NA),
    plot.title = element_text(vjust=3, hjust=.5),
    plot.subtitle = element_text(vjust=3, hjust=.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust=3),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(x= candidate_gov,  # CUSTOMIZE
       y= candidate_pr,   # CUSTOMIZE
  ) +
  scale_size_continuous(range=c(.5, 10),
                        breaks = c(25000, 100000, 500000, 1500000),
                        labels = c("25 mil", "100 mil", "500 mill", "1.5 milhões")
  )



candidate_pr = "Lula"
candidate_gov = "Kalil"
candidate_pr_color = "red"
pr_candidate <- filter(president_state_by_city, NM_VOTAVEL=="LUIZ INÁCIO LULA DA SILVA")

rm(GOV_XPRE)
GOV_X_PRE <- merge(pr_candidate[, c("NM_MUNICIPIO", "PERCENT_PR", "TOTAL_VOTOS")], gov_candidate[, c("NM_MUNICIPIO", "PERCENT_GOV")], ALL=TRUE)


lula_x_kl <- GOV_X_PRE %>%
  ggplot() +
  geom_point(aes(x=PERCENT_GOV, y=PERCENT_PR, size=TOTAL_VOTOS), alpha=0.3, colour=candidate_pr_color) +
  geom_abline(slope = 1, intercept =0, lwd=.5, colour="grey35", linetype='dashed') +
  lims(x=c(0, 100), y=c(0, 100)) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill="grey95", color=NA),
    plot.title = element_text(vjust=3, hjust=.5),
    plot.subtitle = element_text(vjust=3, hjust=.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust=3),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(x= candidate_gov,  # CUSTOMIZE
       y= candidate_pr,   # CUSTOMIZE
  ) +
  scale_size_continuous(range=c(.5, 10),
                        breaks = c(25000, 100000, 500000, 1500000),
                        labels = c("25 mil", "100 mil", "500 mill", "1.5 milhões")
  )





# ~~~~~~~~~~~~~~~~  DASHBOARD: 4 PLOTS  ~~~~~~~~~~~~~~~~
grid_plots <- plot_grid(bol_x_ze,
  lula_x_ze,
  bol_x_kl,
  lula_x_kl,
  ncol = 2, nrow = 2
)



#  ~~~~~~~~~~~~~~~~  TITLES AND SUBTITLES TO MULTIPLE PLOTS  ~~~~~~~~~~~~~~~~
grid_plots_title <- ggdraw() +
  draw_label(
    "Correlation between votes for Governor and President",
    fontface="bold"
  ) +
  theme(
    plot.background = element_rect(fill="grey95", color=NA),
    plot.margin = margin(0, 0, -8, 0)
  )

grid_plots_subtitle <- ggdraw() +
  draw_label(
    "1st round of the 2022 election in Minas Gerais, Brazil"
  ) +
  theme(
    plot.background = element_rect(fill="grey95", color=NA),
    plot.margin = margin(-8, 0, 0, 0)
  )


plot_grid(
  grid_plots_title,
  grid_plots_subtitle,
  grid_plots,
  ncol=1,
  rel_heights = c(0.1, 0.1, 1)
)





# REQUIRE: increase the size of the dots
# OPTION: CHANGE dot line to negative correlations plots



# SAVE LAST PLOT
ggsave("scatter_2022_election_brazilian_MG_correlation_president_x_governador_1_turn_v2.jpg",
       units = "cm",
       width = 15,
       height = 15
       )




