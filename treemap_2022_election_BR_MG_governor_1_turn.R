# 2022 ELECTION BR MINAS GERAIS BARS GOVERNOR X PRESIDENT


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais")


library(dplyr)
library(ggplot2)
library(treemapify)


options(scipen=999)


# general data
VOTES_state <- read.table("dados/votacao_secao_2022_MG.csv", header=TRUE, sep=";")


# select only votes for governor
governor <- filter(VOTES_state, DS_CARGO=="GOVERNADOR")







# VOTES BY CITY & CANDIDATE
gov_votes_cities <- governor %>%
  group_by(CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))







# ~~~~~~~~~~~~~~  TOTAL VOTES BY MUNICIPALITY  ~~~~~~~~~~~~~~

# TOTAL VOTES BY CITY
gov_total_votes_cities <- governor %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
gov_votes_cities$TOTAL_VOTES <- gov_total_votes_cities$QT_VOTOS[ match(gov_votes_cities$NM_MUNICIPIO, gov_total_votes_cities$NM_MUNICIPIO)  ]

# ADD PROPORTION
gov_votes_cities$PROPORTION_TL <- gov_votes_cities$QT_VOTOS/ gov_votes_cities$TOTAL_VOTES

# ADD PERCENT
gov_votes_cities$PERCENT_TL <- gov_votes_cities$PROPORTION_TL *100

# FORMAT PERCENT
gov_votes_cities$PERCENT_TL <- paste0(sprintf("%4.2f", gov_votes_cities$PERCENT_TL), "%")










# ~~~~~~~~~~~~~~  VALID VOTES BY MUNICIPALITY  ~~~~~~~~~~~~~~

gov_valid_votes <- gov_votes_cities %>%
  filter(NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")

# TOTAL VALID VOTES BY CITY
gov_total_valid_votes_cities <- gov_valid_votes %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
gov_valid_votes$TOTAL_VALID <- gov_total_valid_votes_cities$QT_VOTOS[ match(gov_valid_votes$NM_MUNICIPIO, gov_total_valid_votes_cities$NM_MUNICIPIO)  ]

# ADD PROPORTION
gov_valid_votes$PROPORTION_TVV <- gov_valid_votes$QT_VOTOS/ gov_valid_votes$TOTAL_VALID

# ADD PERCENT
gov_valid_votes$PERCENT_TVV <- gov_valid_votes$PROPORTION_TVV *100

# FORMAT PERCENT
gov_valid_votes$PERCENT_TVV <- paste0(sprintf("%4.2f", gov_valid_votes$PERCENT_TVV), "%")








# ~~~~~~~~~~~~~~  CHECK EACH MOST VOTED CANDIDATE IN EACH MUNICIPALITY  ~~~~~~~~~~~~~~

most_gov_voted_each_city <- gov_valid_votes %>%
  group_by(NM_MUNICIPIO) %>%
  slice_max(PROPORTION_TVV)






# ~~~~~~~~~~~~~~  plot variables  ~~~~~~~~~~~~~~

candidate_gov_colors <- c(
  "ALEXANDRE KALIL"="seagreen3",
  "CARLOS ALBERTO DIAS VIANA"="blue4",
  "INDIRA IVANISE XAVIER"="grey44",
  "LORENE FIGUEIREDO DE OLIVEIRA"="gold",
  "LOURDES FRANCISCO DA COSTA"="grey44",
  "MARCUS VINÍCIUS CAETANO PESTANA DA SILVA"="mediumpurple",
  "PAULO TRISTAO PINTO"="grey44",
  "RENATA REGINA DE ABREU RODRIGUES"="grey44",
  "ROMEU ZEMA NETO"="darkorange",
  "VANESSA PORTUGAL BARBOSA"="grey44"
)

candidate_pr_colors <- c(
  "LUIZ INÁCIO LULA DA SILVA"="red",
  "JAIR MESSIAS BOLSONARO"="blue4"
)






# ~~~~~~~~~~~~~~  SET TITLE CASE FOR CITY NAMES  ~~~~~~~~~~~~~~

most_gov_voted_each_city$NM_MUNICIPIO <- stringr::str_to_title(most_gov_voted_each_city$NM_MUNICIPIO)
most_gov_voted_each_city$NM_MUNICIPIO <- gsub(" E ", " e ", most_gov_voted_each_city$NM_MUNICIPIO)
most_gov_voted_each_city$NM_MUNICIPIO <- gsub(" De ", " de ", most_gov_voted_each_city$NM_MUNICIPIO)
most_gov_voted_each_city$NM_MUNICIPIO <- gsub(" Da ", " da ", most_gov_voted_each_city$NM_MUNICIPIO)
most_gov_voted_each_city$NM_MUNICIPIO <- gsub(" Do ", " do ", most_gov_voted_each_city$NM_MUNICIPIO)
most_gov_voted_each_city$NM_MUNICIPIO <- gsub(" Dos ", " dos ", most_gov_voted_each_city$NM_MUNICIPIO)
most_gov_voted_each_city$NM_MUNICIPIO <- gsub(" Das ", " das ", most_gov_voted_each_city$NM_MUNICIPIO)






# ~~~~~~~~~~~~~~  TREEMAP PLOT by GOV  ~~~~~~~~~~~~~~

most_gov_voted_each_city %>%
  ggplot(aes(area=TOTAL_VOTES, fill=NM_VOTAVEL, label=NM_MUNICIPIO), color="black") +
  geom_treemap() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = candidate_gov_colors
  ) +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)




# SAVE LAST PLOT
ggsave("treemap_2022_election_BR_MG_governor_1_turn_v1.jpg",
       units = "cm",
       width = 15,
       height = 15
)












# ~~~~~~~~~~~~~~  PRESIDENT ELECTION IN MG  ~~~~~~~~~~~~~~

# TO DIVIDE:
# BOLSO + ZEMA
# LULA +ZEMA
# BOLSO + KALIL
# LULA + KALIL


VOTES_BR <- read.table("dados/votacao_secao_2022_BR.csv", header=TRUE, sep=";")


# select only votes for governor
votes_pr <- filter(VOTES_BR, SG_UF=="MG")


# VOTES BY CITY & CANDIDATE
pr_votes_cities <- votes_pr %>%
  group_by(CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))









# ~~~~~~~~~~~~~~  TOTAL VOTES BY MUNICIPALITY  ~~~~~~~~~~~~~~

# TOTAL VOTES BY CITY
pr_total_votes_cities <- pr_votes_cities %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
pr_votes_cities$TOTAL_VOTES <- pr_total_votes_cities$QT_VOTOS[ match(pr_votes_cities$NM_MUNICIPIO, pr_total_votes_cities$NM_MUNICIPIO)  ]

# ADD PROPORTION
pr_votes_cities$PROPORTION_TL <- pr_votes_cities$QT_VOTOS/ pr_votes_cities$TOTAL_VOTES

# ADD PERCENT
pr_votes_cities$PERCENT_TL <- pr_votes_cities$PROPORTION_TL *100

# FORMAT PERCENT
pr_votes_cities$PERCENT_TL <- paste0(sprintf("%4.2f", pr_votes_cities$PERCENT_TL), "%")










# ~~~~~~~~~~~~~~  VALID VOTES BY MUNICIPALITY  ~~~~~~~~~~~~~~

pr_valid_votes <- pr_votes_cities %>%
  filter(NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")

# TOTAL VALID VOTES BY CITY
pr_total_valid_votes_cities <- pr_valid_votes %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS = sum(QT_VOTOS))

# ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
pr_valid_votes$TOTAL_VALID <- pr_total_valid_votes_cities$QT_VOTOS[ match(pr_valid_votes$NM_MUNICIPIO, pr_total_valid_votes_cities$NM_MUNICIPIO)  ]

# ADD PROPORTION
pr_valid_votes$PROPORTION_TVV <- pr_valid_votes$QT_VOTOS/ pr_valid_votes$TOTAL_VALID

# ADD PERCENT
pr_valid_votes$PERCENT_TVV <- pr_valid_votes$PROPORTION_TVV *100

# FORMAT PERCENT
pr_valid_votes$PERCENT_TVV <- paste0(sprintf("%4.2f", pr_valid_votes$PERCENT_TVV), "%")







# ~~~~~~~~~~~~~~  CHECK EACH MOST VOTED CANDIDATE IN EACH MUNICIPALITY  ~~~~~~~~~~~~~~

most_pr_voted_each_city <- pr_valid_votes %>%
  group_by(NM_MUNICIPIO) %>%
  slice_max(PROPORTION_TVV)







# ~~~~~~~~~~~~~~  SET TITLE CASE FOR CITY NAMES  ~~~~~~~~~~~~~~

most_pr_voted_each_city$NM_MUNICIPIO <- stringr::str_to_title(most_pr_voted_each_city$NM_MUNICIPIO)
most_pr_voted_each_city$NM_MUNICIPIO <- gsub(" E ", " e ", most_pr_voted_each_city$NM_MUNICIPIO)
most_pr_voted_each_city$NM_MUNICIPIO <- gsub(" De ", " de ", most_pr_voted_each_city$NM_MUNICIPIO)
most_pr_voted_each_city$NM_MUNICIPIO <- gsub(" Da ", " da ", most_pr_voted_each_city$NM_MUNICIPIO)
most_pr_voted_each_city$NM_MUNICIPIO <- gsub(" Do ", " do ", most_pr_voted_each_city$NM_MUNICIPIO)
most_pr_voted_each_city$NM_MUNICIPIO <- gsub(" Dos ", " dos ", most_pr_voted_each_city$NM_MUNICIPIO)
most_pr_voted_each_city$NM_MUNICIPIO <- gsub(" Das ", " das ", most_pr_voted_each_city$NM_MUNICIPIO)







# ~~~~~~~~~~~~~~  TREEMAP PLOT by PR  ~~~~~~~~~~~~~~

most_pr_voted_each_city %>%
  ggplot(aes(area=TOTAL_VOTES, fill=NM_VOTAVEL, label=NM_MUNICIPIO), color="black") +
  geom_treemap() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = candidate_pr_colors
  ) +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15
                    )




# SAVE LAST PLOT
ggsave("treemap_2022_election_BR_MG_governor_1_turn_v2.jpg",
       units = "cm",
       width = 15,
       height = 15
)







# ~~~~~~~~~~~~~~  CROSS VOTES PRESIDENTS AND GOVERNORS  ~~~~~~~~~~~~~~

pr_valid_votes$TOTAL_VALID <- pr_total_valid_votes_cities$QT_VOTOS[ match(pr_valid_votes$NM_MUNICIPIO, pr_total_valid_votes_cities$NM_MUNICIPIO)  ]

pr_x_gov <- select(most_pr_voted_each_city, NM_MUNICIPIO, NM_VOTAVEL, TOTAL_VOTES)

pr_x_gov$NM_GOV_VOTAVEL <- most_gov_voted_each_city$NM_VOTAVEL[ match(pr_x_gov$NM_MUNICIPIO, most_gov_voted_each_city$NM_MUNICIPIO)  ]

pr_x_gov$PR_X_GOV <- NA




pr_x_gov$PR_X_GOV[pr_x_gov$NM_VOTAVEL=="LUIZ INÁCIO LULA DA SILVA" & pr_x_gov$NM_GOV_VOTAVEL=="ROMEU ZEMA NETO" ] <- "LULA e ZEMA"
pr_x_gov$PR_X_GOV[pr_x_gov$NM_VOTAVEL=="LUIZ INÁCIO LULA DA SILVA" & pr_x_gov$NM_GOV_VOTAVEL=="ALEXANDRE KALIL" ] <- "LULA e KALIL"
pr_x_gov$PR_X_GOV[pr_x_gov$NM_VOTAVEL=="JAIR MESSIAS BOLSONARO" & pr_x_gov$NM_GOV_VOTAVEL=="ROMEU ZEMA NETO" ] <- "BOLSONARO e ZEMA"
pr_x_gov$PR_X_GOV[pr_x_gov$NM_VOTAVEL=="LUIZ INÁCIO LULA DA SILVA" & pr_x_gov$NM_GOV_VOTAVEL=="CARLOS ALBERTO DIAS VIANA" ] <- "LULA e VIANA"


total_pr_x_gov <- pr_x_gov %>%
  group_by(PR_X_GOV) %>%
  summarise(TOTAL_VOTES = sum(TOTAL_VOTES))


total_pr_x_gov %>%
  ggplot(aes(area=TOTAL_VOTES, fill=PR_X_GOV, label=PR_X_GOV), color="black") +
  geom_treemap() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = candidate_pr_colors
  ) +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15
  )

table(total_pr_x_gov$PR_X_GOV)


# TO : COMPARE LASTS (10, 14, 18) ELECTIONS WITH PRESENT ELECTION 1 turn
# TO : governor and president
# TO : TOTAL VALID VS TOTAL VOTES
# TO : PARLIAMENT DIAGRAM - ggparliament

# TREEMAP FOR CITY DISTRIBUTION (POPULATION)

# WAFFLE CHART for legislative distribution (LEGISLATIVE)

# STACKED AREA CHART FOR ALL CITIES VOTES (GOV)



