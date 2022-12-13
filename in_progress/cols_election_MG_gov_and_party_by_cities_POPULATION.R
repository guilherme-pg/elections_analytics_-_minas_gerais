# GOV AND PARTY VOTES BY CITIES POPULATION

# TO IMPROVE: set percents of the proportion from the State total amount of votes
# TO IMPROVE: increase the space between columns and increase the width of columns

# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais/ready_code")


library(dplyr)
library(ggplot2)



options(scipen=999)


# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

mg_2022 <- read.table("../dados/votacao_partido_munzona_2022_MG.csv", header=TRUE, sep=";")





# ~~~~~~~~~~~~~~~~  SELECT GOV AND LEGISLATIVE  ~~~~~~~~~~~~~~~~

df_gov <- mg_2022 %>%
  dplyr::filter(DS_CARGO == "Governador")

df_state <- mg_2022 %>%
  dplyr::filter(DS_CARGO == "Deputado Estadual")

df_federal <- mg_2022 %>%
  dplyr::filter(DS_CARGO == "Deputado Federal")


# group by city and party
df_gov_adjust <- df_gov %>%
  group_by(NM_MUNICIPIO, SG_PARTIDO) %>%
  summarise(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS))

# total by city
df_gov_total_by_mun <- df_gov %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS))



df_gov_total_by_mun <- df_gov_total_by_mun[order(df_gov_total_by_mun$QT_VOTOS_NOMINAIS_VALIDOS, decreasing=TRUE),]

# select the 15 most populous cities
df_gov_total_by_mun_15 <- df_gov_total_by_mun %>%
  head(15)

# select the 15 most populous cities on the df with party division
df_gov_party_by_votes <- df_gov_adjust %>%
  filter(NM_MUNICIPIO %in% df_gov_total_by_mun_15$NM_MUNICIPIO)
  
df_gov_party_by_votes <- df_gov_party_by_votes[order(df_gov_party_by_votes$QT_VOTOS_NOMINAIS_VALIDOS, decreasing=TRUE),]

# set total votes to party_by_votes
df_gov_party_by_votes$TOTAL_VOTES <- df_gov_total_by_mun_15$QT_VOTOS_NOMINAIS_VALIDOS[match(
  df_gov_party_by_votes$NM_MUNICIPIO, df_gov_total_by_mun_15$NM_MUNICIPIO
)]


# ~~~~~~~~~~~~~~~~  ADJUST NAMES AND SET TITLE CASE  ~~~~~~~~~~~~~~~~

df_gov_party_by_votes$SG_PARTIDO[df_gov_party_by_votes$SG_PARTIDO == "NOVO"] <- "Novo"

df_gov_party_by_votes$NM_MUNICIPIO <- stringr::str_to_title(df_gov_party_by_votes$NM_MUNICIPIO)
df_gov_party_by_votes$NM_MUNICIPIO[df_gov_party_by_votes$NM_MUNICIPIO == " DE "] <- " de "
df_gov_party_by_votes$NM_MUNICIPIO[df_gov_party_by_votes$NM_MUNICIPIO == " DAS "] <- " das "





# ~~~~~~~~~~~~~~~~  general variables  ~~~~~~~~~~~~~~~~

party_colores <- c(
  "Novo"="darkorange",
  "PSD"="aquamarine1",
  "PL"="darkblue",
  "PSDB"="royalblue",
  "PSOL"="gold",
  "PCB"="brown4",
  "PCO"="coral4",
  "PMB"="wheat",
  "PSTU"="firebrick4",
  "UP"="black"
)





# ~~~~~~~~~~~~~~~~  PLOT  ~~~~~~~~~~~~~~~~

plot <- df_gov_party_by_votes %>%
  ggplot(aes(
    x=QT_VOTOS_NOMINAIS_VALIDOS,
    y=forcats::fct_reorder(NM_MUNICIPIO, TOTAL_VOTES, .desc=FALSE),
    fill=SG_PARTIDO, 
  )) +
  geom_col(alpha=.6) +
  theme(
    plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
    plot.title = element_text(hjust=.5),
    plot.subtitle = element_text(hjust=.5, margin=margin(0,0,30,0)),
    plot.background = element_rect(fill="grey95", color=NA),
    panel.background = element_rect(fill ="grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey65", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    legend.title = element_blank(),
    legend.background = element_rect(fill="grey95")
  ) +
  scale_fill_manual(values = party_colores) +
  labs(title= "Votes in the 15 most populous cities in Minas Gerais",
       subtitle= "First round of the 2022 Governor election",
       x= "Amount of Votes") +
  scale_x_continuous(breaks=c(250000, 500000, 1000000, 1250000),
                     labels = c("250 thousand", 
                                "500 thousand", 
                                "1 million",
                                "1.25 million"))





# ~~~~~~~~~~~~~~~~  SAVE plot  ~~~~~~~~~~~~~~~~

ggsave(plot, filename="../saved_charts/cols_election_2022_MG_party_performance_by_population.jpg",
       units = "cm",
       width = 15,
       height = 10
)




