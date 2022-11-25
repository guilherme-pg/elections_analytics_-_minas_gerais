# 2022 map ELECTION BR MINAS GERAIS GOVERNOR X PRESIDENT


# 2018 presidential vote database
# https://dadosabertos.tse.jus.br/dataset/resultados-2018/resource/f6feee37-4913-4d83-b73b-dc25ba51e2e6

# maps dataset organizacao_do_territorio > malhas_territoriais > malhas_municipais >
# municipios_2018 > UFs > MG, RJ and SP > mg_municipios, rj_municipios and sp_municipios
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais")

library(dplyr)
library(ggplot2)
library(sf)




# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

# VOTES
pr_votes_2010_mg <- read.table("dados/pr_votes_2010_mg.csv", header=TRUE, sep=",")
pr_votes_2014_mg <- read.table("dados/pr_votes_2014_mg.csv", header=TRUE, sep=",")
pr_votes_2018_mg <- read.table("dados/pr_votes_2018_mg.csv", header=TRUE, sep=",")

# MAPS
MAP_2010 <- st_read("dados/2010_MG_31MUE250GC_SIR.shp", options= "ENCODING=WINDOWS-1252")
MAP_2014 <- st_read("dados/2014_MG_31MUE250GC_SIR.shp")
MAP_2018 <- st_read("dados/2018_MG_31MUE250GC_SIR.shp")




# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ ADJUST MAP DATAFRAME

adjust_map_df <- function(df_map) {
  df_map <- rename(df_map, "NM_MUNICIPIO"="NM_MUNICIP")
  
  df_map$SG_UF <- "MG"
  
  return(df_map) 
}



# ~~~~ function ~~~~ DATAFRAMES SELECTIONS

dataframe_select_simplification <- function(pr_votes_2r_mg) {
  #FILTER BY ELECTIONS 2 ROUND
  votes_state <- pr_votes_2r_mg %>%
    filter(NR_TURNO==2)
  
  votos_pr_2 <- votes_state %>%
    group_by(CD_MUNICIPIO, NM_VOTAVEL, NM_MUNICIPIO, SG_UF) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  return(votos_pr_2)
}



# ~~~~ function ~~~~ SET PROPORTIONS AND PERCENTS

set_proportions_and_percents <- function(votos_pr_2) {
  # GROUP VOTES BY MUNICIPALITY to check the total votes
  total_votos_mun <- votos_pr_2 %>%
    group_by(CD_MUNICIPIO) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
  votos_pr_2$TOTAL_VOTOS <- total_votos_mun$QT_VOTOS[ match(votos_pr_2$CD_MUNICIPIO, total_votos_mun$CD_MUNICIPIO)  ]
  
  # ADD PROPORTION
  votos_pr_2$PROPORTION <- votos_pr_2$QT_VOTOS/ votos_pr_2$TOTAL_VOTOS
  
  # ADD PERCENT
  votos_pr_2$PERCENT <- votos_pr_2$PROPORTION *100
  
  # FORMAT PERCENT
  votos_pr_2$PERCENT_FORMAT <- paste0(sprintf("%4.2f", votos_pr_2$PERCENT), "%")
  
  return(votos_pr_2)
}



# ~~~~ function ~~~~ SET VALID VOTES AND.. PROPORTIONS AND PERCENTS

select_valid_votes <- function(votos_estados) {
  
  # FILTER BY VALID VOTES
  df_votos_validos <- filter(votos_estados, NM_VOTAVEL != "VOTO NULO" & NM_VOTAVEL != "VOTO BRANCO")
  
  df_votos_validos <- select(df_votos_validos, CD_MUNICIPIO, NM_MUNICIPIO, NM_VOTAVEL, QT_VOTOS, geometry)
  
  # GROUP TOTAL VOTES BY MUNICIPALITY
  total_votos_val <- df_votos_validos %>%
    group_by(CD_MUNICIPIO) %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  
  # MATCH counties 
  df_votos_validos$TOTAL_VOTOS <- total_votos_val$QT_VOTOS[ match(df_votos_validos$CD_MUNICIPIO, total_votos_val$CD_MUNICIPIO) ]
  
  # ADD PROPORTION
  df_votos_validos$PROPORTION <- df_votos_validos$QT_VOTOS / df_votos_validos$TOTAL_VOTOS
  
  # ADD PERCENT
  df_votos_validos$PERCENT <- df_votos_validos$PROPORTION *100
  
  # FORMAT PERCENT
  df_votos_validos$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df_votos_validos$PERCENT), "%")
  
  return(df_votos_validos)
  
}



# ~~~~ function ~~~~ SELECTING MOST VOTED (WINNER) AND ADJUST FOR THE PLOT

select_winner_adjusts <- function(df_votos_validos, labels_year){
  
  # DISTINGUISH THE WINNER OF EACH MUNICIPALTY BY THE MAJORITY OF THE VALID VOTES
  votos_validos <- df_votos_validos %>%
    group_by(NM_MUNICIPIO) %>%
    slice_max(PERCENT)
  
  # TRANSFORM THE VALUES OF A VARIABLE (INVERTING) TO DISTINGUISH BY OPPOSITE COLORS
  # APPLY THE TRANSFORMATION BY COLUMN ACCORDING CONDITION
  votos_validos <- votos_validos %>%
    mutate(PERCENT_TRANSF = abs(case_when(NM_VOTAVEL==labels_year[[2]][1] ~ PERCENT-100, NM_VOTAVEL==labels_year[[2]][2] ~ PERCENT)))
  
  return(votos_validos)
}



# ~~~~ function ~~~~ PLOT MAP AND SAVE

plot_save_map <- function(votos_validos, labels_year){
  
  # PLOT THE PROPORTIONS OF EACH MUNICIPALY BY CANDIDATE 
  map_plot<- votos_validos %>%
    ggplot() +
    geom_sf(aes(geometry=geometry, fill=PERCENT_TRANSF), 
            color=NA) +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "black", fill=NA),
      plot.background = element_rect(fill="black", color=NA),
      plot.margin = margin(t=10, l=100, r=100, b=10),
      plot.title = element_text(hjust=.5, colour = "white"),
      plot.subtitle = element_text(hjust=.5, colour= "white"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(colour = "white"),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(.4, "cm"),
      legend.background = element_blank()
    ) +
    labs(
      title = stringr::str_interp("Valid Votes in the 2nd Round of the ${labels_year[[1]]} Presidential Elections"),
      subtitle = "Candidate performance by municipality in percentage") +
    scale_fill_gradientn( colors= coloresBlueRed, 
                          limits=c(0, 100),
                          breaks=c(0, 25, 50, 75, 100),
                          labels=labels_year[[3]]
    ) +
    guides(fill = guide_colourbar(ticks = FALSE, 
                                  title.position = "bottom")
    )
  
  ggsave(map_plot, file=stringr::str_interp("map_${labels_year[[1]]}_election_BR_MG_votes_territorial_distribution.jpg"),
         units = "cm",
         width = 15,
         height = 10
  )
}







# ~~~~~~~~~~~~~~~~  PLOT GENERAL VARIABLES  ~~~~~~~~~~~~~~~~

# DISTINGUISH AND IDENTIFY BY COLOR THE GRADES OF THE CANDIDATES PERCENTS
coloresBlueRed <- c(
  "#003467",
  "#0c539c",
  "#1176e0",
  "#4392e9",
  "#82c1ff",
  "grey91",
  "#ff8282",
  "#e94343",
  "#e01111",
  "#9c0c0c",
  "#6a0000"
)
breaks <- c(0, 20, 30, 40, 50, 60, 70, 80, 100)

labels_2010 <- list(2010, 
                    c("JOSÉ SERRA", "DILMA VANA ROUSSEFF"),
                    c("100%", "José Serra", "50%", "Dilma Rousseff", "100%")
)
labels_2014 <- list(2014, 
                    c("AÉCIO NEVES DA CUNHA", "DILMA VANA ROUSSEFF"),
                    c("100%", "Aécio Neves", "50%", "Dilma Rousseff", "100%")
)
labels_2018 <- list(2018, 
                    c("JAIR MESSIAS BOLSONARO", "FERNANDO HADDAD"),
                    c("100%", "Jair Bolsonaro", "50%", "Fernando Haddad", "100%")
)




# ~~~~~~~~~~~~~~~~  MAP ADJUSTS  ~~~~~~~~~~~~~~~~

MAP_2010 <- adjust_map_df(MAP_2010)
MAP_2014 <- adjust_map_df(MAP_2014)
MAP_2018 <- adjust_map_df(MAP_2018)




# ~~~~~~~~~~~~~~~~  DATAFRAMES ADJUSTS  ~~~~~~~~~~~~~~~~

pr_votes_2010_mg <- dataframe_select_simplification(pr_votes_2010_mg)
pr_votes_2014_mg <- dataframe_select_simplification(pr_votes_2014_mg)
pr_votes_2018_mg <- dataframe_select_simplification(pr_votes_2018_mg)




# ~~~~~~~~~~~~~~~~  SET PROPORTION AND PERCENT  ~~~~~~~~~~~~~~~~

pr_votes_2010_mg <- set_proportions_and_percents(pr_votes_2010_mg)
pr_votes_2014_mg <- set_proportions_and_percents(pr_votes_2014_mg)
pr_votes_2018_mg <- set_proportions_and_percents(pr_votes_2018_mg)




# ~~~~~~~~~~~~~~~~  SET TITLE CASE  ~~~~~~~~~~~~~~~~ workaround... REQUIRE REFORMULATION !!!

# MUNICIPALITIES NAMES CORRECTIONS
MAP_2010$NM_MUNICIPIO <- gsub("-D'Á", "-D'Á", MAP_2010$NM_MUNICIPIO)
MAP_2010$NM_MUNICIPIO <- gsub("PASSA-VINTE", "PASSA VINTE", MAP_2010$NM_MUNICIPIO)
MAP_2010$NM_MUNICIPIO <- gsub("PINGO D'ÁGUA", "PINGO-D'ÁGUA", MAP_2010$NM_MUNICIPIO)
MAP_2010$NM_MUNICIPIO <- gsub("BRASÓPOLIS", "BRAZÓPOLIS", MAP_2010$NM_MUNICIPIO)

MAP_2014$NM_MUNICIPIO <- gsub("-D'Á", "-D'Á", MAP_2014$NM_MUNICIPIO)
MAP_2014$NM_MUNICIPIO <- gsub("PASSA-VINTE", "PASSA VINTE", MAP_2014$NM_MUNICIPIO)
MAP_2014$NM_MUNICIPIO <- gsub("PINGO D'ÁGUA", "PINGO-D'ÁGUA", MAP_2014$NM_MUNICIPIO)

MAP_2018$NM_MUNICIPIO <- gsub("-D'Á", "-D'Á", MAP_2018$NM_MUNICIPIO)
MAP_2018$NM_MUNICIPIO <- gsub("PINGO D'ÁGUA", "PINGO-D'ÁGUA", MAP_2018$NM_MUNICIPIO)


# workaround : some cities here are from another state...
pr_votes_2010_mg$NM_MUNICIPIO <- gsub(" D O", " D'O", pr_votes_2010_mg$NM_MUNICIPIO)
pr_votes_2010_mg$NM_MUNICIPIO <- gsub("PAU D ALHO", "PAU-D'ALHO", pr_votes_2010_mg$NM_MUNICIPIO)
pr_votes_2010_mg$NM_MUNICIPIO <- gsub("SÃO LUÍS DO PARAITINGA", "SÃO LUIZ DO PARAITINGA", pr_votes_2010_mg$NM_MUNICIPIO)
pr_votes_2010_mg$NM_MUNICIPIO <- gsub(" D ", "-D'", pr_votes_2010_mg$NM_MUNICIPIO)
pr_votes_2010_mg$NM_MUNICIPIO <- gsub("SEM PEIXE", "SEM-PEIXE", pr_votes_2010_mg$NM_MUNICIPIO)

pr_votes_2014_mg$NM_MUNICIPIO <- gsub(" D O", " D'O", pr_votes_2014_mg$NM_MUNICIPIO)
pr_votes_2014_mg$NM_MUNICIPIO <- gsub("PAU D ALHO", "PAU-D'ALHO", pr_votes_2014_mg$NM_MUNICIPIO)
pr_votes_2014_mg$NM_MUNICIPIO <- gsub(" D ", "-D'", pr_votes_2014_mg$NM_MUNICIPIO)
pr_votes_2014_mg$NM_MUNICIPIO <- gsub("SÃO LUÍS DO PARAITINGA", "SÃO LUIZ DO PARAITINGA", pr_votes_2014_mg$NM_MUNICIPIO)
pr_votes_2014_mg$NM_MUNICIPIO <- gsub("SEM PEIXE", "SEM-PEIXE", pr_votes_2014_mg$NM_MUNICIPIO)

pr_votes_2018_mg$NM_MUNICIPIO <- gsub(" D O", " D'O", pr_votes_2018_mg$NM_MUNICIPIO)
pr_votes_2018_mg$NM_MUNICIPIO <- gsub("PAU D ALHO", "PAU-D'ALHO", pr_votes_2018_mg$NM_MUNICIPIO)
pr_votes_2018_mg$NM_MUNICIPIO <- gsub(" D ", "-D'", pr_votes_2018_mg$NM_MUNICIPIO)
pr_votes_2018_mg$NM_MUNICIPIO <- gsub("SÃO LUÍS DO PARAITINGA", "SÃO LUIZ DO PARAITINGA", pr_votes_2018_mg$NM_MUNICIPIO)
pr_votes_2018_mg$NM_MUNICIPIO <- gsub("SEM PEIXE", "SEM-PEIXE", pr_votes_2018_mg$NM_MUNICIPIO)




# ~~~~~~~~~~~~~~~~  MERGE VOTES WITH MAPS  ~~~~~~~~~~~~~~~~

votes_2010 <- merge(pr_votes_2010_mg, MAP_2010)
votes_2014 <- merge(pr_votes_2014_mg, MAP_2014)
votes_2018 <- merge(pr_votes_2018_mg, MAP_2018)




# ~~~~~~~~~~~~~~~~  SELECT VALID VOTES: PROPORTION AND PERCENT  ~~~~~~~~~~~~~~~~

valid_votes_2010 <- select_valid_votes(votes_2010)
valid_votes_2014 <- select_valid_votes(votes_2014)
valid_votes_2018 <- select_valid_votes(votes_2018)




# ~~~~~~~~~~~~~~~~  PLOT ADJUSTS  ~~~~~~~~~~~~~~~~

adjusted_votes_2010 <- select_winner_adjusts(valid_votes_2010, labels_2010)
adjusted_votes_2014 <- select_winner_adjusts(valid_votes_2014, labels_2014)
adjusted_votes_2018 <- select_winner_adjusts(valid_votes_2018, labels_2018)




# ~~~~~~~~~~~~~~~~  PLOT THE STATE and SAVE  ~~~~~~~~~~~~~~~~

plot_save_map(adjusted_votes_2010, labels_2010)
plot_save_map(adjusted_votes_2014, labels_2014)
plot_save_map(adjusted_votes_2018, labels_2018)





