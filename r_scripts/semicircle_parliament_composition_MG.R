# SEMICIRCLE PARLIAMENT COMPOSITION

# TO IMPROVE: FEDERAL PARLIAMENT SEMICIRCLE require reduce space between dots
# TO IMPROVE: change number of rows for federal and state parliaments
# TO IMPROVE: set ORDER to party by ideological alignment



# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais/ready_code")


library(dplyr)
library(ggplot2)
library(cowplot)
library(ggparliament)





# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ SELECT ONLY ELECTED

select_elected <- function(df) {
  
  situation <- c(
    "ELEITO ELEITO POR MÉDIA",
    "ELEITO POR QP",
    "ELEITO"
  )
  
  df_elected <- df %>%
    dplyr::filter(DS_SIT_TOT_TURNO %in% situation)
  
  return(df_elected)
}



# ~~~~ function ~~~~ change PARTIES NAMES  ~~~~~~~~~~~~~~~~

adjust_abbreviation_parties <- function(df) {
  
  df$SG_PARTIDO[df$SG_PARTIDO == "PFL"] <- "UNIÃO BRASIL"
  df$SG_PARTIDO[df$SG_PARTIDO == "DEM"] <- "UNIÃO BRASIL"
  df$SG_PARTIDO[df$SG_PARTIDO == "PSL"] <- "UNIÃO BRASIL"
  df$SG_PARTIDO[df$SG_PARTIDO == "UNIÃO"] <- "UNIÃO BRASIL"
  df$SG_PARTIDO[df$SG_PARTIDO == "PODE"] <- "PODEMOS"
  df$SG_PARTIDO[df$SG_PARTIDO == "PTN"] <- "PODEMOS"
  df$SG_PARTIDO[df$SG_PARTIDO == "PHS"] <- "PODEMOS"
  df$SG_PARTIDO[df$SG_PARTIDO == "PMDB"] <- "MDB"
  df$SG_PARTIDO[df$SG_PARTIDO == "PC do B"] <- "PCdoB"
  df$SG_PARTIDO[df$SG_PARTIDO == "PPL"] <- "PCdoB"
  df$SG_PARTIDO[df$SG_PARTIDO == "PPS"] <- "Cidadania"
  df$SG_PARTIDO[df$SG_PARTIDO == "PR"] <- "PL"
  df$SG_PARTIDO[df$SG_PARTIDO == "PRB"] <- "REPUBLICANOS"
  df$SG_PARTIDO[df$SG_PARTIDO == "PTC"] <- "AGIR"
  df$SG_PARTIDO[df$SG_PARTIDO == "PEN"] <- "Patriota"
  df$SG_PARTIDO[df$SG_PARTIDO == "PSDC"] <- "DC"
  df$SG_PARTIDO[df$SG_PARTIDO == "PT do B"] <- "AVANTE"
  
  df$SG_PARTIDO[df$SG_PARTIDO == "REPUBLICANOS"] <- "Republicanos"
  df$SG_PARTIDO[df$SG_PARTIDO == "UNIÃO BRASIL"] <- "União Brasil"
  df$SG_PARTIDO[df$SG_PARTIDO == "PATRIOTA"] <- "Patriota"
  df$SG_PARTIDO[df$SG_PARTIDO == "PATRI"] <- "Patriota"
  df$SG_PARTIDO[df$SG_PARTIDO == "SOLIDARIEDADE"] <- "Solidariedade"
  
  return(df)
}



# ~~~~ function ~~~~ FILTER STATE AND FEDERAL

select_federal_and_state <- function(df) {
  
  df_state <- df %>%
    filter(DS_CARGO %in% c("Deputado Estadual", "DEPUTADO ESTADUAL"))
  
  df_federal <- df %>%
    filter(DS_CARGO %in% c("Deputado Federal", "DEPUTADO FEDERAL"))
  
  df_state_federal <- list(df_federal, df_state)
  
  return(df_state_federal)
}



# ~~~~ function ~~~~ reduce variables and set seats

reduce_var_set_seats <- function(df) {
  
  elected_grouped <- df %>%
    group_by(NM_CANDIDATO, SG_PARTIDO, DS_SIT_TOT_TURNO) %>%
    summarise(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS))
  
  elected_grouped$SEATS <- 1
  
  grouped_df <- elected_grouped %>%
    group_by(SG_PARTIDO) %>%
    summarise(SEATS = sum(SEATS))
  
  return(grouped_df)
}



# ~~~~ function ~~~~ PLOT

generate_plot <- function(df) {
  
  parliament_data <- parliament_data(
    election_data = df,
    type = "semicircle",
    party_seats = df$SEATS,
    parl_rows=3,
    plot_order = df$SEATS,
  )
  
  plot <- parliament_data %>%
    ggplot(aes(x, y, colour= SG_PARTIDO)) +
    geom_parliament_seats(size = 3.5) +
    theme_ggparliament() +
    theme(
      plot.margin = margin(1, 1, .3, 1, "cm"),
      plot.title = element_text(margin = margin(0, 0, 30, 0)),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.margin=margin(.5, 0, 0, 0, unit='cm')
    ) +
    scale_colour_manual(values = parties_colores_2022) +
    labs(title = "Composição do Legislativo")
  
  return(plot)
}



# ~~~~ function ~~~~ SAVE

save_plot <- function(plot, df) {
  
  if (any(df$DS_CARGO %in% c("Deputado Federal", "DEPUTADO FEDERAL"))) { # zero length  !!!!!!!!!!!!!!!
    rep_type <- "federal"
    
  } else {
    rep_type <- "state"
  }
  
  file_name <- paste0("../saved_charts/semicircle_parliamment_composition_MG_representatives_", rep_type, ".jpg")
  
  ggsave(plot, filename=file_name,
         units = "cm",
         width = 15,
         height = 13
  )
}



# ~~~~ function ~~~~ AUTOMATION

automatic_analysis <- function(df) {
  
  # ~~~~~  SELECT only ELECTEDS  ~~~~~
  electeds <- select_elected(df)
  
  # ~~~~~  ADJUST PARTY NAMES  ~~~~~
  electeds_adjusted <- adjust_abbreviation_parties(electeds)
  
  # ~~~~~  SELECT ELECTEDS FEDERAL AND STATE DEPUTIES  ~~~~~
  df_federals_and_states <- select_federal_and_state(electeds_adjusted) # return list (federal, state)
  
  # ~~~~~  SELECT ELECTEDS FEDERAL AND STATE DEPUTIES  ~~~~~
  by_federal <- reduce_var_set_seats(df_federals_and_states[[1]])
  by_state <- reduce_var_set_seats(df_federals_and_states[[2]])
  
  # ~~~~~  PLOT  ~~~~~
  plot_federals <- generate_plot(by_federal)
  plot_state <- generate_plot(by_state)
  
  # ~~~~~  SAVE  ~~~~~
  save_plot(plot_federals, df_federals_and_states[[1]])
  save_plot(plot_state, df_federals_and_states[[2]])
}





# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

#mg_2010 <- read.table("../dados/votacao_candidato_munzona_2010_MG.csv", header=TRUE, sep=";")
#mg_2014 <- read.table("../dados/votacao_candidato_munzona_2014_MG.csv", header=TRUE, sep=";")
#mg_2018 <- read.table("../dados/votacao_candidato_munzona_2018_MG.csv", header=TRUE, sep=";")
mg_2022 <- read.table("../dados/votacao_candidato_munzona_2022_MG.csv", header=TRUE, sep=";")




# ~~~~~~~~~~~~~~~~  GENERAL VARIABLES  ~~~~~~~~~~~~~~~~
# TO IMPROVE: create a loop to check parties on the dataframe

parties_colores_2022 <- c(
  "PMN"="tomato4",
  "PSOL"="orange",
  "PV"="green",
  "PT"="red",
  "PDT"="firebrick",
  "PSB"="gold",
  "Rede"="darkseagreen",
  "Solidariedade"="darkorange4",
  "MDB"="grey",
  "PSD"="aquamarine1",
  "Cidadania"="darkorchid1",
  "PSDB"="royalblue",
  "AVANTE"="brown1",
  "PROS"="tan1",
  "PP"="tan4",
  "Patriota"="darkolivegreen1",
  "PSC"="cyan",
  "União Brasil"="deepskyblue",
  "NOVO"="darkorange",
  "PL"="darkblue",
  "Republicanos"="mediumblue",
  "DC"="dodgerblue"
)




# ~~~~~~~~~~~~~~~~  AUTOMATION  ~~~~~~~~~~~~~~~~

automatic_analysis(mg_2022)


