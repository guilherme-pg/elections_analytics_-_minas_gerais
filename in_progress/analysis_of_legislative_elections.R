# analysis of legislative elections

# TO IMPROVE: require smooth the plot, there are empty spaces


# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais/ready_code")


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

parties_colores <- c(
  "PMN"="tomato4",
  "PCO"="coral4",
  "PSTU"="firebrick4",
  "PCB"="brown4",
  "PCdoB"="red4",
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
  "PRP"="slategray1",
  "PROS"="tan1",
  "PP"="tan4",
  "PTC"="lightskyblue1",
  "PRTB"="chartreuse4",
  "Patriota"="darkolivegreen1",
  "PHS"="cadetblue2",
  "PSC"="cyan",
  "União Brasil"="deepskyblue",
  "NOVO"="darkorange",
  "PL"="darkblue",
  "PTB"="darkslategrey",
  "Republicanos"="mediumblue",
  "PSDC"="dodgerblue"
)



# ~~~~~~~~~~~~~~~~  FUNCTIONS  ~~~~~~~~~~~~~~~~

# ~~~~ function ~~~~ REDUCE VARIABLES  ~~~~~~~~~~~~~~~~

reduce_variables <- function(df) {
  
  if (any(df$ANO_ELEICAO  %in% c(2018, 2022))) {
    df_selected <- df %>%
      select(ANO_ELEICAO, NR_TURNO, SG_UF, NM_UE, CD_MUNICIPIO,
             NM_MUNICIPIO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
             SG_PARTIDO, NM_PARTIDO, QT_VOTOS_NOMINAIS_VALIDOS,
             DS_SIT_TOT_TURNO)
    
  } else {
    
    df_selected <- df %>%
      select(ANO_ELEICAO, NR_TURNO, SG_UF, NM_UE, CD_MUNICIPIO,
             NM_MUNICIPIO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO,
             SG_PARTIDO, NM_PARTIDO, QT_VOTOS_NOMINAIS,
             DS_SIT_TOT_TURNO)
  }
  
  return(df_selected)
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
  df$SG_PARTIDO[df$SG_PARTIDO == "PT do B"] <- "AVANTE"
  
  df$SG_PARTIDO[df$SG_PARTIDO == "REPUBLICANOS"] <- "Republicanos"
  df$SG_PARTIDO[df$SG_PARTIDO == "UNIÃO BRASIL"] <- "União Brasil"
  df$SG_PARTIDO[df$SG_PARTIDO == "PATRIOTA"] <- "Patriota"
  df$SG_PARTIDO[df$SG_PARTIDO == "PATRI"] <- "Patriota"
  df$SG_PARTIDO[df$SG_PARTIDO == "SOLIDARIEDADE"] <- "Solidariedade"
  
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
  
  situation <- c(
    "ELEITO ELEITO POR MÉDIA",
    "ELEITO POR QP",
    "ELEITO"
  )
  
  elected_df <- df %>%
    dplyr::filter(DS_SIT_TOT_TURNO %in% situation)
  
  return(elected_df)
}



# ~~~~ function ~~~~ set PROPORTION

set_proportion_by_year_party <- function(df) {
  
  df_total_by_candidates <- df %>%
    group_by(ANO_ELEICAO, DS_CARGO, NM_CANDIDATO) %>%
    summarize(QUANTITY = sum(QUANTITY))
  
  df_by_party <- df %>%
    group_by(ANO_ELEICAO, DS_CARGO, SG_PARTIDO) %>%
    summarize(QUANTITY = sum(QUANTITY))
  
  df_by_party$TOTAL_ELECTED <- sum(df$QUANTITY)
  
  df_by_party$PROPORTION <- (df_by_party$QUANTITY * 100)/df_by_party$TOTAL_ELECTED
  
  return(df_by_party)
}



# ~~~~ function ~~~~ PLOT

proportional_stacked_area <- function(df) {
  
  # df$SG_PARTIDO <- factor(df$SG_PARTIDO,
  #                         levels=c(
  #                           "PMN", "PSOL", "PCdoB", "PT", "PV", "PDT", "PSB", 
  #                           "Solidariedade", "MDB", "PSD", "PSDB", "AVANTE", "PRP",
  #                           "PROS", "PP", "PTC", "PRTB", "Patriota", "PHS",
  #                           "PSC", "União Brasil", "PL", "PTB", "Republicanos"
  #                         )
  # )
  
  # Proportional Stacked Area Chart
   plot <- df %>%
    ggplot(aes(x= ANO_ELEICAO,
               y= PROPORTION,
               fill=SG_PARTIDO)) +
    geom_area(alpha=0.5) +
    theme(
      plot.margin = margin(20, 20, 20, 20),
      plot.title = element_text(vjust=3, hjust=.5),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey88", linetype='dashed'),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(vjust=-2),
      axis.title.y = element_text(vjust=3),
      axis.ticks = element_blank(),
      legend.title = element_blank()
    ) +
    scale_fill_manual(values = parties_colores) +
    scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
    labs(title="Variation of parties in Minas Gerais",
      y="Proportion",
           x="Election Year")
  
  return(plot)
}



# ~~~~ function ~~~~ SAVE PLOT

plot_save <- function(plot, df) {
  
  if (df$DS_CARGO %in% c("Deputado Federal", "DEPUTADO FEDERAL")) { # zero length  !!!!!!!!!!!!!!!
    rep_type <- "federal"
    
  } else {
    rep_type <- "state"
  }
  
  file_name <- paste0("../saved_charts/proportional_stacked_area_MG_representatives_", rep_type, ".jpg")
  
  ggsave(plot, filename=file_name,
         units = "cm",
         width = 15,
         height = 10
  )
}



# ~~~~ function ~~~~ AUTOMATION

automatic_analysis <-function(df_list) {
  
  for(df in df_list) {
    
    df <- reduce_variables(df)
    
    df <- adjust_abbreviation_parties(df)
    
    df_representatives <- separate_by_position(df) # return a list[1 e 2]
    
    state_rep <- select_elected(df_representatives[[1]])
    federal_rep <- select_elected(df_representatives[[2]])
    
    state_rep$QUANTITY <- 1
    federal_rep$QUANTITY <- 1
    
    state_rep_by_party <- set_proportion_by_year_party(state_rep)
    federal_rep_by_party <- set_proportion_by_year_party(federal_rep)
    
    by_states <- rbind(by_states, state_rep_by_party)
    by_federals <- rbind(by_federals, federal_rep_by_party)
  }
  
  plot_state_rep <-  proportional_stacked_area(by_states)
  plot_federals_rep <- proportional_stacked_area(by_federals)
  
  plot_save(plot_state_rep, by_states)
  plot_save(plot_federals_rep, by_federals)
  
}







# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

mg_2010 <- read.table("../dados/votacao_candidato_munzona_2010_MG.csv", header=TRUE, sep=";")
mg_2014 <- read.table("../dados/votacao_candidato_munzona_2014_MG.csv", header=TRUE, sep=";")
mg_2018 <- read.table("../dados/votacao_candidato_munzona_2018_MG.csv", header=TRUE, sep=";")
mg_2022 <- read.table("../dados/votacao_candidato_munzona_2022_MG.csv", header=TRUE, sep=";")

all_dataframes <- list(
  mg_2022,
  mg_2018,
  mg_2014,
  mg_2010
)



# ~~~~~~~~~~~~~~~~  AUTOMATION  ~~~~~~~~~~~~~~~~

automatic_analysis(all_dataframes)


