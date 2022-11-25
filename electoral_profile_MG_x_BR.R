# electoral profile: MG x BR ----- BY GENDER



# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais")


library(dplyr)
library(ggplot2)


options(scipen=999)



# ~~~~~~~~~~~~~~~~  IMPORT DATA  ~~~~~~~~~~~~~~~~

general_electoral_profile_2022 <- read.table("dados/perfil_eleitorado_2022.csv", header=TRUE, sep=";")





# ~~~~ function ~~~~ SET PROPORTIONS AND PERCENTS BY TOTAL

set_proportions_and_percents <- function(df) {
  
  df <- df %>%
    group_by(DS_GENERO) %>%
    summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))
  
  # ADD COLUMN WITH TOTAL VOTES BY MUNICIPALITY
  df$TOTAL_VOTES <- sum(df$QT_ELEITORES_PERFIL)
  
  # ADD PROPORTION
  df$PROPORTION <- df$QT_ELEITORES_PERFIL/ df$TOTAL_VOTES
  
  # ADD PERCENT
  df$PERCENT <- df$PROPORTION *100
  
  # FORMAT PERCENT
  df$PERCENT_FORMAT <- paste0(sprintf("%4.2f", df$PERCENT), "%")
  
  df$VOTES_MEASURE <- "Total Votes"
  
  return(df)
}





# ~~~~~~~~~~~~~~~~  reorder data  ~~~~~~~~~~~~~~~~

# group data
br_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))


# group data by State
sp_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="SP") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

mg_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="MG") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

rj_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="RJ") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

ba_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="BA") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

pr_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="PR") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

rs_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="RS") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))

pe_electoral_profile_2022_grouped <- general_electoral_profile_2022 %>%
  filter(SG_UF=="PE") %>%
  group_by(DS_GENERO, DS_ESTADO_CIVIL, DS_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE) %>%
  summarise(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL))









br_electoral_profile_2022_grouped <- set_proportions_and_percents(br_electoral_profile_2022_grouped)
sp_electoral_profile_2022_grouped <- set_proportions_and_percents(sp_electoral_profile_2022_grouped)
mg_electoral_profile_2022_grouped <- set_proportions_and_percents(mg_electoral_profile_2022_grouped)
rj_electoral_profile_2022_grouped <- set_proportions_and_percents(rj_electoral_profile_2022_grouped)
ba_electoral_profile_2022_grouped <- set_proportions_and_percents(ba_electoral_profile_2022_grouped)
pr_electoral_profile_2022_grouped <- set_proportions_and_percents(pr_electoral_profile_2022_grouped)
rs_electoral_profile_2022_grouped <- set_proportions_and_percents(rs_electoral_profile_2022_grouped)
pe_electoral_profile_2022_grouped <- set_proportions_and_percents(pe_electoral_profile_2022_grouped)



# set scope difference Brasil X Minas Gerais
br_electoral_profile_2022_grouped$SG_UF <- "Brasil"
mg_electoral_profile_2022_grouped$SG_UF <- "Minas Gerais"
sp_electoral_profile_2022_grouped$SG_UF <- "São Paulo"
rj_electoral_profile_2022_grouped$SG_UF <- "Rio de Janeiro"
ba_electoral_profile_2022_grouped$SG_UF <- "Bahia"
pr_electoral_profile_2022_grouped$SG_UF <- "Paraná"
rs_electoral_profile_2022_grouped$SG_UF <- "Rio Grande do Sul"
pe_electoral_profile_2022_grouped$SG_UF <- "Pernambuco"


# append datasets
electoral_profile_2022_grouped_merged <- rbind(
  br_electoral_profile_2022_grouped,
  sp_electoral_profile_2022_grouped,
  mg_electoral_profile_2022_grouped,
  rj_electoral_profile_2022_grouped,
  ba_electoral_profile_2022_grouped,
  pr_electoral_profile_2022_grouped,
  rs_electoral_profile_2022_grouped,
  pe_electoral_profile_2022_grouped
)



# set title case
electoral_profile_2022_grouped_merged$DS_GENERO <- stringr::str_to_title(electoral_profile_2022_grouped_merged$DS_GENERO)



# ~~~~~~~~~~~~~~~~  general plot variables  ~~~~~~~~~~~~~~~~
gender_colors <- c(
  "Feminino"="darksalmon",
  "Masculino"="darkslategray4",
  "Não Informado"="darkkhaki"
)

# set order to candidates names
electoral_profile_2022_grouped_merged$SG_UF <- factor(electoral_profile_2022_grouped_merged$SG_UF, levels=c(
  "Brasil",
  "São Paulo",
  "Minas Gerais",
  "Rio de Janeiro",
  "Bahia",
  "Paraná",
  "Rio Grande do Sul",
  "Pernambuco"
))





# ~~~~~~~~~~~~~~~~  plot  ~~~~~~~~~~~~~~~~

# TO IMPROVE: set x values with title case

electoral_profile_2022_grouped_merged %>%
  ggplot(aes(x=DS_GENERO,
             y=PERCENT,
             fill=DS_GENERO)) +
  geom_col() +
  geom_text(aes(label=PERCENT_FORMAT), vjust =-1, size=2) +
  facet_grid(~ SG_UF, scales="free_x") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(margin = margin(0, 0, 20, 0), size = 10),
    plot.background = element_rect(fill="grey95", color=NA),
    panel.grid.major = element_line(colour = "grey85", linetype='dashed'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill="grey95"),
    legend.title = element_blank()
  ) +
  lims(y=c(0, 75)) +
  labs(
    title="Percentage of the electorate by gender in Brazil and in the most populous states"
  ) +
  scale_fill_manual(values=gender_colors) + 
  facet_wrap(~SG_UF, ncol = 4)




ggsave("cols_election_br_x_mg_by_voters_gender.jpg",
       units = "cm",
       width = 15,
       height = 10
      )




