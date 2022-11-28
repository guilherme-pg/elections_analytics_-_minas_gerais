# PIE CHART TOTAL VOTES: BRAZIL x MINAS GERAIS



# SET MAIN DIRECTORY
setwd("C:/Users/guima/Desktop/data_science/Projetos/elections_analytics_-_minas_gerais")

library(dplyr)
library(ggplot2)


voters_2022 <- read.table("dados/perfil_eleitorado_2022.csv", header=TRUE, sep=";")


total_voters_br <- sum(voters_2022$QT_ELEITORES_PERFIL)


voters_mg <- voters_2022 %>%
  filter(SG_UF=="MG")


total_voters_mg <-sum(voters_mg$QT_ELEITORES_PERFIL)


percent_voters_mg <- total_voters_mg * 100/total_voters


voters_br_mg <- data.frame(
  voters = c(total_voters_br, total_voters_mg),
  region = c("Brasil", "Minas Gerais")
)


br_minus_mg <- 100 - percent_voters_mg

voters_br_mg$PERCENT <-c("89.59 %", "10.41 %")







colores <- c(
  "Brasil"="forestgreen",
  "Minas Gerais"="firebrick"
)


voters_br_mg %>%
  ggplot(aes(x="", y=voters, fill=region)) +
  geom_col() +
  geom_label(aes(label=PERCENT), 
             color="white",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE
            ) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill="grey95", color=NA),
    plot.title = element_text(hjust=1.5),
    legend.title = element_blank()
  ) +
  scale_fill_manual(
    values=colores
  ) +
  labs(title= "Total voters Brasil vs. Minas Gerais in 2022"
  )






# SAVE LAST PLOT
ggsave("pie_chart_2022_total_voters_br_x_mg.jpg",
       units = "cm",
       width = 15,
       height = 12
)



# b