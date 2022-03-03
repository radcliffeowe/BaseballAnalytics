library(dplyr)
library(ggplot2)

WPI_PD <- read.csv("~/Baseball-Analytics/WPI_PD.csv", header = TRUE, sep = ",")

ggplot(WPI_PD, aes(IP, FIP, colour = PID)) +
  geom_point()

#' Individual Charts we want: 
#' K% by pitch
#' Pitch Usage %
#' Swing Rate by pitch
#' GB+FB% by pitch
#' BABIP by pitch
#' 
#' Teamwide Charts we want:
#' R/L FIP splits
#' 

# BABIP rel to GB%
ggplot(WPI_PD, aes(GBRATE, BABIP)) +
  geom_point() + 
  geom_text(label = WPI_PD$Pitcher, nudge_y = -0.00005, check_overlap = T)
