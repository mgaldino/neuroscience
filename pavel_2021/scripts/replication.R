

setwd("pavel_2021")
library(here)
library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)
library(rstanarm)
library(ggplot2)
library(tidyr)

# importando dados
rep_data1 <- read_excel("rep_data/Data_Study1_2_3.xlsx", sheet = "Study1")
rep_data2 <- read_excel("rep_data/Data_Study1_2_3.xlsx", sheet = "Study_2")
rep_data3 <- read_excel("rep_data/Data_Study1_2_3.xlsx", sheet = "Study_3")

# inspecionando importação dos dados
glimpse(rep_data1)
glimpse(rep_data2)
glimpse(rep_data3)

# Não sei como o screening funcionou
# no estudo 3, ficaram 393 participantes, 256 Democrats and 137 Republicans
# Então não consigo reproduzir exatamente a amostra do estudo

rep_data3_aprox <- rep_data3 %>%
  slice(-(2:3)) %>% #removendo duas primeiras linhas
  filter(Q21 != "Other") %>%
  filter(Q21 != "Independent") %>%
  filter(!is.na(Q21))

# Aqui ficamos com 375, 243 D, 132 R.
rep_data3_aprox %>%
  slice(-1) %>%
  group_by(Q21) %>%
  summarise(particp=n()) %>%
  mutate(total = sum(particp))

#escolhendo dados de crenças e doações (pré e pós)
rep_data3_aux <- rep_data3_aprox %>%
  slice(-1) %>%
  select( c(9, 20:36, 41:58)) #colunas relevantes com as perguntas sobre crenças e doações pré e pós

#Pegando todas as perguntas de crenças (pré e pós)
rep_data3_belief <- rep_data3_aux %>%
  select( c(1:9, 19:26)) 
names(rep_data3_belief)[2:17] <- c(paste(rep("x", 8), 1:8, sep=""),  paste(rep("y", 8), 1:8, sep=""))

rep_data3_belief_aux <- rep_data3_belief %>%
  pivot_longer(cols = !ResponseId,
               names_to = c(".value", "question"),
               names_pattern = "(.)(.)")

names(rep_data3_belief_aux)[3:4] <- c("belief_pre", "belief_post")

#Pegando todas as perguntas de doações (pré e pós)
rep_data3_behavior <- rep_data3_aux %>%
  select( c(1, 10:17, 28:35)) ## Q1725_32:Q1725_50 Q1169_32:Q1169_50

names(rep_data3_behavior)[2:17] <- c(paste(rep("x", 8), 1:8, sep=""),  paste(rep("y", 8), 1:8, sep=""))

rep_data3_behavior_aux <- rep_data3_behavior %>%
  pivot_longer(cols = !ResponseId,
               names_to = c(".value", "question"),
               names_pattern = "(.)(.)")

names(rep_data3_behavior_aux)[3:4] <- c("behave_pre", "behave_post")

# juntando crença e comportamento em uma única base

rep_data3_final <- bind_cols(rep_data3_belief_aux, rep_data3_behavior_aux) %>%
  select(1,2,3,4,7,8) %>%
  rename(ResponseId = ResponseId...1,
         question =  question...2) %>%
  mutate_at(c("belief_pre", "belief_post",  "behave_pre", "behave_post"), as.numeric)

rep_data3_final <- rep_data3_final %>%
  mutate(change_belief = belief_post - belief_pre,
         change_behavior = behave_post - behave_pre)
glimpse(rep_data3_final)

# gráfico 1 da fig4

rep_data3_final %>%
  ggplot(aes(belief_pre, behave_pre)) + geom_smooth(method= "lm", formula = y ~ x) +
  coord_cartesian(xlim=c(0,100), ylim=c(0, 10))
  
# gráfico 2 da fig4

rep_data3_final %>%
  ggplot(aes(change_belief, change_behavior)) + geom_smooth(method= "lm", formula = y ~ x) +
  coord_cartesian(xlim=c(-100,100), ylim=c(-10, 10))
