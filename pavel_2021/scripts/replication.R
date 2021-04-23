

setwd("pavel_2021")
library(here)
library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)

# importando dados
rep_data1 <- read_excel("rep_data/Data_Study1_2_3.xlsx", sheet = "Study1")
rep_data2 <- read_excel("rep_data/Data_Study1_2_3.xlsx", sheet = "Study_2")
rep_data3 <- read_excel("rep_data/Data_Study1_2_3.xlsx", sheet = "Study_3")

# inspecionando importação dos dados
glimpse(rep_data1)
glimpse(rep_data2)
glimpse(rep_data3)

# Questão: no estudo 3, democratas tiveram pequena mudança de crença com relação itens republicanos (painel C, p.12)
# e mudança não significativa no comportamento. O mesmo se repete no estudo 2?

ideology_3 <- rep_data3 %>%
  dplyr::select(Q5, Q1184, Q21)

#primeiro replicando painel do estudo 3
rep_data3 %>%
  slice(-(1:3)) %>% #removendo duas primeiras linhas
  dplyr::filter(grepl("Yes", Q5)) %>%
  group_by(Q21) %>%
  summarise(n())

# Não há 137 republicanos pela pergunta Q21
rep_data3 %>%
  slice(-(1:3)) %>% #removendo duas primeiras linhas
  group_by(Q1184) %>%
  summarise(n())
# Não há 137 republicanos pela pergunta Q1184

# transformando variáveis para aumentar o número de republicanos e poder replicar

rep_data3 %>%
  slice(-(1:3)) %>% #removendo duas primeiras linhas
  dplyr::filter(grepl("Yes", Q5)) %>%
  mutate(Q1184 = if_else(is.na(Q1184), Q21, Q1184)) %>% # se não tem o partido na pergunda 1184, pega da Q21
  dplyr::filter(!(grepl("Independent", Q1184) | grepl("Other", Q1184))) %>% 
  group_by(Q1184) %>%
  summarise(n())
# agora, 143 rebulicanos e 270 democratas. Total 413. Precisamos chegar ao número do estudo

rep_data3 %>%
  slice(-(1:3)) %>% #removendo duas primeiras linhas
  dplyr::filter(grepl("Yes", Q5)) %>%
  mutate(q1095_page_submit = as.numeric(`Q1095_Page Submit`)) %>%
  dplyr::filter(q1095_page_submit < 250) %>%
  mutate(q1095_first_click = as.numeric(`Q1095_First Click`)) %>%
  dplyr::filter(q1095_first_click < 120) %>%
  mutate(Q1184 = if_else(is.na(Q1184), Q21, Q1184)) %>% # se não tem o partido na pergunda 1184, pega da Q21
  dplyr::filter(!(grepl("Independent", Q1184) | grepl("Other", Q1184))) %>% 
  mutate(Q1184 = if_else(grepl("Demo",Q1184), "Democratic", Q1184)) %>%
  mutate(Q11 = as.numeric(Q11)) %>%
  group_by(Q1184) %>%
  summarise(n(),
            mean(Q11),
            max(Q11))



rep_data3_a <- rep_data3 %>%
  slice(-(1:3)) %>% #removendo três primeiras linhas (3 parece que é teste. Ver resposta pra idade, Q11)
  dplyr::filter(grepl("Yes", Q5)) %>%
  mutate(Q1184 = if_else(is.na(Q1184), Q21, Q1184)) %>% # se não tem o partido na pergunda 1184, pega da Q21
  dplyr::filter(!(grepl("Independent", Q1184) | grepl("Other", Q1184)))
  
rep_data3_a %>%
  mutate(Q11 = as.numeric(Q11)) %>%
  arrange(desc(Q11)) %>%
  View()

rep_data3_a %>%
  mutate(Q11 = as.numeric(Q11)) %>%
  summarise(median(Q11)) # 37.8

rep_data3_a %>%
  clean_names() %>%
  mutate(q1095_first_click = as.numeric(q1095_first_click)) %>%
  dplyr::filter(q1095_first_click > 70) %>%
  View()

rep_data3_a %>%
  clean_names() %>%
  mutate(q1095_first_click = as.numeric(q1095_first_click)) %>%
  dplyr::filter(q1095_first_click < 60) %>%
  summarise(median(as.numeric(q1095_first_click)),
            median(as.numeric(q1095_last_click)),
            median(as.numeric(q1095_page_submit)),
            median(as.numeric(q1095_click_count)),
            sd(as.numeric(q1095_first_click)),
            sd(as.numeric(q1095_last_click)),
            sd(as.numeric(q1095_page_submit)),
            sd(as.numeric(q1095_click_count)))
# precisamos chegar ao

rep_data3 %>%
  slice(-(1:2)) %>% #removendo duas primeiras linhas
  dplyr::filter(grepl("Yes", Q5)) %>%
  group_by(Q21) %>%
  summarise(n())
