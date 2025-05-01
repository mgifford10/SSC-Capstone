# Mary Gifford
# March 3, 2025
# -------------------------------------------------------------- #
# load packages
library(janitor)
library(dplyr)
library(haven)
library(tidyverse)
library(magrittr)
library(DescTools)
library(ggplot2)
# -------------------------------------------------------------- #
# set working directory
setwd("/Users/marygifford/Desktop/SOC209/data")
# -------------------------------------------------------------- #
# load gss2018 & gss1982 (DON'T RE-RUN!!!!)
gss2018 <- read_dta("gss2018.dta")
gss1982 <- read_dta("gss1982.dta")
# -------------------------------------------------------------- #
# save datasets as r files
save(gss2018, file = "gss2018.Rdata")
save(gss1982, file = "gss1982.Rdata")
# -------------------------------------------------------------- #
# load datasets
load("gss2018.RData")
load("gss1982.RData")
# -------------------------------------------------------------- #
# mvr dataset gss2018
mvr2018 <- gss2018 %>% 
  select(polviews, mobile16, race, famgen, maeduc, paeduc, relig) %>% 
  remove_missing(na.rm = TRUE) %>% 
  mutate(religion = case_when (relig < 0 ~ NA,
                               relig == 1 ~ "protestant",
                               relig == 2 ~ "catholic",
                               relig == 3 ~ "jewish",
                               relig == 4 ~ "none",
                               relig >= 5 ~"other")) %>% 
  mutate(race.new = case_when (race < 0 ~ NA,
                               race == 1 ~ "white",
                               race == 2 ~"black",
                               race == 3 ~"other"),
         race.new = fct(race.new, levels = c("white", "black", "other"))) %>% 
  mutate(generations = case_when (famgen < 0 ~ NA,
                                  famgen == 1 ~ "one gen",
                                  famgen == 2 ~ "two gens children",
                                  famgen == 3 ~ "two gens parents",
                                  famgen == 4 ~ "two gens grandchildren",
                                  famgen == 5 ~ "children and grandchildren",
                                  famgen == 6 ~ "children and parents",
                                  famgen == 7 ~ "four gens")) %>% 
  mutate(mob16 = case_when(mobile16 < 0 ~ NA,
                           mobile16 == 1 ~ "same city", 
                           mobile16 == 2 ~ "same st",
                           mobile16 == 3 ~ "dif st"), 
         mob16 = fct(mob16, levels = c("same city","same st","dif st"))) %>% 
  mutate(maeduc2 = if_else(maeduc < 0, NA, maeduc),
         paeduc2 = if_else(paeduc < 0, NA, paeduc),
         polviews2 = if_else(polviews < 0, NA, polviews))
# -------------------------------------------------------------- #
# mvr dataset gss1982
mvr1982 <- gss1982 %>% 
  select(polviews, mobile16, race, famgen, maeduc, paeduc, relig) %>% 
  remove_missing(na.rm = TRUE) %>% 
  mutate(religion = case_when (relig < 0 ~ NA,
                               relig == 1 ~ "protestant",
                               relig == 2 ~ "catholic",
                               relig == 3 ~ "jewish",
                               relig == 4 ~ "none",
                               relig >= 5 ~"other")) %>% 
  mutate(race.new = case_when (race < 0 ~ NA,
                               race == 1 ~ "white",
                               race == 2 ~"black",
                               race == 3 ~"other"),
      race.new = fct(race.new, levels = c("white", "black", "other"))) %>% 
  mutate(generations = case_when (famgen < 0 ~ NA,
                                  famgen == 1 ~ "one gen",
                                  famgen == 2 ~ "two gens children",
                                  famgen == 3 ~ "two gens parents",
                                  famgen == 4 ~ "two gens grandchildren",
                                  famgen == 5 ~ "children and grandchildren",
                                  famgen == 6 ~ "children and parents",
                                  famgen == 7 ~ "four gens")) %>% 
  mutate(mob16 = case_when(mobile16 < 0 ~ NA,
                           mobile16 == 1 ~ "same city", 
                           mobile16 == 2 ~ "same st",
                           mobile16 == 3 ~ "dif st"), 
         mob16 = fct(mob16, levels = c("same city","same st","dif st"))) %>% 
  mutate(maeduc2 = if_else(maeduc < 0, NA, maeduc),
         paeduc2 = if_else(paeduc < 0, NA, paeduc),
         polviews2 = if_else(polviews < 0, NA, polviews))
# -------------------------------------------------------------- #
# create multivariate regressions (2018)
mvr1.2018 <- lm(polviews2 ~ mob16, data = mvr2018)
summary(mvr1.2018)

mvr2.2018 <- lm(polviews2 ~ mob16 + race.new, data = mvr2018)
summary(mvr2.2018)

mvr3.2018 <- lm(polviews2 ~ mob16 + race.new + maeduc2, data = mvr2018)
summary(mvr3.2018)

mvr4.2018 <- lm(polviews2 ~ mob16 + race.new + maeduc2 + paeduc2, data = mvr2018)
summary(mvr4.2018)

mvr5.2018 <- lm(polviews2 ~ mob16 + race.new + maeduc2 + paeduc2 + generations
              , data = mvr2018)
summary(mvr5.2018)
mvr6.2018 <- lm(polviews2 ~ mob16 + race.new + maeduc2 + paeduc2 + generations
                + religion
                , data = mvr2018)
summary(mvr6.2018)
# -------------------------------------------------------------- #
# create multivariate regressions (1982)
mvr1.1982 <- lm(polviews2 ~ mob16, data = mvr1982)
summary(mvr1.1982)

mvr2.1982 <- lm(polviews2 ~ mob16 + race.new, data = mvr1982)
summary(mvr1982)

mvr3.1982 <- lm(polviews2 ~ mob16 + race.new + maeduc2, data = mvr1982)
summary(mvr3.1982)

mvr4.1982 <- lm(polviews2 ~ mob16 + race.new + maeduc2 + paeduc2, data = mvr1982)
summary(mvr4.1982)

mvr5.1982 <- lm(polviews2 ~ mob16 + race.new + maeduc2 + paeduc2 + generations
                , data = mvr1982)
summary(mvr5.1982)
mvr6.1982 <- lm(polviews2 ~ mob16 + race.new + maeduc2 + paeduc2 + generations
                + religion
                , data = mvr1982)
summary(mvr6.1982)
# -------------------------------------------------------------- #
# compare r-squared (2018)
summary(mvr1.2018)$r.squared
# 0.004721265
summary(mvr2.2018)$r.squared
# 0.02183422
summary(mvr3.2018)$r.squared
# 0.02849592
summary(mvr4.2018)$r.squared
# 0.02880713
# -------------------------------------------------------------- #
# compare r-squared (1982)
summary(mvr1.1982)$r.squared
# 0.0001649747
summary(mvr2.1982)$r.squared
# 0.02682107
summary(mvr3.1982)$r.squared
# 0.0338737
summary(mvr4.1982)$r.squared
# 0.03942912
# -------------------------------------------------------------- #
# measures of central tendency (2018)
gss2018mct <- gss2018 %>% 
  select(polviews, mobile16, race, famgen, maeduc, paeduc, relig) %>% 
  mutate(polviews1 = if_else(polviews < 0, NA, polviews), 
         maeduc1 = if_else(maeduc < 0, NA, maeduc), 
         paeduc1 = if_else(paeduc < 0, NA, paeduc),
         mobile16.1 = if_else(mobile16 < 0, NA, mobile16),
         race1 = if_else(race < 0, NA, race)) %>% 
  remove_missing(na.rm = TRUE)

# mean, median, mode, sd of maeduc
mean.maeduc1.18 <- mean(gss2018mct$maeduc1)
# 11.95
median.maeduc1.18 <- median(gss2018mct$maeduc1)
# 12
mode.maeduc1.18 <- Mode(gss2018mct$maeduc1)
# 12
sd.maeduc1.18 <- sd(gss2018mct$maeduc1)
# 3.79

# mean, median, mode, sd of paeduc
mean.paeduc1.18 <- mean(gss2018mct$paeduc1)
# 11.98
median.paeduc1.18 <- median(gss2018mct$paeduc1)
# 12
mode.paeduc1.18 <- Mode(gss2018mct$paeduc1)
# 12
sd.paeduc1.18 <- sd(gss2018mct$paeduc1)
# 4.12

# mean, median, mode, sd of polviews
mean.polviews1.18 <- mean(gss2018mct$polviews1)
# 4.07
median.polviews1.18 <- median(gss2018mct$polviews1)
# 4
mode.polviews1.18 <- Mode(gss2018mct$polviews1)
# 4
sd.polviews1.18 <-sd(gss2018mct$polviews1)
# 1.52
-------------------------------------------------------------- #
# measures of central tendency (1982)
  gss1982mct <- gss1982 %>% 
  select(polviews, mobile16, race, famgen, maeduc, paeduc, relig) %>% 
  mutate(polviews1 = if_else(polviews < 0, NA, polviews), 
         maeduc1 = if_else(maeduc < 0, NA, maeduc), 
         paeduc1 = if_else(paeduc < 0, NA, paeduc),
         mobile16.1 = if_else(mobile16 < 0, NA, mobile16),
         race1 = if_else(race < 0, NA, race)) %>% 
  remove_missing(na.rm = TRUE)
  
# mean, median, mode, sd of maeduc
mean.maeduc1.82 <- mean(gss1982mct$maeduc1)
# 10.2 
median.maeduc1.82 <- median(gss1982mct$maeduc1)
# 12
mode.maeduc1.82 <- Mode(gss1982mct$maeduc1)
# 12
sd.maeduc1.82 <- sd(gss1982mct$maeduc1)
# 3.71

# mean, median, mode, sd of paeduc
mean.paeduc1.82 <- mean(gss1982mct$paeduc1)
# 9.75
median.paeduc1.82 <- median(gss1982mct$paeduc1)
# 10
mode.paeduc1.82 <- Mode(gss1982mct$paeduc1)
# 12
sd.paeduc1.82 <- sd(gss1982mct$paeduc1)
# 4.28 

# mean, median, mode, sd of polviews
mean.polviews1.82 <- mean(gss1982mct$polviews1)
# 4.1
median.polviews1.82 <- median(gss1982mct$polviews1)
# 4
mode.polviews1.82 <- Mode(gss1982mct$polviews1)
# 4
sd.polviews1.82 <-sd(gss1982mct$polviews1)
# 1.36
# -------------------------------------------------------------- #
# mode of other variables
mode.race1.82 <-Mode(gss1982mct$race1)
#1
mode.race1.18 <-Mode(gss2018mct$race1)
#1

mode.relig.82 <-Mode(gss1982mct$relig)
#1
mode.relig.18 <-Mode(gss2018mct$relig)
#1

mode.mob16.82 <-Mode(gss1982mct$mobile16.1)
#1
mode.mob16.18 <-Mode(gss2018mct$mobile16.1)
#3

mode.gens.82 <-Mode(gss1982mct$famgen)
#1 
mode.gens.18 <-Mode(gss2018mct$famgen)
#1 

# Check Variable Creation
tabyl(mvr2018, race, race.new)
tabyl(mvr2018, polviews, polviews2)
tabyl(mvr2018, maeduc, maeduc2)
tabyl(mvr2018, paeduc, paeduc2)
tabyl(mvr2018, mobile16, mob16)
tabyl(mvr2018, relig, religion)
tabyl(mvr2018, famsize, generations)

tabyl(mvr1982, race, race.new)
tabyl(mvr1982, polviews, polviews2)
tabyl(mvr1982, maeduc, maeduc2)
tabyl(mvr1982, paeduc, paeduc2)
tabyl(mvr1982, mobile16, mob16)
tabyl(mvr1982, relig, religion)
tabyl(mvr1982, famsize, generations)
# -------------------------------------------------------------- #
# Frequency Tables/One-Variable crosstabs
tabyl(mvr2018, race.new)
tabyl(mvr2018, polviews2)
tabyl(mvr2018, maeduc2)
tabyl(mvr2018, paeduc2)
tabyl(mvr2018, mob16)
tabyl(mvr2018, religion)
tabyl(mvr2018, generations)

tabyl(mvr1982, race.new)
tabyl(mvr1982, polviews2)
tabyl(mvr1982, maeduc2)
tabyl(mvr1982, paeduc2)
tabyl(mvr1982, mob16)
tabyl(mvr1982, religion)
tabyl(mvr1982, generations)
# -------------------------------------------------------------- #
# Frequency Distributions
ggplot(mvr1982 %>% remove_missing(), aes(religion)) + #Religion/82
  geom_bar(color = "#F0BDFF", fill = "#F0BDFF")

ggplot(mvr2018 %>% remove_missing(), aes(religion)) + #Religion/18
  geom_bar(color = "#F0BDFF", fill = "#F0BDFF")

ggplot(mvr1982 %>% remove_missing(), aes(mob16)) + #Mobile16/82
  geom_bar(color = "#BDF1FF", fill = "#BDF1FF")

ggplot(mvr2018 %>% remove_missing(), aes(mob16)) + #Mobile16/18
  geom_bar(color = "#BDF1FF", fill = "#BDF1FF")

ggplot(mvr1982 %>% remove_missing(), aes(maeduc2)) + #Maeduc/82
  geom_bar(color = "#F96183", fill = "#F96183")

ggplot(mvr2018 %>% remove_missing(), aes(maeduc2)) + #Maeduc/18
  geom_bar(color = "#F96183", fill = "#F96183")

ggplot(mvr1982 %>% remove_missing(), aes(polviews2)) + #Polviews/82
  geom_bar(color = "yellow", fill = "yellow")

  ggplot(mvr2018 %>% remove_missing(), aes(polviews2)) + #Polviews/18
  geom_bar(color = "yellow", fill = "yellow")

ggplot(mvr1982 %>% remove_missing(), aes(race.new)) + #Race/82
  geom_bar(color = "green", fill = "green")

ggplot(mvr2018 %>% remove_missing(), aes(race.new)) + #Race/18
  geom_bar(color = "green", fill = "green")

ggplot(mvr1982 %>% remove_missing(), aes(paeduc2)) + #Paeduc/82
  geom_bar(color = "pink", fill = "pink")

ggplot(mvr2018 %>% remove_missing(), aes(paeduc2)) + #Paeduc/18
  geom_bar(color = "pink", fill = "pink")

ggplot(mvr1982 %>% remove_missing(), aes(generations)) + #Paeduc/82
  geom_bar(color = "blue", fill = "blue")

ggplot(mvr2018 %>% remove_missing(), aes(generations)) + #Paeduc/18
  geom_bar(color = "blue", fill = "blue")
# -------------------------------------------------------------- #
# Two-Variable Crosstabs
tabyl(mvr1982, polviews2, race.new, #Race/Polviews 82
      show_missing_levels = FALSE,
      show_na = FALSE) %>%
  adorn_totals(c("col","row")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) 

tabyl(mvr2018, polviews2, race.new, #Race/Polviews 18
      show_missing_levels = FALSE,
      show_na = FALSE) %>%
  adorn_totals(c("col","row")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) 

tabyl(mvr1982, polviews2, religion, #Religion/Polviews 82
      show_missing_levels = FALSE,
      show_na = FALSE) %>%
  adorn_totals(c("col","row")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) 

tabyl(mvr2018, polviews2, religion, #Religion/Polviews 18
      show_missing_levels = FALSE,
      show_na = FALSE) %>%
  adorn_totals(c("col","row")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) 

tabyl(mvr1982, polviews2, mob16, #Mob16/Polviews 82
      show_missing_levels = FALSE,
      show_na = FALSE) %>%
  adorn_totals(c("col","row")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) 

tabyl(mvr2018, polviews2, mob16, #Mob16/Polviews 18
      show_missing_levels = FALSE,
      show_na = FALSE) %>%
  adorn_totals(c("col","row")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) 
# -------------------------------------------------------------- #
