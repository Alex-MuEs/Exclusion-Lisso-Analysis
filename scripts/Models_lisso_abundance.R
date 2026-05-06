#### LISSO ABUNDANCE ####

######## Mixed models analysis ########

rm(list = ls())
cat("\014")
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)

#Load libraries
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(performance)
library(car)

#Load data
Lisso <- read.csv2("data/modified/Lisso_abundance.csv") %>% 
  mutate(Date = as.factor(Date),
         Field = as.factor(Field))

#Fit LMM
model <- glmmTMB(sqrt(Abundance) ~ Treatment*Date + (1|Field), data = Lisso, family = gaussian)
summary(model)
r2(model)

Anova(model, type = 3)

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)
