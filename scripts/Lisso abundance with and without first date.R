# Comparing lisso abundance models with and without first date #

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
library (visreg)

#Load data
Lisso <- read.csv2("data/modified/Lisso_abundance.csv") %>% 
  mutate(Date = as.factor(Date),
         Field = as.factor(Field),
         Treatment = as.factor(Treatment))

Lisso.2 <- read.csv2("data/modified/Lisso_abundance.csv") %>% 
  filter(Date != "2025-06-26") %>% 
  mutate(Date = as.factor(Date),
         Field = as.factor(Field),
         Treatment = as.factor(Treatment))



#### Model with first date ####
model <- glmmTMB(Abundance ~ Treatment + (1|Field) + (1|Date), data = Lisso, family = poisson)
summary(model)
r2(model)

check_distribution(model)

Anova(model, type = 2)

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)

#Estimated marginal means comparison plots
emmeans(model, pairwise~Treatment)


model_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment, type = "response")$emmeans)
ggplot(model_emm, aes(x = Treatment, y = rate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = rate-SE, ymax = rate+SE), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of Abundance") +
  theme_minimal()


visreg(model, "Treatment", scale="response", type = "conditional", rug = FALSE, gg = TRUE) +
  theme_minimal()



#### Model w/o first date ####
model.2 <- glmmTMB(Abundance ~ Treatment + (1|Field) + (1|Date), data = Lisso.2, family = poisson)
summary(model.2)
r2(model.2)

Anova(model.2, type = 2)

#Model diagnostics
dharma <- simulateResiduals(model.2, plot = T)

#Estimated marginal means comparison plots
emmeans(model.2, pairwise~Treatment)


model.2_emm <- as.data.frame(emmeans(model.2, pairwise ~ Treatment, type = "response")$emmeans)
ggplot(model.2_emm, aes(x = Treatment, y = rate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = rate-SE, ymax = rate+SE), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of Abundance") +
  theme_minimal()


visreg(model.2, "Treatment", scale="response", type = "conditional", rug = FALSE, gg = TRUE) +
  theme_minimal()
