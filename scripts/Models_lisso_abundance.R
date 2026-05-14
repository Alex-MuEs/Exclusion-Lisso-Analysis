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



#### Fit LMM gaussian transformed ####
model <- glmmTMB(log(Abundance+1) ~ Treatment*Date + (1|Field), data = Lisso, family = gaussian)
summary(model)
r2(model)

Anova(model, type = 3)

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)

#Estimated marginal means comparison plots
emmeans(model, pairwise~Treatment)
emmeans(model, pairwise~Treatment|Date)

model_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment)$emmeans)
ggplot(model_emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of log(Abundance+1)") +
  theme_minimal()
model_emm.2 <- as.data.frame(emmeans(model, pairwise ~ Treatment|Date)$emmeans)
ggplot(model_emm.2, aes(x = Date, y = emmean, color = Treatment)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of log(Abundance+1)") +
  theme_minimal()




#### Fit LMM family poisson ####
model.poi <- glmmTMB(Abundance ~ Treatment*Date + (1|Field), data = Lisso, family = poisson)
summary(model.poi)
r2(model.poi)

Anova(model.poi, type = 3)

#Model diagnostics
dharma <- simulateResiduals(model.poi, plot = T)

#Estimated marginal means comparison plots
emmeans(model.poi, pairwise~Treatment)
emmeans(model.poi, pairwise~Treatment|Date)

model.poi_emm <- as.data.frame(emmeans(model.poi, pairwise ~ Treatment)$emmeans)
ggplot(model.poi_emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of Abundance") +
  theme_minimal()
model.poi_emm.2 <- as.data.frame(emmeans(model.poi, pairwise ~ Treatment|Date)$emmeans)
ggplot(model.poi_emm.2, aes(x = Date, y = emmean, color = Treatment)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of Abundance") +
  theme_minimal()


#Comparing models gaussian and poisson
model_performance(model)
model_performance(model.poi)




##### Comparing models ####
#with and without interaction
model.poi <- glmmTMB(Abundance ~ Treatment*Date + (1|Field), data = Lisso, family = poisson)
summary(model.poi)
r2(model.poi)

model.poi2 <- glmmTMB(Abundance ~ Treatment+Date + (1|Field), data = Lisso, family = poisson)
summary(model.poi2)
r2(model.poi2)

model_performance(model.poi)
model_performance(model.poi2)
MuMIn::model.sel(model.poi, model.poi2)


#poisson, negative binomial and poisson with dispersion parameter
model.disp <- glmmTMB(Abundance ~ Treatment*Date + (1|Field), data = Lisso, family = poisson, dispformula = ~1)
summary(model.disp)
r2(model.disp)
dharma <- simulateResiduals(model.disp, plot = T)

model.bn <- glmmTMB(Abundance ~ Treatment*Date + (1|Field), data = Lisso, family = nbinom2)
summary(model.bn)
r2(model.bn)
Anova(model.bn, type = 3)
dharma <- simulateResiduals(model.bn, plot = T)

model_performance(model.poi)
model_performance(model.disp)
model_performance(model.bn)
MuMIn::model.sel(model.poi, model.disp, model.bn)





#### Final model ####
model.final <- glmmTMB(log(Abundance+1) ~ Treatment*Date + (1|Field), data = Lisso, family = gaussian)
summary(model.final)
r2(model.final)

Anova(model.final, type = 3)

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)

#Estimated marginal means comparison plots
emmeans(model, pairwise~Treatment)
emmeans(model, pairwise~Treatment|Date)

model_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment)$emmeans)
ggplot(model_emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of log(Abundance+1)") +
  theme_minimal()
model_emm.2 <- as.data.frame(emmeans(model, pairwise ~ Treatment|Date)$emmeans)
ggplot(model_emm.2, aes(x = Date, y = emmean, color = Treatment)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of log(Abundance+1)") +
  theme_minimal()