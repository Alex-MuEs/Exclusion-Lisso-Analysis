######## Mixed models analysis ########
### Yield ###

rm(list = ls())

#Load libraries
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(performance)
library(car)

#Load data
Yield <- read.csv2("data/original/Yield.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.factor(Field), 
         Repeat = as.character(Repeat)) %>% 
  filter(Treatment %in% c("BE", "FO"))

#Fit LMM
model <- glmmTMB(sqrt(Yield_kg.ha_HR14) ~ Treatment + (1|Field), data = Yield, family = gaussian)
summary(model)
r2(model)
emmeans(model, pairwise ~ Treatment)

emm <- as.data.frame(emmeans(model, pairwise ~Treatment)$emmeans)
ggplot(emm, aes(x = Treatment, y = emmean))+
         geom_point(size = 2)+
         geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, width = 0.2))+
         labs(x = "Treatment", y = "Estimated Marginal Mean of Yield (kg/ha)")+
         theme_minimal()

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)


### Same without field 5 data ###
Yield_no5 <- Yield %>% 
  filter(Field != "5")

model_no5 <- glmmTMB(sqrt(Yield_kg.ha_HR14) ~ Treatment + (1|Field), data = Yield_no5, family = gaussian)
summary(model_no5)
r2(model_no5)
emmeans(model_no5, pairwise ~ Treatment)

emm <- as.data.frame(emmeans(model_no5, pairwise ~Treatment)$emmeans)
ggplot(emm, aes(x = Treatment, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, width = 0.2))+
  labs(x = "Treatment", y = "Estimated Marginal Mean of Yield (kg/ha)")+
  theme_minimal()

#Model diagnostics
dharma_no5 <- simulateResiduals(model_no5, plot = T)


