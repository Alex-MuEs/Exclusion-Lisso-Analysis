######## Mixed models analysis ########
### Leaf damage ###

rm(list = ls())

#Load libraries
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(car)
library(performance)



#Load data
leaf_dmg <- read.csv2("data/original/leaf_dmg.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))

leaf_dmg <- leaf_dmg %>%
  mutate(No_dmg = 10 - Leaves_dmg_10leaves) %>% 
  mutate(julian_day = yday(Date)) %>% 
  filter(Treatment %in% c("BE", "FO"))

#Paired t-test glmmTMB
model <- glmmTMB(cbind(Leaves_dmg_10leaves, No_dmg) ~ Treatment + julian_day + (1|Field), data = leaf_dmg, family = binomial)
summary(model)
r2(model)
emmeans(model, pairwise ~ Treatment)

Anova(model)

#Plot emmeans
emm <- as.data.frame(emmeans(model, pairwise ~ Treatment)$emmeans)

ggplot(emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  #geom_jitter(data = leaf_dmg, aes(x = Treatment, y = log(Leaves_dmg_10leaves)-log(No_dmg)), height = 0, width = 0.1, alpha = 0.5) + #No sé si esto está bien
  labs(title = "Estimated Marginal Means of Leaf Damage by Treatment",
       x = "Treatment",
       y = "Estimated Marginal Mean of Leaf Damage") +
  theme_minimal()

#Comprobación de asunciones del modelo
dharma <- simulateResiduals(model, plot = T)


###Same without field 5 data###
leaf_dmg_no5 <- leaf_dmg %>% filter(Field != 5)

model_no5 <- glmmTMB(cbind(Leaves_dmg_10leaves, No_dmg) ~ Treatment + julian_day + (1|Field), data = leaf_dmg_no5, family = binomial)
summary(model_no5)
r2(model_no5)
emmeans(model_no5, pairwise ~ Treatment)

emm_no5<-as.data.frame(emmeans(model_no5, pairwise ~ Treatment)$emmeans)
ggplot(emm_no5, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  #geom_jitter(data = leaf_dmg, aes(x = Treatment, y = log(Leaves_dmg_10leaves)-log(No_dmg)), height = 0, width = 0.1, alpha = 0.5) + #No sé si esto está bien
  labs(title = "Estimated Marginal Means of Leaf Damage by Treatment",
       x = "Treatment",
       y = "Estimated Marginal Mean of Leaf Damage") +
  theme_minimal()

#Comprobación de asunciones del modelo
dharma_no5 <- simulateResiduals(model_no5, plot = T)
