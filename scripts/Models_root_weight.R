######## Mixed models analysis ########
### Root biomass ###

# Load packages
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)

# Load data
root <- read.csv2("data/original/Root_dmg_20.csv") %>% 
        mutate(Date = as.POSIXct(Date),
               Field = as.character(Field), 
               Repeat = as.character(Repeat)) %>% 
        filter(Treatment %in% c("BE", "FO"))

# Fit LMM
model <- glmmTMB(Root_weight ~ Treatment + (1|Field), 
                data = root, 
                family = gaussian)
summary(model)
emmeans(model, pairwise ~ Treatment)

emm <- as.data.frame(emmeans(model, pairwise ~ Treatment)$emmeans)
ggplot(emm, aes(x = Treatment, y = emmean)) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
        labs(x = "Treatment", y = "Estimated Marginal Mean of Root Weight") +
        theme_minimal()


#Compose model diagnostics
dharma <- simulateResiduals(model, plot = T)



### Same without field 5 data ###
root_no5 <- root %>% filter(Field != "5")

model_no5 <- glmmTMB(Root_weight ~ Treatment + (1|Field), 
                data = root_no5, 
                family = gaussian)
summary(model_no5)
emmeans(model_no5, pairwise ~ Treatment)

emm_no5 <- as.data.frame(emmeans(model_no5, pairwise ~ Treatment)$emmeans)
ggplot(emm_no5, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of Root Weight") +
  theme_minimal()


#Compose model diagnostics
dharma <- simulateResiduals(model_no5, plot = T)
