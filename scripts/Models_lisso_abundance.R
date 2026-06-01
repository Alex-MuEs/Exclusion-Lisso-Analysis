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



#### Comparing models ####
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
dharma <- simulateResiduals(model.final, plot = T)

#Estimated marginal means comparison plots
emmeans(model.final, pairwise~Treatment)
emmeans(model.final, pairwise~Treatment|Date)

model_emm <- as.data.frame(emmeans(model.final, pairwise ~ Treatment)$emmeans)
ggplot(model_emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of log(Abundance+1)") +
  theme_minimal()
model_emm.2 <- as.data.frame(emmeans(model.final, pairwise ~ Treatment|Date)$emmeans)
ggplot(model_emm.2, aes(x = Date, y = emmean, color = Treatment)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of log(Abundance+1)") +
  theme_minimal()




#### FIGURE TREATMENT ####

lisso_summ <- Lisso %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_dmg = mean(Abundance),
            sd = sd(Abundance, na.rm = TRUE),
            n = sum(!is.na(Abundance)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

lisso_emm <- as.data.frame(emmeans(model.poi, pairwise ~ Treatment, type = "response")$emmeans)


pd_jitter <- position_jitter(width = 0.07, height = 0)
pd_dodge  <- position_dodge(width = 0.15)
pal_cb <- c(
  "#0072B2", # azul
  "#D55E00", # rojo/naranja
  "#009E73", # verde
  "#CC79A7", # morado
  "#E69F00"  # amarillo/naranja
)


ggplot() +
  geom_jitter(
    data = Lisso,
    aes(x = Treatment, y = Abundance, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = lisso_summ,
    aes(x = Treatment, y = mean_dmg, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = lisso_summ,
    aes(x = Treatment, y = mean_dmg, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +

  
  #Global emmean layer
  
  geom_line(
    data = lisso_emm,
    aes(x = Treatment, y = rate, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = lisso_emm,
    aes(x = Treatment,
        ymin = asymp.LCL,
        ymax = asymp.UCL,
        group = 1),
    inherit.aes = FALSE,
    width = 0.08,
    linewidth = 1.1,
    color = "black"
  ) +
  
  geom_point(
    data = lisso_emm,
    aes(x = Treatment, y = rate),
    inherit.aes = FALSE,
    size = 4.8,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 1.2
  ) +
  scale_x_discrete(labels = c("BE" = "No", "FO" = "Yes")) +
  scale_color_manual(values = pal_cb, name = "Field") +
  labs(
    x = "Birds",
    y = "Adult abundance",
  ) +
  theme_minimal(base_size = 14)



######## FIGURE DATE ########

lisso_summ_date <- Lisso %>% 
  group_by(Field, Treatment, Date) %>% 
  summarise(mean_dmg = mean(Abundance),
            sd = sd(Abundance, na.rm = TRUE),
            n = sum(!is.na(Abundance)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

lisso_emm_date <- as.data.frame(emmeans(model.poi, pairwise ~ Treatment|Date, type = "response")$emmeans)


ggplot() +
  geom_jitter(
    data = Lisso,
    aes(x = Treatment, y = Abundance, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = lisso_summ_date,
    aes(x = Treatment, y = mean_dmg, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = lisso_summ_date,
    aes(x = Treatment, y = mean_dmg, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  facet_wrap(~Date) +
  
  
  #Global emmean layer
  
  geom_line(
    data = lisso_emm_date,
    aes(x = Treatment, y = rate, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = lisso_emm_date,
    aes(x = Treatment,
        ymin = asymp.LCL,
        ymax = asymp.UCL,
        group = 1),
    inherit.aes = FALSE,
    width = 0.08,
    linewidth = 1.1,
    color = "black"
  ) +
  
  geom_point(
    data = lisso_emm_date,
    aes(x = Treatment, y = rate),
    inherit.aes = FALSE,
    size = 4.8,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 1.2
  ) +
  scale_x_discrete(labels = c("BE" = "No", "FO" = "Yes")) +
  scale_color_manual(values = pal_cb, name = "Experimental plot") +
  labs(
    x = "Birds",
    y = "Abundancia de adultos",
  ) +
  theme_minimal(base_size = 14)
