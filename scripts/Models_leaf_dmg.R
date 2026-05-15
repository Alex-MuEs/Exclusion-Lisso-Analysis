#### LEAF DAMAGE ####

######## Mixed models analysis ########


rm(list = ls())
cat("\014")
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)

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

#Add column with a number per observation for the random effect of overdispersion
leaf_dmg <- leaf_dmg %>% mutate(obs = 1:n())


#### LMM binomial ####
model <- glmmTMB(cbind(Leaves_dmg_10leaves, No_dmg) ~ Treatment + julian_day + (1|Field), data = leaf_dmg, family = binomial)
summary(model)
r2(model)

Anova(model)

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)

#Plot emmeans
emmeans(model, pairwise ~ Treatment)

leaf_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment, type = "response")$emmeans)
ggplot(leaf_emm, aes(x = Treatment, y = prob)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(title = "Estimated Marginal Means of Leaf Damage by Treatment",
       x = "Treatment",
       y = "Estimated Marginal Mean of Leaf Damage") +
  theme_minimal()



#### LMM betabinomial transformed ####
model.2 <- glmmTMB(cbind(Leaves_dmg_10leaves, No_dmg) ~ Treatment + factor(julian_day) + (1|Field), data = leaf_dmg, family = betabinomial)
summary(model.2)
r2(model.2)

Anova(model.2)

#Model diagnostics
dharma <- simulateResiduals(model.2, plot = T)


model_performance(model)
model_performance(model.2)
MuMIn::model.sel(model, model.2)



######## Without field 5 ########
leaf_dmg_no5 <- leaf_dmg %>% filter(Field != 5)

model_no5 <- glmmTMB(cbind(Leaves_dmg_10leaves, No_dmg) ~ Treatment + julian_day + (1|Field), data = leaf_dmg_no5, family = binomial)
summary(model_no5)
r2(model_no5)

Anova(model_no5)

emmeans(model_no5, pairwise ~ Treatment)

leafno5_emm<-as.data.frame(emmeans(model_no5, pairwise ~ Treatment)$emmeans)
ggplot(leafno5_emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(title = "Estimated Marginal Means of Leaf Damage by Treatment",
       x = "Treatment",
       y = "Estimated Marginal Mean of Leaf Damage") +
  theme_minimal()

#Model assumptions
dharma_no5 <- simulateResiduals(model_no5, plot = T)


#Transformed model without field 5
model_no5.2 <- glmmTMB(cbind(Leaves_dmg_10leaves, No_dmg) ~ Treatment + factor(julian_day) + (1|Field), data = leaf_dmg_no5, family = betabinomial)
summary(model_no5.2)
r2(model_no5.2)

Anova(model_no5.2)

#Model diagnostics
dharma_no5.2 <- simulateResiduals(model_no5.2, plot = T)


model_performance(model_no5)
model_performance(model_no5.2)
MuMIn::model.sel(model_no5, model_no5.2)


######## Figure ########

leaf_summ <- leaf_dmg %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_dmg = mean(Leaves_dmg_10leaves/10),
            sd = sd(Leaves_dmg_10leaves/10, na.rm = TRUE),
            n = sum(!is.na(Leaves_dmg_10leaves/10)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

leaf_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment, type = "response")$emmeans)


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
    data = leaf_dmg,
    aes(x = Treatment, y = Leaves_dmg_10leaves/10, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = leaf_summ,
    aes(x = Treatment, y = mean_dmg, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = leaf_summ,
    aes(x = Treatment, y = mean_dmg, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  
  
#Global emmean layer
  
  geom_line(
    data = leaf_emm,
    aes(x = Treatment, y = prob, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = leaf_emm,
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
    data = leaf_emm,
    aes(x = Treatment, y = prob),
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
    y = "Proporción de hojas dañadas",
  ) +
  theme_minimal(base_size = 14)



######## Figure without field 5 data ######## 

leafno5_summ <- leaf_dmg_no5 %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_dmg = mean(Leaves_dmg_10leaves/10),
            sd = sd(Leaves_dmg_10leaves/10, na.rm = TRUE),
            n = sum(!is.na(Leaves_dmg_10leaves/10)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

leafno5_emm <- as.data.frame(emmeans(model_no5, pairwise ~ Treatment, type = "response")$emmeans)


ggplot() +
  geom_jitter(
    data = leaf_dmg_no5,
    aes(x = Treatment, y = Leaves_dmg_10leaves/10, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = leafno5_summ,
    aes(x = Treatment, y = mean_dmg, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = leafno5_summ,
    aes(x = Treatment, y = mean_dmg, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  
  
  #Global emmean layer
  
  geom_line(
    data = leafno5_emm,
    aes(x = Treatment, y = prob, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = leafno5_emm,
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
    data = leafno5_emm,
    aes(x = Treatment, y = prob),
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
    y = "Proporción de hojas dañadas",
  ) +
  theme_minimal(base_size = 14)
