#### Root biomass ####
######## Mixed models analysis ########


rm(list = ls())

# Load packages
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(performance)
library(car)


# Load data
root <- read.csv2("data/original/Root_dmg_tot.csv") %>% 
        mutate(Date = as.POSIXct(Date),
               Field = as.factor(Field), 
               Repeats = as.character(Repeats)) %>% 
        filter(Treatment %in% c("BE", "FO"),
               Date == "2025-06-30",
               Root_weight < 3) #Remove one outlier with weight value higher than 6


#### Fit LMM ####
check_distribution(model)

model <- glmmTMB(Root_weight ~ Treatment + (1|Field), 
                data = root, 
                family = Gamma (link = "log"))

summary(model)
r2(model)

Anova(model, type = 2)

emmeans(model, pairwise ~ Treatment)

#Compose model diagnostics
dharma <- simulateResiduals(model, plot = T)
model_performance(model)


root_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment)$emmeans)
ggplot(root_emm, aes(x = Treatment, y = emmean)) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
        labs(x = "Treatment", y = "Estimated Marginal Mean of Root Weight") +
        theme_minimal()





#### LMM without field 5 data ####
root_no5 <- root %>% filter(Field != "5")

model_no5 <- glmmTMB(sqrt(Root_weight) ~ Treatment + (1|Field), 
                data = root_no5, 
                family = gaussian)
summary(model_no5)
r2(model_no5)
emmeans(model_no5, pairwise ~ Treatment)


#Model diagnostics
dharma <- simulateResiduals(model_no5, plot = T)
model_performance(model_no5)


rootno5_emm <- as.data.frame(emmeans(model_no5, pairwise ~ Treatment)$emmeans)
ggplot(rootno5_emm, aes(x = Treatment, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Treatment", y = "Estimated Marginal Mean of Root Weight") +
  theme_minimal()





#### Figure ####

root_summ <- root %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_weight = mean(Root_weight),
            sd = sd(Root_weight, na.rm = TRUE),
            n = sum(!is.na(Root_weight)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

root_emm <- as.data.frame(emmeans(model, pairwise ~ Treatment)$emmeans)


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
    data = root,
    aes(x = Treatment, y = Root_weight, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = root_summ,
    aes(x = Treatment, y = mean_weight, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = root_summ,
    aes(x = Treatment, y = mean_weight, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  
  
  #Global emmean layer
  
  geom_line(
    data = root_emm,
    aes(x = Treatment, y = emmean^2, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = root_emm,
    aes(x = Treatment,
        ymin = asymp.LCL^2,
        ymax = asymp.UCL^2,
        group = 1),
    inherit.aes = FALSE,
    width = 0.08,
    linewidth = 1.1,
    color = "black"
  ) +
  
  geom_point(
    data = root_emm,
    aes(x = Treatment, y = emmean^2),
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
    y = "Root weight (g)",
  ) +
  theme_minimal(base_size = 14)



#### Figure without field 5 data ####

rootno5_summ <- root_no5 %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_weight = mean(Root_weight),
            sd = sd(Root_weight, na.rm = TRUE),
            n = sum(!is.na(Root_weight)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

rootno5_emm <- as.data.frame(emmeans(model_no5, pairwise ~ Treatment)$emmeans)


ggplot() +
  geom_jitter(
    data = root_no5,
    aes(x = Treatment, y = Root_weight, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = rootno5_summ,
    aes(x = Treatment, y = mean_weight, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = rootno5_summ,
    aes(x = Treatment, y = mean_weight, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  
  
  #Global emmean layer
  
  geom_line(
    data = rootno5_emm,
    aes(x = Treatment, y = emmean^2, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = rootno5_emm,
    aes(x = Treatment,
        ymin = asymp.LCL^2,
        ymax = asymp.UCL^2,
        group = 1),
    inherit.aes = FALSE,
    width = 0.08,
    linewidth = 1.1,
    color = "black"
  ) +
  
  geom_point(
    data = rootno5_emm,
    aes(x = Treatment, y = emmean^2),
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
    y = "Root weight (g)",
  ) +
  theme_minimal(base_size = 14)
