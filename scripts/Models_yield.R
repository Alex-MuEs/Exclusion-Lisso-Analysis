#### YIELD ####

######## Mixed models analysis ########

rm(list = ls())

#Load libraries
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(performance)
library(car)

#Load data
Yield <- read_csv2("data/original/Yield.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.factor(Field), 
         Repeat = as.character(Repeat)) %>% 
  rename(Yield = `Yield_kg/ha_HR14`) %>%
  filter(Treatment %in% c("BE", "FO"))

#Fit LMM
model <- glmmTMB(sqrt(Yield) ~ Treatment + (1|Field), data = Yield, family = gaussian)
summary(model)
r2(model)

Anova(model)

emmeans(model, pairwise ~ Treatment)

yield_emm <- as.data.frame(emmeans(model, pairwise ~Treatment)$emmeans)
ggplot(yield_emm, aes(x = Treatment, y = emmean))+
         geom_point(size = 2)+
         geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, width = 0.2))+
         labs(x = "Treatment", y = "Estimated Marginal Mean of Yield (kg/ha)")+
         theme_minimal()

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)


######## Mixxed model without field 5 data ######## 
Yield_no5 <- Yield %>% 
  filter(Field != "5")

model_no5 <- glmmTMB(sqrt(Yield) ~ Treatment + (1|Field), data = Yield_no5, family = gaussian)
summary(model_no5)
r2(model_no5)

Anova(model_no5)

emmeans(model_no5, pairwise ~ Treatment)

yieldno5_emm <- as.data.frame(emmeans(model_no5, pairwise ~Treatment)$emmeans)
ggplot(yieldno5_emm, aes(x = Treatment, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, width = 0.2))+
  labs(x = "Treatment", y = "Estimated Marginal Mean of Yield (kg/ha)")+
  theme_minimal()

#Model diagnostics
dharma_no5 <- simulateResiduals(model_no5, plot = T)




######## Figure ######## 

yield_summ <- Yield %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_yield = mean(Yield),
            sd = sd(Yield, na.rm = TRUE),
            n = sum(!is.na(Yield)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

yield_emm <- as.data.frame(emmeans(model, pairwise ~Treatment)$emmeans)


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
    data = Yield,
    aes(x = Treatment, y = Yield, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = yield_summ,
    aes(x = Treatment, y = mean_yield, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = yield_summ,
    aes(x = Treatment, y = mean_yield, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  
  
#Global emmean layer
  
geom_line(
  data = yield_emm,
  aes(x = Treatment, y = emmean^2, group = 1),
  inherit.aes = FALSE,
  linewidth = 1.6,
  color = "black"
) +
  
  geom_errorbar(
    data = yield_emm,
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
    data = yield_emm,
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
    y = "Yield (Kg/Ha)",
  ) +
  theme_minimal(base_size = 14)



######## Figure without field 5 data ######## 

yieldno5_summ <- Yield_no5 %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_yield = mean(Yield),
            sd = sd(Yield, na.rm = TRUE),
            n = sum(!is.na(Yield)),
            se = sd / sqrt(n),
            .groups = "drop"
  )

yieldno5_emm <- as.data.frame(emmeans(model_no5, pairwise ~Treatment)$emmeans)


ggplot() +
  geom_jitter(
    data = Yield_no5,
    aes(x = Treatment, y = Yield, color = as.factor(Field)),
    width = 0.07, height = 0,
    alpha = 0.30, size = 2.5
  ) +
  geom_line(
    data = yieldno5_summ,
    aes(x = Treatment, y = mean_yield, group = as.factor(Field), color = as.factor(Field)),
    linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    data = yieldno5_summ,
    aes(x = Treatment, y = mean_yield, color = as.factor(Field)),
    size = 4,
    alpha = 0.5
  ) +
  
  
#Global emmean layer
  
  geom_line(
    data = yieldno5_emm,
    aes(x = Treatment, y = emmean^2, group = 1),
    inherit.aes = FALSE,
    linewidth = 1.6,
    color = "black"
  ) +
  
  geom_errorbar(
    data = yieldno5_emm,
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
    data = yieldno5_emm,
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
    y = "Yield (Kg/Ha)",
  ) +
  theme_minimal(base_size = 14)
