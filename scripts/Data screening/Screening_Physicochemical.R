######## PHYSICOCHEMICAL DATA SCREENING ########


#Clear session
rm(list = ls(all.names = TRUE))
cat("\014")
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)


#Load libraries for screening the data
library(ggplot2)
library(data.table)
library(tidyverse)
library(dlookr)

#Load data
PhCh <- read.csv2("data/original/Physicochemical.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field)) %>% 
  filter(Treatment %in% c("BE", "FO"))



#Assesing differences between treatments

#Soil_cond per treatment and field
ggplot(PhCh, aes(x = Treatment, y = Soil_cond)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "Soil Conductivity per Treatment") +
  theme_bw()

#Plot Soil_temp per Treatment and field
ggplot(PhCh, aes(x = Treatment, y = Soil_temp)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "Soil Temperature per Treatment") +
  theme_bw()

#Plot Water_temp per Treatment and field
ggplot(PhCh, aes(x = Treatment, y = Water_temp)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "Water Temperature per Treatment") +
  theme_bw()

#Plot O2_percent per Treatment and field
ggplot(PhCh, aes(x = Treatment, y = O2_percent)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "O2_percent per Treatment") +
  theme_bw()

#Plot O2_mgL per Treatment and field
ggplot(PhCh, aes(x = Treatment, y = O2_mgL)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "O2_mgL per Treatment") +
  theme_bw()

#Plot Salinity per Treatment and field
ggplot(PhCh, aes(x = Treatment, y = Salinity)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "Salinity per Treatment") +
  theme_bw()

#Plot pH per Treatment and field
ggplot(PhCh, aes(x = Treatment, y = pH)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  facet_wrap(~Field) +
  labs(title = "pH per Treatment") +
  theme_bw()





#Assessing differences in field 5#

#Plot Soil_cond per field
ggplot(PhCh, aes(x = Field, y = Soil_cond, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Soil Conductivity per Field") +
  theme_bw()
#Plot Soil_cond per date
ggplot(PhCh, aes(x = Date, y = Soil_cond, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "Soil Conductivity per Field Over Time") +
  theme_bw()



#Plot Soil_temp per field
ggplot(PhCh, aes(x = Field, y = Soil_temp, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Soil Temperature per Field") +
  theme_bw()
#Plot Soil_temp per date
ggplot(PhCh, aes(x = Date, y = Soil_temp, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "Soil Temperature per Field Over Time") +
  theme_bw()



#Plot Water_temp per field
ggplot(PhCh, aes(x = Field, y = Water_temp, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Water Temperature per Field") +
  theme_bw()
#Plot Water_temp per date
ggplot(PhCh, aes(x = Date, y = Water_temp, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "Water Temperature per Field Over Time") +
  theme_bw()



#Plot O2_percent per field
ggplot(PhCh, aes(x = Field, y = O2_percent, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "O2 % per Field") +
  theme_bw()
#Plot Water_temp per date
ggplot(PhCh, aes(x = Date, y = O2_percent, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "O2 % per Field Over Time") +
  theme_bw()


#Plot O2_mgL per field
ggplot(PhCh, aes(x = Field, y = O2_mgL, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "O2 mg/L per Field") +
  theme_bw()
#Plot Water_temp per date
ggplot(PhCh, aes(x = Date, y = O2_mgL, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "O2 mg/L per Field Over Time") +
  theme_bw()



#Plot Salinity per field
ggplot(PhCh, aes(x = Field, y = Salinity, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Salinity per Field") +
  theme_bw()
#Plot Salinity per date
ggplot(PhCh, aes(x = Date, y = Salinity, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "Salinity per Field Over Time") +
  theme_bw()



#Plot pH per field
ggplot(PhCh, aes(x = Field, y = pH, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "pH per Field") +
  theme_bw()
#Plot pH per date
ggplot(PhCh, aes(x = Date, y = pH, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "pH per Field Over Time") +
  theme_bw()





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
PhCh <- read.csv2("data/original/Physicochemical.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.factor(Field)) %>% 
  filter(Treatment %in% c("BE", "FO"))


#Fit LMM
model <- glmmTMB(Soil_cond ~ Field, data = PhCh, family = gaussian)
summary(model)
r2(model)

Anova(model)

emmeans(model, pairwise ~ Field)

Soil_cond_emm <- as.data.frame(emmeans(model, pairwise ~Field)$emmeans)
ggplot(Soil_cond_emm, aes(x = Field, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.2))+
  labs(x = "Field", y = "Estimated Marginal Mean of Soil_cond")+
  theme_minimal()

#Model diagnostics
dharma <- simulateResiduals(model, plot = T)
