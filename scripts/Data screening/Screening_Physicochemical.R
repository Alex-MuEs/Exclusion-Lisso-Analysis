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



##### Assesing differences between treatments #####

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





##### Assessing differences in field 5 ####

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
#Plot Water_temp per field and treatment
ggplot(PhCh, aes(x = Field, y = Water_temp, color = Treatment)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Water Temperature per Field and Treatment") +
  theme_bw()
#Plot Water_temp per date
ggplot(PhCh, aes(x = Date, y = Water_temp, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "Water Temperature per Field Over Time") +
  theme_bw()



#Plot O2_percent per field and treatment
ggplot(PhCh, aes(x = Field, y = O2_percent, color = Treatment)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "O2 % per Field and Treatment") +
  theme_bw()
#Plot O2_percent per date
ggplot(PhCh, aes(x = Date, y = O2_percent, color = Field)) +
  geom_line() +
  facet_wrap(~Treatment) +
  labs(title = "O2 % per Field & Treatment Over Time") +
  theme_bw()
#Plot O2_percent per treatment
ggplot(PhCh, aes(x = Treatment, y = O2_percent, color = Treatment)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "O2 % per Treatment") +
  theme_bw()
#Plot O2_percent per field and treatment, facet per date
ggplot(PhCh, aes(x = Field, y = O2_percent, color = Treatment)) +
  geom_point(size = 2) +
  facet_wrap(~Date) +
  labs(title = "O2 % per Field and Date") +
  theme_bw()


#Plot O2_mgL per field
ggplot(PhCh, aes(x = Field, y = O2_mgL, color = Field)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "O2 mg/L per Field") +
  theme_bw()
#Plot O2_mgL per date
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





##### Mixed models analysis #####

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
  mutate(Date = as.factor(Date),
         Field = as.factor(Field)) %>% 
  filter(Treatment %in% c("BE", "FO"))



#Fit LMM for Soil conductivity
lm_soilcond <- lm(sqrt(Soil_cond) ~ Field+Date, data = PhCh)
summary(lm_soilcond)
r2(lm_soilcond)

Anova(lm_soilcond, type = 2)
emmeans(lm_soilcond, pairwise ~ Field)
emmeans(lm_soilcond, pairwise ~ Date)

#Model diagnostics
dharma <- simulateResiduals(lm_soilcond, plot = T)


Field_emm <- as.data.frame(emmeans(lm_soilcond, pairwise ~ Field)$emmeans)
ggplot(Field_emm, aes(x = Field, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.2))+
  labs(x = "Field", y = "Estimated Marginal Mean of Soil_cond")+
  theme_minimal()

Date_emm <- as.data.frame(emmeans(lm_soilcond, pairwise ~ Date)$emmeans)
ggplot(Date_emm, aes(x = Date, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.2))+
  labs(x = "Date", y = "Estimated Marginal Mean of Soil_cond")+
  theme_minimal()



#Fit LMM for O2

lm_O2 <- lm(log(O2_percent) ~ Field*Treatment, data = PhCh)
summary(lm_O2)
r2(lm_O2)

Anova(lm_O2, type = 3)


emmeans(lm_O2, pairwise ~ Field)
emmeans(lm_O2, pairwise ~ Treatment)
emmeans(lm_O2, pairwise ~ Treatment|Field)

#Model diagnostics
dharma <- simulateResiduals(lm_O2, plot = T)


Field_emm <- as.data.frame(emmeans(lm_O2, pairwise ~ Field)$emmeans)
ggplot(Field_emm, aes(x = Field, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.2))+
  labs(x = "Field", y = "Estimated Marginal Mean of O2")+
  theme_minimal()

Treatment_emm <- as.data.frame(emmeans(lm_O2, pairwise ~ Treatment)$emmeans)
ggplot(Treatment_emm, aes(x = Treatment, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.2))+
  labs(x = "Treatment", y = "Estimated Marginal Mean of O2")+
  theme_minimal()

Field_Treatment_emm <- as.data.frame(emmeans(lm_O2, pairwise ~ Field|Treatment)$emmeans)
ggplot(Field_Treatment_emm, aes(x = Treatment, y = emmean))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.2))+
  facet_wrap(~ Field, scales = "free_y")+
  labs(x = "Treatment", y = "Estimated Marginal Mean of O2")+
  theme_minimal()
  