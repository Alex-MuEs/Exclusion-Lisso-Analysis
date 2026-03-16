#YIELD#

#Clear session
cat("\014")
rm(list = ls(all.names = TRUE))
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)

#Load libraries for screening the data
library(ggplot2)
library(tidyverse)
library(data.table)
library(dlookr)

#Load data
Yield <- read.csv2("data/original/Yield.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))
diagnose(Yield)
diagnose_category(Yield)
diagnose_numeric(Yield)
diagnose_outlier(Yield)

#Filter data for treatments BE and FO
yield_filt <- Yield %>%
  filter(Treatment %in% c("BE", "FO"))




#Plot yield per field
ggplot(yield_filt, aes(x = Field, y = Yield_kg.ha_HR14, fill = Treatment, color = Treatment)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Producción por campo",
       x = "",
       y = "Producción (kg/ha a 14% HR)") +
  theme_bw()


#Plot yield per treatment
ggplot(yield_filt, aes(x = Treatment, y = Yield_kg.ha_HR14)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Yield per treatment",
       x = "Treatment",
       y = "Yield (kg/ha at 14% HR)") +
  theme_bw()


#Plot yield per treatment per field
ggplot(yield_filt, aes(x = Treatment, y = Yield_kg.ha_HR14)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~Field) +
  labs(title = "Yield per treatment and field",
       x = "Treatment",
       y = "Yield (kg/ha at 14% HR)") +
  theme_bw()



#Calculate the contribution of waterbirds to yield
yield_summ <- yield_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_yield = mean(Yield_kg.ha_HR14, na.rm = TRUE))

yield_wide <- yield_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_yield
  ) %>% 
  mutate(yield_contribution = (((FO-BE)/(FO)))*100)

ggplot(yield_wide, aes(x = "", y = yield_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to yield (%)") +
  theme_bw() +
  coord_flip()
