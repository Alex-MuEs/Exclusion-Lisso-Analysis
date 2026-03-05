#LARVAE ABUNDANCE#

#Clear session
rm(list = ls(all.names = TRUE))
cat("\014")
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)


library(ggplot2)
library(tidyverse)
library(data.table)
library(dlookr)

#Load data
Larvae <- read_csv2("data/original/Larvae.csv") %>% 
  mutate(Field = as.character(Field), 
         Repeat = as.character(Repeat))
#Filter data for BE and FO treatments
larvae_filt <- Larvae %>% 
  filter(Treatment %in% c("BE", "FO"))

diagnose_outlier(larvae_filt)
plot_outlier(larvae_filt)


#Plot larvae abundance per field
ggplot(larvae_filt, aes(x = Field, y = Abundance)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 2, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Larvae abundance per Field",
       x = "Field",
       y = "Abundance") +
  theme_bw()


#Plot larvae abundance per treatment
ggplot(larvae_filt, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 2, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Larvae abundance per Treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()


#Plot larvae abundance per treatment and field
ggplot(larvae_filt, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.2, height = 0) +
  facet_wrap(~Field) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 2, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Larvae abundance per Treatment and Field",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()



#Calculate the contribution of waterbirds to larvae abundance
larvae_summ <- larvae_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_larvae = mean(Abundance, na.rm = T))

larvae_wide <- larvae_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_larvae
  ) %>% 
  mutate(larvae_contribution = (((BE-FO)/(BE)))*100)

ggplot(larvae_wide, aes(x = "", y = larvae_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to larvae control (%)") +
  theme_bw() +
  coord_flip()






########### Separate data by sampling method (core v. whole plant) ############
