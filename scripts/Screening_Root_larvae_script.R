#Larvae abundance and root damage#

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(data.table)
library(dlookr)

#Load data
Root_larvae <- read_csv2("data/original/Root_dmg_larvae.csv") %>% 
  mutate(Field = as.character(Field), 
         Repeat = as.character(Repeat))

diagnose_outlier(Root_larvae)
plot_outlier(Root_larvae)


#Plot larvae abundance per field
ggplot(Root_larvae, aes(x = Field, y = N_larvae)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Larvae abundance per field",
       x = "Field",
       y = "Abundance") +
  theme_bw()


#Plot larvae abundance per treatment
ggplot(Root_larvae, aes(x = Treatment, y = N_larvae)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Larvae abundance per treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()


#Plot larvae abundance per treatment per date
ggplot(Root_larvae, aes(x = Treatment, y = N_larvae)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  facet_wrap(~Date)+
  labs(title = "Larvae abundance per treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()


#Plot root weight per field
ggplot(Root_larvae, aes(x = Field, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Root weight per field",
       x = "Field",
       y = "Root weight") +
  theme_bw()


#Plot root weight per treatment
ggplot(Root_larvae, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Root weight per treatment",
       x = "Treatment",
       y = "Root weight") +
  theme_bw()


#Plot root weight per treatment per date
ggplot(Root_larvae, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  facet_wrap(~Date)+
  labs(title = "Root weight per treatment",
       x = "Treatment",
       y = "Root weight") +
  theme_bw()


#Plot larvae abundance vs root weight
ggplot(Root_larvae, aes(x = N_larvae, y = Root_weight, color = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Larvae abundance vs root weight",
       x = "Larvae abundance",
       y = "Root weight") +
  theme_bw() +
  scale_color_manual(values = c("blue", "green", "orange", "purple"))




#################### NOW USING ONLY DATA FROM FO AND BE ###################

#Filter data for BE and FO treatments
root_larvae_filt <- Root_larvae %>% 
  filter(Treatment %in% c("BE", "FO"))


#Plot larvae abundace per treatment per field
ggplot(root_larvae_filt, aes(x = Treatment, y = N_larvae)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  facet_wrap(~Field)+
  labs(title = "Larvae abundance per treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()

#Plot root weight per treatment per field
ggplot(root_larvae_filt, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  facet_wrap(~Field)+
  labs(title = "Root weight per treatment",
       x = "Treatment",
       y = "Root weight") +
  theme_bw()


#Calculate the contribution of waterbirds to larvae abundance
larvae_summ <- root_larvae_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_larvae = mean(N_larvae))

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


#Calculate the contribution of waterbirds to root weight
root_summ <- root_larvae_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_root = mean(Root_weight))

root_wide <- root_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_root
  ) %>% 
  mutate(root_contribution = (((FO-BE)/(FO)))*100)

ggplot(root_wide, aes(x = "", y = root_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to root weight (%)") +
  theme_bw() +
  coord_flip()
