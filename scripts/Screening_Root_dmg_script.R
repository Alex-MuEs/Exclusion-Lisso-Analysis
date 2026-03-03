#ROOT DAMAGE#

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
Root <- read.csv2("data/original/Root_dmg_tot.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeats = as.character(Repeats))
diagnose(Root)
diagnose_category(Root)
diagnose_numeric(Root)
diagnose_outlier(Root)
plot_outlier(Root)

#Filter data for treatments BE and FO, and root weights less than 4, as these are outliers in the data.
root_filt <- Root %>%
  filter(Treatment %in% c("BE", "FO"),
         Root_weight < 4)




#Plot root weight by field
ggplot(root_filt, aes(x = Field, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Root Weight by Field",
       x = "Field",
       y = "Root Weight (g)") +
  theme_bw()


#Plot root weight by treatment
ggplot(root_filt, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Root Weight by Treatment",
       x = "Treatment",
       y = "Root Weight (g)") +
  theme_bw()


#Plot root weight by field and treatment
ggplot(root_filt, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  facet_wrap(~ Field) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Root Weight by Field and Treatment",
       x = "Field",
       y = "Root Weight (g)") +
  theme_bw()




#Calculate the contribution of waterbirds to root weight
root_summ <- root_filt %>% 
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
       y = "Contribution of waterbirds to root biomass (%)") +
  theme_bw() +
  coord_flip()


ggplot(root_filt, aes(x = Field, y = Root_weight, fill = Treatment, colour = Treatment)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Root Weight by Field and Treatment",
       x = "Field",
       y = "Root Weight (g)") +
  theme_bw() 




########### DATA ONLY FROM THE 20 REPS SAMPLING ###########

#Load data
Root_20 <- read.csv2("data/original/Root_dmg_20.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))
diagnose(Root_20)
diagnose_category(Root_20)
diagnose_numeric(Root_20)
diagnose_outlier(Root_20)
plot_outlier(Root_20)

#Filter data for treatments BE and FO, and root weights less than 2.5, as these are outliers in the data.
root20_filt <- Root_20 %>%
  filter(Treatment %in% c("BE", "FO"),
         Root_weight < 2.5)




#Plot root weight by field
ggplot(root20_filt, aes(x = Field, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Root Weight by Field",
       x = "Field",
       y = "Root Weight (g)") +
  theme_bw()


#Plot root weight by treatment
ggplot(root20_filt, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Root Weight by Treatment",
       x = "Treatment",
       y = "Root Weight (g)") +
  theme_bw()


#Plot root weight by field and treatment
ggplot(root20_filt, aes(x = Treatment, y = Root_weight)) +
  geom_jitter(width = 0.05, height = 0) +
  facet_wrap(~ Field) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Root Weight by Field and Treatment",
       x = "Field",
       y = "Root Weight (g)") +
  theme_bw()




#Calculate the contribution of waterbirds to root weight
root20_summ <- root20_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_root = mean(Root_weight))

root20_wide <- root20_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_root
  ) %>% 
  mutate(root_contribution = (((FO-BE)/(FO)))*100)

ggplot(root20_wide, aes(x = "", y = root_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = Field), vjust = -0.5, size = 5) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to root biomass (%)") +
  theme_bw() +
  coord_flip()


ggplot(root20_filt, aes(x = Field, y = Root_weight, fill = Treatment, colour = Treatment)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Root Weight by Field and Treatment",
       x = "Field",
       y = "Root Weight (g)") +
  theme_bw() 
