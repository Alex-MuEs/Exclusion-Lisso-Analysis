#LEAF DAMAGE#

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(data.table)
library(dlookr)

#Load data
Leaf_dmg <- read_csv2("data/original/Leaf_dmg.csv") %>% 
  mutate(Field = as.character(Field), 
         Repeat = as.character(Repeat))

diagnose(Leaf_dmg)

leaf_filt <- Leaf_dmg %>% 
  filter(Treatment %in% c("BE", "FO"))


#Plot leaf dmg per field
ggplot(leaf_filt, aes(x = Field, y = Leaves_dmg_10leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaves damaged out of 10 leaves per field",
       x = "Field",
       y = "Leaves damaged") +
  theme_bw()

ggplot(leaf_filt, aes(x = Field, y = Marks_5leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaf marks in 5 leaves per field",
       x = "Field",
       y = "Leaf marks") +
  theme_bw()
diagnose_outlier(leaf_filt, Marks_5leaves)

#Filtrar observaciones con valor de Marks_5leaves mayor a 40
leaf_filt2 <- leaf_filt %>% 
  filter(Marks_5leaves <= 40)

ggplot(leaf_filt2, aes(x = Field, y = Marks_5leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaf marks in 5 leaves per field",
       x = "Field",
       y = "Leaf marks") +
  theme_bw()

#Relationship with Lisso abundance (emergence)?
lisso_abundance <- read_csv2("data/modified/Lisso_abundance.csv")
lisso_filt <- lisso_abundance %>% 
  filter(Treatment %in% c("BE", "FO"))

ggplot(lisso_abundance, aes(x = Field, y = Abundance)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Lissorhoptrus emergence per field",
       x = "Field",
       y = "Abundance") +
  theme_bw()



#Plot leaf dmg per treatment
ggplot(leaf_filt2, aes(x = Treatment, y = Leaves_dmg_10leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaves damaged out of 10 leaves per treatment",
       x = "Treatment",
       y = "Leaves damaged") +
  theme_bw()

ggplot(leaf_filt2, aes(x = Treatment, y = Marks_5leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaf marks in 5 leaves per treatment",
       x = "Treatment",
       y = "Leaf marks") +
  theme_bw()


#Calculate the contribution of waterbirds to leaf damage
leaf_summ <- leaf_filt2 %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_dmgleaves = mean(Leaves_dmg_10leaves, na.rm = TRUE),
            mean_marks = mean(Marks_5leaves, na.rm = TRUE))
#Proportion of marked leaves
dmgleaves_wide <- leaf_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_dmgleaves
  ) %>% 
  mutate(dmgleaves_contribution = (((BE-FO)/(BE)))*100)

ggplot(dmgleaves_wide, aes(x = "", y = dmgleaves_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to prevent marked leaves (%)") +
  theme_bw() +
  coord_flip()

#Number of marks
marks_wide <- leaf_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_marks
  ) %>% 
  mutate(marks_contribution = (((BE-FO)/(BE)))*100)

ggplot(marks_wide, aes(x = "", y = marks_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to prevent feeding marks (%)") +
  theme_bw() +
  coord_flip()
