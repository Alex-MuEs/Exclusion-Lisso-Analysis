#LEAF DAMAGE#

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
Leaf_dmg <- read_csv2("data/original/Leaf_dmg.csv") %>% 
  mutate(Field = as.character(Field), 
         Repeat = as.character(Repeat))

diagnose(Leaf_dmg)

leaf_filt <- Leaf_dmg %>% 
  filter(Treatment %in% c("BE", "FO"))


#Plot leaf dmg per field
ggplot(leaf_filt, aes(x = Field, y = Leaves_dmg_10leaves*10, fill = Treatment, color= Treatment)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Porcentaje de hojas dañadas por campo",
       x = "",
       y = "Porcentaje de hojas dañadas") +
  theme_bw()

ggplot(leaf_filt, aes(x = Field, y = Marks_5leaves/5, fill = Treatment, color= Treatment)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "",
       x = "",
       y = "Número de marcas por hoja") +
  theme_bw()
diagnose_outlier(leaf_filt, Marks_5leaves)


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
ggplot(leaf_filt, aes(x = Treatment, y = Leaves_dmg_10leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaves damaged out of 10 leaves per treatment",
       x = "Treatment",
       y = "Leaves damaged") +
  theme_bw()

ggplot(leaf_filt, aes(x = Treatment, y = Marks_5leaves)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red")+
  labs(title = "Leaf marks in 5 leaves per treatment",
       x = "Treatment",
       y = "Leaf marks") +
  theme_bw()


#Calculate the contribution of waterbirds to leaf damage
leaf_summ <- leaf_filt %>% 
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



#Plot leaf damage per date
ggplot(leaf_filt, aes(x = Date, y = Leaves_dmg_10leaves, color = Field)) +
  geom_jitter(width = 2, height = 0, alpha = 0.4) +
  geom_smooth(se = F) +
  #facet_wrap(~ Field) +
  theme_bw() +
  #theme(legend.position = "none") +
  labs(title = "Leaves damaged out of 10 leaves over time by field",
       x = "Date",
       y = "Leaves damaged")

ggplot(leaf_filt, aes(x = Date, y = Marks_5leaves, colour = Field)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Field) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Marks in 5 leaves over time by field",
       x = "Date",
       y = "Leaf marks")




########## Compare larvae and adult abundance with leaf damage over time ##########

# Cargar los datos
lisso <- read.csv2("data/modified/Lisso_abundance.csv")
larvae <- read.csv2("data/original/Larvae.csv")

# Convertir fechas
lisso$Date <- as.Date(lisso$Date)
larvae$Date <- as.Date(larvae$Date)



ggplot() +
  geom_jitter(data = lisso, aes(x = Date, y = Abundance, color = "Abundancia adultos"), alpha = 0.4, size = 2, width = 1, height = 0) +
  geom_jitter(data = larvae, aes(x = Date, y = Abundance*10, color = "Abundancia larvas x 10"), alpha = 0.4, size = 2, width = 1, height = 0) +
  geom_jitter(data = leaf_filt, aes(x = Date, y = Leaves_dmg_10leaves*10, color = "% hojas dañadas"), alpha = 0.4, size = 2, width = 1, height = 0) +
  geom_smooth(data = lisso, aes(x = Date, y = Abundance, color = "Abundancia adultos"), 
              method = "loess", se = F) +
  geom_smooth(data = larvae, aes(x = Date, y = Abundance*10, color = "Abundancia larvas x 10"), 
              method = "loess", se = F) +
  geom_smooth(data = leaf_filt, aes(x = Date, y = Leaves_dmg_10leaves*10, color = "% hojas dañadas"), 
              method = "loess", se = F) +
  scale_y_continuous(name = "") +
  scale_color_manual(values = c("Abundancia adultos" = "#F8766D",
                                "Abundancia larvas x 10" = "#619CFF",
                                "% hojas dañadas" = "#00BA38")) +
  labs(title = "Abundancia de larvas y adultos & Daño en hoja en el tiempo",
       x = "Fecha",
       color = "") +
  theme_bw()

