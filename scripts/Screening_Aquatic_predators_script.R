#AQUATIC PREDATORS#


#Clean session before starting
rm(list=ls())
#clean console
cat("\014")


#Load libraries for screening the data
library(ggplot2)
library(readxl)
library(dplyr)
library(dlookr)

#Load data
Macrofauna <- fread("data/original/Macrofauna.csv", colClasses = c("Date", "character", "character", "character", "character", "character", "numeric", "NULL"), data.table = FALSE)
#Delete non-predators from Macrofauna data
Aquatic_predators <- Macrofauna[Macrofauna$Taxa %in% c(NA, "Procambarus clarkii", "Pelophylax perezi", "Carassius sp", "Mugil sp", "Pseudorasbora sp", "Epidalea calamita", "Gobio lozanoi", "Misgurnus anguillicaudatus", "Alburnus alburnus"),]
unique(Aquatic_predators$Taxa)
#Save new csv
write.csv2(Aquatic_predators, "data/modified/Aquatic_predators.csv", row.names = FALSE)

#Filtering data
Aquatic_predators2 <- Aquatic_predators %>%
  filter(!is.na(Aquatic_predators$Taxa),
         Stage != "RENAC",
         Date < "2025-06-30")
Aquatic_predators3 <- Aquatic_predators2 %>% 
  mutate(funct_group = case_when(
    Taxa %in% c("Pelophylax perezi", "Epidalea calamita") ~ "Amphibians",
    Taxa %in% c("Misgurnus anguillicaudatus", "Carassius sp", "Pseudorasbora sp",
                "Mugil sp", "Alburnus alburnus", "Gobio lozanoi" ) ~ "Fish",
    TRUE ~ "Procambarus clarkii")) %>% 
  group_by(Field, Treatment, Date, funct_group) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup() -> Aquatic_predators3

##Añadir una observacion con valor de abundacia 0 para las parcelas, taxa y tratamientos que no tienen observación en alguna de las fechas en los datos Aquatic_predators
all_combinations <- expand.grid(Field = unique(Aquatic_predators3$Field), Treatment = unique(Aquatic_predators3$Treatment), Date = unique(Aquatic_predators3$Date), funct_group=unique(Aquatic_predators3$funct_group))
Aquatic_predators_complete <- merge(all_combinations, Aquatic_predators3, by = c ("Field", "Treatment", "Date", "funct_group"), all.x = TRUE)
Aquatic_predators_complete$Abundance[is.na(Aquatic_predators_complete$Abundance )] <- 0



ggplot(Aquatic_predators_complete, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~ funct_group, scale = "free_y") +
  labs(title = "Aquatic predator abundance by treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()

Aquatic_predators_complete %>% 
  group_by(funct_group, Treatment) %>%
  summarise(Abundance_media = mean(Abundance),
            Abundance_sd = sd(Abundance))

ggplot(Aquatic_predators_complete, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~ funct_group + Field, scale = "free_y") +
  labs(title = "Aquatic predator abundance by field & treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()

ggplot(Aquatic_predators_complete, aes(x = Field, y = Abundance)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~ funct_group, scale = "free_y") +
  labs(title = "Aquatic predator abundance by field",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()
