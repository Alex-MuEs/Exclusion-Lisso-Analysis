#Clean session before starting
rm(list=ls())

#Load libraries for screening the data
library(ggplot2)
library(readxl)
library(dplyr)
library(dlookr)

#Load the data from data folder
Emergence <- read_excel("data/original/Emergence.xlsx", col_types = c("date", "text", "text", "text", "numeric", "skip"))
Leaf_dmg <- read_excel("data/original/Leaf_dmg.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "numeric", "skip"))
Larvae <- read_excel("data/original/Larvae.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "skip"))
Root_dmg20 <- read_excel("data/original/Root_dmg_20.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "skip"))
Root_dmg_larvae <- read_excel("data/original/Root_dmg_larvae.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "numeric", "skip"))
Macrofauna <- read_excel("data/original/Macrofauna.xlsx", col_types = c("date", "text", "text", "text", "text", "text", "numeric", "skip"))
Yield <- read_excel("data/original/Yield.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip"))

#Check the structure of the data
str(Emergence)
unique(Emergence$Taxa)
str(Leaf_dmg)
str(Larvae)
str(Root_dmg20)
str(Root_dmg_larvae)
str(Macrofauna)
unique(Macrofauna$Taxa)
unique(Macrofauna$Stage)
str(Yield)

#Delete non-predators from Macrofauna data
Aquatic_predators <- Macrofauna[Macrofauna$Taxa %in% c(NA, "Procambarus clarkii", "Pelophylax perezi", "Carassius sp", "Mugil sp", "Pseudorasbora sp", "Epidalea calamita", "Gobio lozanoi", "Misgurnus anguillicaudatus", "Alburnus alburnus"),]
str(Aquatic_predators)
distinct(Aquatic_predators, Field, Treatment)
#Delete non-Lissorhoptrus from Emergence data
Lisso_abundance <- Emergence[Emergence$Taxa %in% c("L. oryzophilus"),]
str(Lisso_abundance)
distinct(Lisso_abundance, Field, Treatment)

write.csv2(Aquatic_predators, "data/modified/Aquatic_predators.csv", row.names = FALSE)
write.csv2(Lisso_abundance, "data/modified/Lisso_abundance.csv", row.names = FALSE)


#Screening Lisso abundance data
########################################################################
#Añadir una observacion con valor de abundacia 0 para las parcelas y tratamientos que no tienen observación en alguna de las fechas en los datos Lisso_abundance
#Crear un data frame con todas las combinaciones de Field, Treatment y Date
all_combinations <- expand.grid(Field = unique(Lisso_abundance$Field), Treatment = unique(Lisso_abundance$Treatment), Date = unique(Lisso_abundance$Date))
#Unir el data frame original con el de todas las combinaciones, rellenando los valores faltantes con 0
Lisso_abundance_complete <- merge(all_combinations, Lisso_abundance, by = c ("Field", "Treatment", "Date"), all.x = TRUE)
Lisso_abundance_complete$Abundance[is.na(Lisso_abundance_complete$Abundance )] <- 0
Lisso_abundance_complete$Taxa[is.na(Lisso_abundance_complete$Taxa )] <- "L. oryzophilus"

diagnose_numeric(Lisso_abundance)
diagnose_numeric(Lisso_abundance_complete)
distinct(Lisso_abundance_complete, Field, Treatment, Date)
########################################################################

diagnose_outlier(Lisso_abundance)
plot_outlier(Lisso_abundance)

ggplot(Lisso_abundance, aes(x = Field, y = Abundance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) + 
  theme_bw() +
  labs(title = "Lisso abundance by field",
       x = "Field",
       y = "Abundance")

ggplot(Lisso_abundance_complete, aes(x = Field, y = Abundance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) + 
  theme_bw() +
  labs(title = "Lisso abundance by field",
       x = "Field",
       y = "Abundance")

ggplot(Lisso_abundance, aes(x = Treatment, y = Abundance, colour = Treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) + 
  facet_wrap(~ Field) +
  theme_bw() +
  labs(title = "Lisso abundance by treatment and field",
       x = "Treatment",
       y = "Abundance")

ggplot(Lisso_abundance_complete, aes(x = Treatment, y = Abundance, colour = Treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) + 
  facet_wrap(~ Field) +
  theme_bw() +
  labs(title = "Lisso abundance by treatment and field",
       x = "Treatment",
       y = "Abundance")

#Lisso abundance data per date
ggplot(Lisso_abundance, aes(x = Date, y = Abundance, colour = Field)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Field) +
  theme_bw() +
  ylim(0, max(Lisso_abundance$Abundance) + 5) +
  labs(title = "Lisso abundance over time by field",
       x = "Date",
       y = "Abundance")

ggplot(Lisso_abundance_complete, aes(x = Date, y = Abundance, colour = Field)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Field) +
  theme_bw() +
  ylim(0, max(Lisso_abundance$Abundance) + 5) +
  labs(title = "Lisso abundance over time by field",
       x = "Date",
       y = "Abundance")
