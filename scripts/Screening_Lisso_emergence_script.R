#LISSO EMERGENCE#


#Clean session before starting
rm(list=ls())
#clean console
cat("\014")


#Load libraries for screening the data
library(ggplot2)
library(data.table)
library(dplyr)
library(dlookr)

#Load data
Emergence <- fread("data/original/Emergence.csv", colClasses = c("Date", "character", "character", "character", "numeric", "NULL"), data.table = FALSE)

#Check the structure of the data
str(Emergence)
unique(Emergence$Taxa)


#Delete non-Lissorhoptrus from Emergence data
Lisso_abundance <- Emergence[Emergence$Taxa %in% c("L. oryzophilus"),]
str(Lisso_abundance)
distinct(Lisso_abundance, Field, Treatment)

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
########################################################################

diagnose_outlier(Lisso_abundance_complete)
plot_outlier(Lisso_abundance_complete)

#Lisso abundance per field and per field & treatment

ggplot(Lisso_abundance_complete, aes(x = Field, y = Abundance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) + 
  theme_bw() +
  labs(title = "Lisso abundance by field",
       x = "Field",
       y = "Abundance")

ggplot(Lisso_abundance_complete, aes(x = Treatment, y = Abundance, colour = Treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) + 
  facet_wrap(~ Field, scale = "free_y") +
  theme_bw() +
  labs(title = "Lisso abundance by treatment and field",
       x = "Treatment",
       y = "Abundance")


#Lisso abundance data per date

ggplot(Lisso_abundance_complete, aes(x = Date, y = Abundance, colour = Field)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Field) +
  theme_bw() +
  ylim(0, max(Lisso_abundance$Abundance) + 5) +
  labs(title = "Lisso abundance over time by field",
       x = "Date",
       y = "Abundance")
