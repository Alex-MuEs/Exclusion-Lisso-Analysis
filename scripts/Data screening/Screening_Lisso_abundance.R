#LISSO ABUNDANCE (net sampling)#


#Clear session
rm(list = ls(all.names = TRUE))
cat("\014")
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)


#Load libraries for screening the data
library(ggplot2)
library(data.table)
library(dplyr)
library(dlookr)

#Load data
Macroinv <- read.csv2("data/original/Macroinvertebrates.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Sampling = as.character(Sampling))

#Filter Lissorhoptrus observations and drop unnecesary columns
Lisso_abundance <- Macroinv %>% 
  filter(Genus %in% c("Lissorhoptrus"),
        Stage == "Adulto") %>% 
  select(c(Date, Field, Treatment, Stage, Total_Abundance)) %>% 
  rename(Abundance = Total_Abundance)

#Add up abundances from the same date, field and treatment
Abundance_summ <- group_by(Lisso_abundance, Date, Field, Treatment) %>% 
  summarise(Abundance = sum(Abundance))

#Add observations with abundance value 0 for the combinations of Field, treatment and date that don't exist in the data
all_combinations <- expand.grid(Field = unique(Lisso_abundance$Field), Treatment = unique(Lisso_abundance$Treatment), Date = unique(Lisso_abundance$Date))
#Unir el data frame original con el de todas las combinaciones, rellenando los valores faltantes con 0
Lisso_abundance <- merge(all_combinations, Abundance_summ, by = c ("Date", "Field", "Treatment"), all.x = TRUE)
Lisso_abundance$Abundance[is.na(Lisso_abundance$Abundance )] <- 0


