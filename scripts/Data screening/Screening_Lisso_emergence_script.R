#LISSO EMERGENCE#


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
Emergence <- fread("data/original/Emergence.csv", colClasses = c("Date", "character", "character", "character", "numeric", "NULL"), data.table = FALSE)

#Check the structure of the data
str(Emergence)
unique(Emergence$Taxa)


#Delete non-Lissorhoptrus from Emergence data
Lisso_abundance <- Emergence[Emergence$Taxa %in% c("L. oryzophilus"),]
str(Lisso_abundance)

write.csv2(Lisso_abundance, "data/modified/Lisso_abundance.csv", row.names = FALSE)

Lisso_abundance <- read.csv2("data/modified/Lisso_abundance.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field))


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

#Filter observations with Abundance > 90
Lisso_abundance_complete <- Lisso_abundance_complete %>%
  filter(Abundance < 90)

#Mean abundance per field
Lisso_abundance_complete %>% 
  group_by(Field) %>%
  summarise(Abundance_mean = mean(Abundance),
            Abundance_sd = sd(Abundance))



#Lisso abundance per field and per field & treatment

ggplot(Lisso_abundance_complete, aes(x = Field, y = Abundance)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  theme_bw() +
  labs(title = "Lisso abundance by field",
       x = "Field",
       y = "Abundance")

ggplot(Lisso_abundance_complete, aes(x = Treatment, y = Abundance, colour = Treatment)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~ Field, scale = "free_y") +
  theme_bw() +
  labs(title = "Lisso abundance by treatment and field",
       x = "Treatment",
       y = "Abundance")


#Lisso abundance data per date

ggplot(Lisso_abundance_complete, aes(x = Date, y = Abundance)) +
  geom_point() +
  geom_smooth() +
  #facet_wrap(~ Field) +
  theme_bw() +
  ylim(0, max(Lisso_abundance$Abundance) + 5) +
  labs(title = "Lisso abundance over time by field",
       x = "Date",
       y = "Abundance")




############## Datos filtrados para tratamientos BE y FO ##############

#Filter data for treatments BE and FO
Lisso_abundance_filt <- Lisso_abundance_complete %>%
  filter(Treatment %in% c("BE", "FO"))


#Lisso abundance per field and per field & treatment

ggplot(Lisso_abundance_filt, aes(x = Field, y = Abundance)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  theme_bw() +
  labs(title = "Lisso abundance by field",
       x = "Field",
       y = "Abundance")

ggplot(Lisso_abundance_filt, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~ Field, scale = "free_y") +
  theme_bw() +
  labs(title = "Lisso abundance by treatment and field",
       x = "Treatment",
       y = "Abundance")


#Lisso abundance data per date

ggplot(Lisso_abundance_filt, aes(x = Date, y = Abundance)) +
  geom_point() +
  geom_smooth() +
  #facet_wrap(~ Field) +
  theme_bw() +
  ylim(0, max(Lisso_abundance$Abundance) + 5) +
  labs(title = "Lisso abundance over time by field",
       x = "Date",
       y = "Abundance")
