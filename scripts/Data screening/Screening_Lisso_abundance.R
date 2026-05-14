#LISSO ABUNDANCE (net sampling)#


#Clear session
rm(list = ls(all.names = TRUE))
cat("\014")
graphics.off()
pacman::p_unload(pacman::p_loaded(), character.only = TRUE, force = TRUE)


#Load libraries for screening the data
library(ggplot2)
library(data.table)
library(tidyverse)
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
Added_abundance <- group_by(Lisso_abundance, Date, Field, Treatment) %>% 
  summarise(Abundance = sum(Abundance))

#Add observations with abundance value 0 for the combinations of Field, treatment and date that don't exist in the data
all_combinations <- expand.grid(Field = unique(Lisso_abundance$Field), Treatment = unique(Lisso_abundance$Treatment), Date = unique(Lisso_abundance$Date))
#Join original df with all_combinations df, filling the missing abundances with "0"
Lisso_abundance <- merge(all_combinations, Added_abundance, by = c ("Date", "Field", "Treatment"), all.x = TRUE)
Lisso_abundance$Abundance[is.na(Lisso_abundance$Abundance )] <- 0

#Save modified csv and load it
write.csv2(Lisso_abundance, "data/modified/Lisso_abundance.csv", row.names = FALSE)
Lisso_abundance <- read.csv2("data/modified/Lisso_abundance.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field))




#Plot abundance per field and treatment
ggplot(Lisso_abundance, aes(x = Field, y = Abundance, fill = Treatment, color = Treatment)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, stroke = 1.5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, size  = 1) +
  labs(title = "Lissorhoptrus adult abundance per field & treatment",
       x = "Field",
       y = "Nº of adults") +
  theme_bw()


#Plot abundance per treatment per field
ggplot(Lisso_abundance, aes(x = Date, y = Abundance, color = Treatment)) +
  geom_jitter(width = 0.1, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.15) +
  facet_wrap(~Field) +
  labs(title = "Abundance per treatment and field",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()


#Plot abundance per treatment
ggplot(Lisso_abundance, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Lissorhoptrus adult abundance per treatment",
       x = "Treatment",
       y = "Nº of adults") +
  theme_bw()


#Plot lisso abundance per date
ggplot(Lisso_abundance, aes(x = Date, y = Abundance, color = Treatment)) +
  geom_smooth() +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 2) +
  facet_wrap(~Field) +
  labs(title = "Lissorhoptrus adult abundace over time",
       x = "Date",
       y = "Nº of adults") +
  theme_bw()


#Calculate the contribution of waterbirds to lisso adult abundance
Abundance_summ <- Lisso_abundance %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE))

Abundance_wide <- Abundance_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_abundance
  ) %>% 
  mutate(abundance_contribution = (((BE-FO)/(BE)))*100)

ggplot(Abundance_wide, aes(x = "", y = abundance_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  labs(x = "",
       y = "Contribution of waterbirds to Lissorhoptrus adult abundance (%)") +
  theme_bw() +
  coord_flip()
