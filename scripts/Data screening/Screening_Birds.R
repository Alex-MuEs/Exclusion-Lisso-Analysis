#BIRDS (CAMERATRAPS)#

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
Birds <- read.csv2("data/original/Birds.csv") %>% 
  unite(Scientific, Genus, Species, sep = " ", remove = FALSE)



#Plot Plegadis falcinellus abundance index per field
filter(Birds, Scientific == "Plegadis falcinellus") %>% 
  ggplot(aes(x = Field, y = Abundance_Index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Field", y = "Abundance Index", title = "Plegadis falcinellus abundance index per field")

#Plot Chroicocephalus ridibundus abundance index per field
filter(Birds, Scientific == "Chroicocephalus ridibundus") %>% 
  ggplot(aes(x = Field, y = Abundance_Index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Field", y = "Abundance Index", title = "Chroicocephalus ridibundus abundance index per field")

#Plot Gallinula chloropus abundance index per field
filter(Birds, Scientific == "Gallinula chloropus") %>%
  ggplot(aes(x = Field, y = Abundance_Index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Field", y = "Abundance Index", title = "Gallinula chloropus abundance index per field")

#Plot Himantopus himantopus abundance index per field
filter(Birds, Scientific == "Himantopus himantopus") %>%
  ggplot(aes(x = Field, y = Abundance_Index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Field", y = "Abundance Index", title = "Himantopus himantopus abundance index per field")

#Plot Larus melanocephalus abundance index per field
filter(Birds, Scientific == "Larus melanocephalus") %>%
  ggplot(aes(x = Field, y = Abundance_Index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Field", y = "Abundance Index", title = "Larus melanocephalus abundance index per field")

#Plot Larus michahellis abundance index per field
filter(Birds, Scientific == "Larus michahellis") %>%
  ggplot(aes(x = Field, y = Abundance_Index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Field", y = "Abundance Index", title = "Larus michahellis abundance index per field")
