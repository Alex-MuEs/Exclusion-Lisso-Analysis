#Clean session before starting
rm(list=ls())

#Load libraries for screening the data
library(ggplot2)
library(readxl)

#Load the data from data folder
Emergence <- read_excel("data/Emergence.xlsx", col_types = c("date", "text", "text", "text", "numeric", "skip"))
Leaf_dmg <- read_excel("data/Leaf_dmg.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "numeric", "skip"))
Larvae <- read_excel("data/Larvae.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "skip"))
Root_dmg20 <- read_excel("data/Root_dmg_20.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "skip"))
Root_dmg_larvae <- read_excel("data/Root_dmg_larvae.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "numeric", "skip"))
Macrofauna <- read_excel("data/Macrofauna.xlsx", col_types = c("date", "text", "text", "text", "text", "text", "numeric", "skip"))
Yield <- read_excel("data/Yield.xlsx", col_types = c("date", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip"))

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

