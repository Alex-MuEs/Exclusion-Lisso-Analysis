#YIELD#

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(data.table)
library(dlookr)

#Load data
Yield <- read.csv2("data/original/Yield.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))
