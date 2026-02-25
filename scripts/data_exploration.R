rm(list=ls())

library(tidyverse)

lisso_abundance <- read_csv2("data/modified/Lisso_abundance.csv")

lisso_filt <- lisso_abundance %>% 
  filter(Treatment %in% c("BE", "FO"))

ggplot(lisso_filt, aes(Treatment, Abundance)) +
  geom_jitter(width = 0.1, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  theme_minimal() +
  facet_wrap(~Field)



######################## ROOT BIOMASS ##################
root_biomass <- read_csv2("data/original/Root_dmg_tot.csv")
biomass_filt <- root_biomass %>% 
  filter(Date == "2025-06-30",
         Treatment %in% c("BE", "FO"),
         Root_weight < 3)


ggplot(biomass_filt, aes(Treatment, Root_weight)) +
  geom_jitter(width = 0.1, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  theme_minimal() +
  facet_wrap(~Field)


root_summ <- biomass_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_root = mean(Root_weight))

root_wide <- root_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_root
  ) %>% 
  mutate(root_contribution = (((FO-BE)/(FO)))*100)

ggplot(root_wide, aes(x = "", y = root_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  #facet_wrap(~ funct_group,scale = "free_y") +
  labs(x = "",
       y = "Contribution of waterbirds to root biomass (%)") +
  theme_bw() +
  coord_flip()


########################LISSOROPTRHUS LARVAE##################
larvae <- read_csv2("data/original/Larvae.csv")
larvae_filt <- larvae %>% 
  filter(Treatment %in% c("BE", "FO"))

ggplot(larvae_filt, aes(Treatment, Abundance)) +
  geom_jitter(width = 0.1, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  theme_minimal() +
  facet_wrap(~Field) +
  scale_y_sqrt()

larvae_summ <- larvae_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_larvae = mean(Abundance, na.rm =T))

larvae_wide <- larvae_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_larvae
  ) %>% 
  mutate(larvae_control_contribution = (((BE-FO)/(BE)))*100)

ggplot(larvae_wide, aes(x = "", y = larvae_control_contribution)) +
  geom_jitter(width = 0.05, size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  #facet_wrap(~ funct_group,scale = "free_y") +
  labs(x = "",
       y = "Contribution of waterbirds to larvae control (%)") +
  theme_bw() +
  coord_flip()


#################YIELD############################
yield <- read_csv2("data/original/Yield.csv")
yield_filt <- yield %>% 
  filter(Treatment %in% c("BE", "FO"))


ggplot(yield_filt, aes(Treatment, `Yield_kg/ha_HR14`)) +
  geom_jitter(width = 0.1, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  theme_minimal() +
  facet_wrap(~Field)

yield_summ <- yield_filt %>% 
  group_by(Field, Treatment) %>% 
  summarise(mean_yield = mean(`Yield_kg/ha_HR14`))

yield_wide <- yield_summ %>%
  pivot_wider(
    id_cols = c(Field),
    names_from = Treatment,
    values_from = mean_yield
  ) %>% 
  mutate(yield_contribution = (((FO-BE)/(FO)))*100)

ggplot(yield_wide, aes(x = "", y = yield_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  #facet_wrap(~ funct_group,scale = "free_y") +
  labs(x = "",
       y = "Contribution of waterbirds to yield (%)") +
  theme_bw() +
  coord_flip()



t.test(yield_wide$FO, yield_wide$BE, paired = TRUE)

################MESOFAUNA EFFECTS ###############

Aquatic_predators <- read_csv2("data/modified/Aquatic_predators.csv")

#Filtering data
Aquatic_predators2 <- Aquatic_predators %>%
  filter(!is.na(Aquatic_predators$Taxa),
         #Stage != "RENAC",
         #Date < "2025-06-30",
         Treatment %in% c("BE", "FO"))
Aquatic_predators3 <- Aquatic_predators2 %>% 
  mutate(funct_group = case_when(
    Taxa %in% c("Pelophylax perezi", "Epidalea calamita") ~ "Amphibians",
    Taxa %in% c("Misgurnus anguillicaudatus", "Carassius sp", "Pseudorasbora sp",
                "Mugil sp", "Alburnus alburnus", "Gobio lozanoi" ) ~ "Fish",
    TRUE ~ "Procambarus clarkii")) %>% 
  group_by(Field, Treatment, funct_group) %>%
  filter(funct_group != "Fish") %>% 
  summarise(Abundance = sum(Abundance)) %>%
  ungroup()


predator_wide <- Aquatic_predators3 %>%
  pivot_wider(
    id_cols = c(Field, funct_group),
    names_from = Treatment,
    values_from = Abundance
  ) %>% 
  mutate(control_contribution = (((BE-FO)/(BE)))*100)

predator_wide


ggplot(predator_wide, aes(x = funct_group, y = control_contribution)) +
  geom_point(size = 3, alpha = 0.5, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 6, color = "darkred") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.01, color = "black") +
  #facet_wrap(~ funct_group,scale = "free_y") +
  labs(x = "",
       y = "Contribution of waterbirds to aquatic mesopredators (%)") +
  theme_bw() +
  coord_flip()




################ EMERGENCE EFFECTS ###############

emergence <- read_csv2("data/original/Emergence.csv")
emergence_filt <- emergence %>% 
  filter(Treatment %in% c("BE", "FO")) %>% 
  group_by(Field, Treatment, Taxa) %>% 
  summarise(Abundance = sum (Abundance))


ggplot(emergence_filt, aes(x = Treatment, y = Abundance)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, color = "red") +
  facet_wrap(~ Taxa + Field,scale = "free_y") +
  labs(title = "Boxplot of Abundance by treatment",
       x = "Treatment",
       y = "Abundance") +
  theme_bw()

