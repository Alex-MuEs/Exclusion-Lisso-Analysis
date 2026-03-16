#Cargar paquetes necesarios para hacer modelos lineales mixtos
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(DHARMa)


##YIELD##


#Cargar datos
Yield <- read.csv2("data/original/Yield.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))
yield_filt <- Yield %>%
  filter(Treatment %in% c("BE", "FO"))

#Modelo lineal mixto con Yield_kg.ha_HR14 como variable respuesta; Treatment como efecto fijo y campo y repetición como efectos aleatorios
model_yield <- lmer(Yield_kg.ha_HR14 ~ Treatment + (1|Field) + (1|Repeat), data = yield_filt)
summary(model_yield)
#Comparación de medias entre tratamientos utilizando emmeans
emmeans_results <- emmeans(model_yield, pairwise ~ Treatment)
print(emmeans_results)

#Comprobación de asunciones del modelo
#Residuos vs valores ajustados, homogeneidad de varianzas
plot(model_yield, which = 1)
#QQ plot de residuos
qqnorm(residuals(model_yield))
qqline(residuals(model_yield))
#Prueba de normalidad de residuos utilizando DHARMa
simulationOutput <- simulateResiduals(fittedModel = model_yield)
plot(simulationOutput)

#Gráfico de medias ajustadas por tratamiento
emmeans_df <- as.data.frame(emmeans_results$emmeans)
ggplot(emmeans_df, aes(x = Treatment, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Medias ajustadas por tratamiento", y = "Media ajustada (kg/ha)", x = "Tratamiento") +
  theme_minimal()




##ROOT DMG##


#Cargar datos
Root <- read.csv2("data/original/Root_dmg_20.csv") %>% 
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))
root_filt <- Root %>%
  filter(Treatment %in% c("BE", "FO"))

#Modelo lineal mixto con Root_weight como variable respuesta; Treatment como efecto fijo y campo y repetición como efectos aleatorios
model_root <- lmer(sqrt(Root_weight) ~ Treatment + (1|Field) + (1|Repeat), data = root_filt)
summary(model_root)
#Comparación de medias entre tratamientos utilizando emmeans
emmeans_results_root <- emmeans(model_root, pairwise ~ Treatment)
print(emmeans_results_root)

#Comprobación de asunciones del modelo
#Residuos vs valores ajustados, homogeneidad de varianzas
plot(model_root, which = 1)
#QQ plot de residuos
qqnorm(residuals(model_root))
qqline(residuals(model_root))
#Prueba de normalidad de residuos utilizando DHARMa
simulationOutput_root <- simulateResiduals(fittedModel = model_root)
plot(simulationOutput_root)

#Gráfico de medias ajustadas por tratamiento
emmeans_df_root <- as.data.frame(emmeans_results_root$emmeans)
ggplot(emmeans_df_root, aes(x = Treatment, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Medias ajustadas por tratamiento", y = "Media ajustada (g)", x = "Tratamiento") +
  theme_minimal()





#LEAF DMG##


#Cargar datos
Leaf <- read.csv2("data/original/Leaf_dmg.csv") %>%
  mutate(Date = as.POSIXct(Date),
         Field = as.character(Field), 
         Repeat = as.character(Repeat))
leaf_filt <- Leaf %>%
  filter(Treatment %in% c("BE", "FO"))

#Modelo lineal mixto con Leaves_dmg_10leaves como variable respuesta; Treatment como efecto fijo y campo y repetición como efectos aleatorios
model_leaf <- lmer(Leaves_dmg_10leaves ~ Treatment + (1|Field) + (1|Repeat), data = leaf_filt)
summary(model_leaf)
#Comparación de medias entre tratamientos utilizando emmeans
emmeans_results_leaf <- emmeans(model_leaf, pairwise ~ Treatment)
print(emmeans_results_leaf)

#Comprobación de asunciones del modelo
#Residuos vs valores ajustados, homogeneidad de varianzas
plot(model_leaf, which = 1)
#QQ plot de residuos
qqnorm(residuals(model_leaf))
qqline(residuals(model_leaf))
#Prueba de normalidad de residuos utilizando DHARMa
simulationOutput_leaf <- simulateResiduals(fittedModel = model_leaf)
plot(simulationOutput_leaf)

#Gráfico de medias ajustadas por tratamiento
emmeans_df_leaf <- as.data.frame(emmeans_results_leaf$emmeans)
ggplot(emmeans_df_leaf, aes(x = Treatment, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Medias ajustadas por tratamiento", y = "Media ajustada (número de hojas dañadas)", x = "Tratamiento") +
  theme_minimal()



#Modelo lineal mixto con Marks_5leaves como variable respuesta; Treatment como efecto fijo y campo y repetición como efectos aleatorios
model_marks <- lmer(sqrt(Marks_5leaves) ~ Treatment + (1|Field) + (1|Repeat), data = leaf_filt)
summary(model_marks)
#Comparación de medias entre tratamientos utilizando emmeans
emmeans_results_marks <- emmeans(model_marks, pairwise ~ Treatment)
print(emmeans_results_marks)

#Comprobación de asunciones del modelo
#Residuos vs valores ajustados, homogeneidad de varianzas
plot(model_marks, which = 1)
#QQ plot de residuos
qqnorm(residuals(model_marks))
qqline(residuals(model_marks))
#Prueba de normalidad de residuos utilizando DHARMa
simulationOutput_marks <- simulateResiduals(fittedModel = model_marks)
plot(simulationOutput_marks)

#Gráfico de medias ajustadas por tratamiento
emmeans_df_marks <- as.data.frame(emmeans_results_marks$emmeans)
ggplot(emmeans_df_marks, aes(x = Treatment, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(title = "Medias ajustadas por tratamiento", y = "Media ajustada (número de marcas)", x = "Tratamiento") +
  theme_minimal()
