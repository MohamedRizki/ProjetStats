# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(tidyverse)
library(cluster)
library(ggpubr)
library(sf)

# Charger les données (adapter les chemins aux fichiers réels)
hashrate_data <- read.csv("bitcoin_hashrate_europe.csv")
production_data <- read.csv("production_energie_europe.csv")
consumption_data <- read.csv("consommation_energie_europe.csv")

# Statistiques descriptives pour Avg Hashra
summary(hashrate_data$Avg.Hashra)
# Transformation logarithmique
hashrate_data$Log_Avg.Hashra <- log10(hashrate_data$Avg.Hashra + 1)

# Histogramme après transformation
ggplot(hashrate_data, aes(x = Log_Avg.Hashra)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution  de Avg Hashra", x = "Avg Hashra", y = "Fréquence")


# Préparation des données pour le clustering
hashrate_geo <- hashrate_data %>% 
  select(Latitude, Longitude, Avg.Hashra) %>% 
  na.omit()

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(hashrate_geo[, c("Latitude", "Longitude")], centers = 3)

# Ajouter les clusters aux données
hashrate_geo$Cluster <- as.factor(kmeans_result$cluster)

# Visualiser les clusters sur une carte
ggplot(hashrate_geo, aes(x = Longitude, y = Latitude, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Clusters d'activités de minage Bitcoin", x = "Longitude", y = "Latitude")








#Production d'énergie

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(cluster)
library(ggpubr)
library(sf)


# Statistiques descriptives pour capacity_m
summary(production_data$capacity_m)


ggplot(production_data, aes(x = log1p(capacity_m))) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution de Capacity (MW)", x = "log(Capacity)", y = "Fréquence")


production_data$log_capacity <- log1p(production_data$capacity_m)

# Histogramme avec stratification par type d'énergie
ggplot(production_data, aes(x = log_capacity, fill = primary_fu)) +
  geom_histogram(binwidth = 0.2, position = "stack", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution logarithmique de Capacity (MW) par type d'énergie",
    x = "Log(Capacity + 1)",
    y = "Fréquence",
    fill = "Type d'énergie"
  ) +
  scale_fill_brewer(palette = "Set3")








#Consommation d'énergie

# Convertir les colonnes des années en numérique
consumption_data <- consumption_data %>%
  mutate(across(starts_with("X20"), as.numeric))

# Appliquer pivot_longer pour réorganiser les colonnes des années
data_long <- consumption_data %>%
  pivot_longer(
    cols = starts_with("X20"), # Colonnes correspondant aux années
    names_to = "année",        # Nouvelle colonne pour les années
    names_prefix = "X",        # Supprime le préfixe "X" des noms de colonnes
    values_to = "consommation" # Nouvelle colonne pour les valeurs
  )

summary(data_long$consommation)

# Histogramme de la consommation énergétique en 2018
#ggplot(filter(data_long, année == "2018"), aes(x = consommation)) +
# geom_histogram(binwidth = 500, fill = "orange", color = "black", alpha = 0.7) +
# theme_minimal() +
#labs(
# title = "Consommation énergétique en 2018",
#   x = "Consommation (GWh)",
#   y = "Fréquence"
#  )


ggplot(filter(data_long, année == "2018"), aes(x = log10(consommation))) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Consommation énergétique en 2018 ",
    x = "Consommation (GWh)",
    y = "Fréquence"
  )


