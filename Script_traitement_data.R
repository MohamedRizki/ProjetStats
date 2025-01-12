############### I. Installation et chargement des bibliothèques nécessaires ###############
# 1.Installe des packages
install.packages("sf")       
install.packages("ggplot2")  
install.packages("spatstat")
install.packages("raster")    
install.packages("dplyr")
install.packages("spdep")
install.packages("spgwr")
install.packages("spatialreg")

# 2.Charger les packages
library(sf)       
library(ggplot2)  
library(spatstat)
library(raster)    
library(dplyr)
library(spdep)
library(spgwr)
library(spatialreg)


############### II. Charger les données ###############
# Shapefile frontières mondiales
shapefile_path <- "C:/Users/DELL/Desktop/Projet_stat/Data/WORLD ADMIN BOUNDARIES/world-administrative-boundaries.shp"
world_boundaries <- st_read(shapefile_path)
# Reprojeter les données du shapefile
world_boundaries <- st_transform(world_boundaries, crs = 3857)
# Les données de Bitcoin Mining
bitcoin_data <- read.csv("C:/Users/DELL/Desktop/Projet_stat/Data/bitcoin_hashrate_europe.csv")
# Les données de production energitique
production_data <- read.csv("C:/Users/DELL/Desktop/Projet_stat/Data/production_energie_europe.csv")
# Charger les données d'empreinte carbone par pays
carbon_data <- read.csv("C:/Users/DELL/Desktop/Projet_stat/Data/carbon_footprint_europe.csv", stringsAsFactors = FALSE)
# Convertir les Données en Objet sf
carbon_sf <- st_as_sf(carbon_data, coords = c("europe_coordonnées_longitude", "europe_coordonnées_latitude"), crs = 4326)


############### III. Analyse de la répartition spatiale des sites Bitcoin Mining ###############
# 1. Visualisation initiale des sites de Bitcoin Mining en Europe
# Convertir les données Bitcoin Mining en objet sf et reprojeter en EPSG:3857
bitcoin_sf <- st_as_sf(bitcoin_data, coords = c("Longitude", "Latitude"), crs = 4326)
bitcoin_sf <- st_transform(bitcoin_sf, crs = 3857)
# Extraire les coordonnées reprojetées en mètres
coords_proj <- st_coordinates(bitcoin_sf)
# Supprimer les doublons dans les coordonnées
unique_coords <- unique(coords_proj)
# Créer un objet ppp avec poids
ppp_data_proj <- ppp(x = unique_coords[, 1], 
                     y = unique_coords[, 2],
                     window = owin(xrange = range(unique_coords[, 1]),
                                   yrange = range(unique_coords[, 2])))
# Carte initiale avec les points Bitcoin Mining et le fond de carte Europe
ggplot() +
  geom_sf(data = world_boundaries, color = "black", fill = NA) +
  geom_point(data = data.frame(coords_proj), aes(x = X, y = Y), 
             color = "red", size = 1) +
  labs(title = "Carte des Sites de Bitcoin Mining en Europe",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# 2. Calculer la densité KDE pour les points de Bitcoin Mining
# Définir la bande passante sigma pour contrôler le lissage de la densité
sigma_value <- 50000
# Calculer la densité spatiale avec la fonction
kde_proj <- density(ppp_data_proj, sigma = sigma_value)
# Convertir l'objet KDE en un format de ggplot2
kde_raster_proj <- as.im(kde_proj)
kde_raster_df_proj <- as.data.frame(kde_raster_proj, xy = TRUE)
# Normaliser les valeurs KDE entre 0 et 1 pour une meilleure lisibilité
kde_raster_df_proj$normalized_value <- kde_raster_df_proj$value / max(kde_raster_df_proj$value, na.rm = TRUE)
# Appliquer un seuil minimal pour éliminer les valeurs de densité très faibles
threshold <- 0.05 
kde_raster_df_proj_filtered <- kde_raster_df_proj %>%
  filter(normalized_value >= threshold)
# Afficher la carte KDE
ggplot() +
  geom_sf(data = world_boundaries, color = "black", fill = NA) +
  geom_tile(data = kde_raster_df_proj_filtered, 
            aes(x = x, y = y, fill = normalized_value), alpha = 0.8) +
  scale_fill_gradient(low = "green", high = "red", 
                      name = "Densité KDE (Normalisée)",
                      guide = guide_colorbar(barwidth = 15, barheight = 0.5)) + 
  labs(title = paste("Densité des Sites de Bitcoin Mining (KDE, sigma =", sigma_value, "m)"),
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")

#3. Analyse avec la fonction K de Ripley
# Calculer la fonction K de Ripley
K_result <- Kest(ppp_data_proj)
# Afficher les résultats de la fonction K
plot(K_result, main = "Fonction K de Ripley pour les Sites Bitcoin Mining",
     lwd = 2, col = "black")
lines(K_result$r, pi * (K_result$r)^2, col = "red", lty = 2, lwd = 2)  
legend("topleft", legend = c("K observée", "K théorique aléatoire"), 
       col = c("black", "red"), lty = c(1, 2), lwd = 2)


############### IV. Corrélation entre Bitcoin mining et production énergétique ###############
#1. Calcul de l’indice de Moran
# Transformation des données en objets sf
production_sf <- st_as_sf(production_data, coords = c("longitude", "latitude"), crs = 4326)
bitcoin_sf <- st_as_sf(bitcoin_data, coords = c("Longitude", "Latitude"), crs = 4326)
# Fusionner les deux datasets (proche des régions de production)
combined_data <- st_join(bitcoin_sf, production_sf, join = st_nearest_feature)
# Supprimer les doublons
combined_data <- combined_data[!duplicated(st_coordinates(combined_data)), ]
coords <- st_coordinates(combined_data)
#Créer une matrice de voisinage spatiale
neighbors <- knearneigh(coords, k = 2)
# Conversion en objet nb
nb_list <- knn2nb(neighbors)
# Création d'une matrice de poids spatiaux
listw <- nb2listw(nb_list, style = "W")
# Calculer l'indice de Moran pour le Avg Hashrate
moran_test <- moran.test(combined_data$Avg.Hashra, listw)
# Résultat de Moran
print(moran_test)
# Calculer Moran pour la production énergétique
moran_test_energy <- moran.test(combined_data$capacity_m, listw)
print(moran_test_energy)

# 2. Régression Géographique Pondérée (GWR)
# formule gwr
gwr_formula <- Avg.Hashra ~ capacity_m
# Recalculer la bande passante
gwr_bandwidth <- gwr.sel(gwr_formula, data = combined_data, coords = coords)
# Ajuster le modèle GWR
gwr_model <- gwr(gwr_formula, 
                 data = combined_data, 
                 coords = coords, 
                 bandwidth = gwr_bandwidth, 
                 hatmatrix = TRUE, 
                 se.fit = TRUE) 

# 3. Cartes thématiques des coefficients de corrélation locale.
# Ajouter les coefficients locaux au jeu de données spatial
combined_data$GWR_Coefficient <- gwr_model$SDF$capacity_m
# Carte
ggplot() +
  geom_sf(data = world_boundaries, color = "black", fill = NA) +
  geom_point(data = combined_data, aes(x = st_coordinates(geometry)[, 1], 
                                       y = st_coordinates(geometry)[, 2], 
                                       color = GWR_Coefficient)) +
  scale_color_gradient2(low = "blue", mid = "orange", high = "red",
                        midpoint = mean(combined_data$GWR_Coefficient), 
                        name = "Coeff. GWR") +
  labs(title = "Carte des Coefficients GWR (Points)",
       subtitle = "Relation entre Avg.Hashrate et capacity_m",
       x = "Longitude", y = "Latitude") +
  theme_minimal()


############### V. Empreinte carbone du Bitcoin mining + Consommation énergétique par secteur et par localisation ###############
#1. Cartographie de l’empreinte carbone
# Faire correspondre les noms des pays
bitcoin_carbon <- combined_data %>%
  left_join(carbon_data, by = c("CRName" = "Country"))
# Calculer l'empreinte carbone pour chaque site de mining
bitcoin_carbon <- bitcoin_carbon %>%
  mutate(Carbon_Footprint = Avg.Hashra * as.numeric(`Avg.EF....kgCO2eq.kWh.`))
# Créer un objet spatial avec les empreintes carbone
bitcoin_carbon_sf <- st_as_sf(bitcoin_carbon, coords = c("Longitude", "Latitude"), crs = 4326)
# Ordonner les données par ordre croissant de l'empreinte carbone
bitcoin_carbon_sf <- bitcoin_carbon_sf %>%
  arrange(Carbon_Footprint)
# Cartographier l'empreinte carbone
ggplot() +
  geom_sf(data = world_boundaries, color = "black", fill = "lightgray") +
  geom_point(data = bitcoin_carbon_sf, 
             aes(x = st_coordinates(geometry)[, 1], 
                 y = st_coordinates(geometry)[, 2], 
                 size = Carbon_Footprint, 
                 color = Carbon_Footprint)) +
  scale_color_gradientn(colors = c("yellow", "orange", "red", "darkred"), 
                        name = "Empreinte Carbone (kgCO2eq)") +
  scale_size_continuous(range = c(1, 5), name = "Empreinte Carbone") +
  labs(title = "L'Empreinte Carbone des Sites de Bitcoin",
       subtitle = "Basée sur le hashrate moyen et les facteurs d'émission",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# 2. Modèle à auto-régression spatiale (SAR)
# Assurez-vous que Carbon_Footprint et capacity_m ne contiennent pas de valeurs NA
bitcoin_carbon_sf <- bitcoin_carbon_sf %>%
  filter(!is.na(Carbon_Footprint), !is.na(capacity_m))
# Créer une matrice de voisinage spatiale pour les données filtrées
coords <- st_coordinates(bitcoin_carbon_sf)
neighbors <- knearneigh(coords, k = 2)
nb_list <- knn2nb(neighbors)
listw <- nb2listw(nb_list, style = "W")
# Ajuster le modèle SAR
sar_model <- lagsarlm(Carbon_Footprint ~ capacity_m, 
                      data = bitcoin_carbon_sf, 
                      listw = listw, 
                      method = "eigen")
# Afficher les résultats du modèle
summary(sar_model)
# Extraire les coefficients pour interprétation
sar_coefficients <- coef(sar_model)
print(sar_coefficients)
# Calculer les résidus pour analyse ultérieure
residuals_sar <- residuals(sar_model)
bitcoin_carbon_sf$residuals <- residuals_sar
# Cartographier les résidus pour identifier les régions problématiques
ggplot() +
  geom_sf(data = world_boundaries, color = "black", fill = "lightgray") +
  geom_point(data = bitcoin_carbon_sf, 
             aes(x = st_coordinates(geometry)[, 1], 
                 y = st_coordinates(geometry)[, 2], 
                 color = residuals), size = 3) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0, name = "Résidus SAR") +
  labs(title = "Carte des Résidus du Modèle SAR",
       subtitle = "Analyse des écarts locaux entre l'empreinte carbone et la production énergétique",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# 3. Identifier les régions à risque élevé pour des interventions ciblées
# Extraire les résidus du modèle SAR
bitcoin_carbon_sf$residuals <- residuals(sar_model, type = "response")
# Définir un seuil pour identifier les régions à risque élevé
# Critères : Hautes empreintes carbone ou résidus positifs significatifs
high_risk_threshold <- quantile(bitcoin_carbon_sf$Carbon_Footprint, probs = 0.9, na.rm = TRUE)
significant_residuals <- bitcoin_carbon_sf$residuals > quantile(bitcoin_carbon_sf$residuals, probs = 0.9, na.rm = TRUE)
# Identifier les points à risque élevé
bitcoin_carbon_sf$high_risk <- ifelse(
  bitcoin_carbon_sf$Carbon_Footprint > high_risk_threshold | significant_residuals,
  "High Risk", 
  "Low/Moderate Risk"
)
# Visualiser les régions à risque élevé
ggplot() +
  geom_sf(data = world_boundaries, color = "black", fill = "lightgray") +
  geom_point(data = bitcoin_carbon_sf, 
             aes(x = st_coordinates(geometry)[, 1], 
                 y = st_coordinates(geometry)[, 2], 
                 size = Carbon_Footprint, 
                 color = high_risk)) +
  scale_color_manual(values = c("High Risk" = "red", "Low/Moderate Risk" = "blue"), 
                     name = "Risque") +
  scale_size_continuous(range = c(1, 5), name = "Empreinte Carbone") +
  labs(title = "Identification des Régions à Risque Élevé",
       subtitle = "Basée sur l'empreinte carbone et les résidus du modèle SAR",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")