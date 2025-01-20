##nodes data 
library(dplyr)
library(sf)
library(ggplot2)

library("rnaturalearth") 
library("rnaturalearthdata")

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf") 

csv_file <- "data/nodes_data_1737124530.csv"
nodes <- read.csv(csv_file)

nodes_per_country <- nodes %>% count(country)  %>%  arrange(desc(n)) 
nodes_per_country $ cumsum <- cumsum(nodes_per_country $ n / sum(nodes_per_country $ n))

 ##Pareto's law and distribution of nodes  by country
scale_right <- max(nodes_per_country $ n)
ggplot(data = nodes_per_country[1:30,] , aes(reorder(country, -n), y = n))  + 
  geom_bar(stat = "identity", width=0.2) +
  geom_path(aes(y = cumsum *scale_right), group = 1, color ="red", size = 0.8) +
  scale_y_continuous(sec.axis = sec_axis(~./scale_right*100, name="Cumulative (%)"))+
  labs(title = "Nodes distribution per country", x = "Countries", y ="Number of rechable nodes")
 
##Map distribution
ggplot(data = world) +
  geom_sf(color = "black", fill = "grey") +
  geom_point(data = nodes, aes(x = long, y = lat), color = "red") +
  theme_minimal() +
  labs(title = "Bitcoin nodes distribution", x = "Longitude", y = "Latitude")
