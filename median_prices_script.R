#---------------------------------------------------------------------------#
# Nom : median_price_script.R                         					            #
# Description : Compare median salary and house prices in US States         #
# Auteur: Pietro Violo                                                      #
# Date : 21 mai 2022                                                        #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

#'* Libraries *
library(tidyverse)
library(viridis)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(broom)

#'* Data import *

median_house_prices <- read.csv("median_house_prices.csv")

median_income <- read.csv("median_income_census_bureau.csv")

# We're interested in the second to last row (median income) as well as 
# every 9th value

median_income <- median_income[,-1]

median_income <- data.frame(c(state.name, "Puerto Rico"),unlist(median_income[11,seq(1,408, by = 8)]))

colnames(median_income) <- c("State", "Median income")

rownames(median_income) <- NULL

joined_df <- left_join(median_income, median_house_prices) 

joined_df <- joined_df %>% mutate(percentage = `Median income` / MedianValue * 100) %>% 
  dplyr::mutate(pct = case_when(percentage >= 10 & percentage < 15 ~ "10 - 15 %",
                                percentage >= 15 & percentage < 20 ~ "15 - 19 %",
                                       percentage >= 20 & percentage < 25 ~ "20 - 24 %",
                                       percentage >= 25 & percentage < 30 ~ "25 - 29 %",
                                       percentage >= 30 & percentage < 35 ~ "30 - 34 %",
                                       percentage >=  35 & percentage < 40~ "35 - 39 %",
                                       percentage >=  40 & percentage < 45~ "40 - 44 %"))




#'* Plotting *

# Tutorial on RGallery at the following link : https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , joined_df, by=c("id"="State")) %>% 
  na.omit()

my_palette <- rev(inferno(9))[c(-1,-9)]


png("median_prices_graph.png", res = 300, width = 4200, height = 2400)

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = pct, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Median household income / median home price * 100 %", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  ggtitle( "Median yearly household income as a percentage of the median home price, by state, 2022" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 20, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

dev.off()

