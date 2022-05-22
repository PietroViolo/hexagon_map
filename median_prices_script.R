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
install.packages("rgeos")

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

joined_df <- joined_df %>% mutate(percentage = `Median income` / MedianValue * 100)



#'* Plotting *

# Tutorial on RGallery at the following link : https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))


