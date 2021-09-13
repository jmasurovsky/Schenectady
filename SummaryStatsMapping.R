getwd()
install.packages("tidyverse")
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(plotly)
library(methods)
library(plyr)
library(RCurl)
library(RJSONIO)
library(tidyverse)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)


install.packages("tmap")
install.packages("maptools")
install.packages(c("RColorBrewer", "Sp", "rgeos", "tmap", "tmaptools", "sf", "downloader", "rgdal", "geojsonio"))
library(tmap)
library(tmaptools)

setwd("C:/Users/jmasurovsky/Documents/TSF")

NYtracts <- readOGR("NY Census Tracts/cb_2018_36_tract_500k.shp", layer="cb_2018_36_tract_500k")

NYtractsSF <- st_as_sf(NYtracts)

Demographics <- read_csv("ACS Demographic & Pop/ACS2019_DemoPop.csv")

CountyDemographics <- merge(NYtractsSF, Demographics, by.x = "AFFGEOID", by.y = "id")

qtm(CountyDemographics, fill = "Percent!!SEX AND AGE!!Total population")

install.packages("shinyjs")
library(shinyjs)

tmap_mode("view")


## ------------------------------------ MAPPING ----------------------------------------- ##

# Population and Demographics

tm_shape(CountyDemographics) +
  tm_polygons("Percent!!SEX AND AGE!!Total population",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Total Population")

tm_shape(CountyDemographics) +
  tm_polygons("Percent!!RACE!!Total population!!One race!!White",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="White Population")

tm_shape(CountyDemographics) +
  tm_polygons("Percent!!RACE!!Total population!!One race!!Black or African American",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Black Population")

tm_shape(CountyDemographics) +
  tm_polygons("Percent!!RACE!!Total population!!One race!!Asian",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Asian Population")

tm_shape(CountyDemographics) +
  tm_polygons("Percent!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Hispanic or Latino Population")


# Education

Education <- read_csv("ACS Educational Attainment/Education.csv")

CountyEducation <- merge(NYtractsSF, Education, by.x = "AFFGEOID", by.y = "id")

tm_shape(CountyEducation) +
  tm_polygons("Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="High School Graduate (25+ Years Old)")


tm_shape(CountyEducation) +
  tm_polygons("Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Bachelor's degree or higher (25+ Years Old)")

tm_shape(CountyEducation) +
  tm_polygons("Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college, no degree",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Some college, no degree (25+ Years Old)")

tm_shape(CountyEducation) +
  tm_polygons("Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Graduate or professional degree",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Graduate or Professional Degree (25+ Years Old)")


# HOUSING

Housing <- read_csv("ACS Housing/Housing.csv")

CountyHousing <- merge(NYtractsSF, Housing, by.x = "AFFGEOID", by.y = "id")

names(Housing)

tm_shape(CountyHousing) +
  tm_polygons("Percent!!HOUSING TENURE!!Occupied housing units!!Owner-occupied",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Pct Owner-occupied")

tm_shape(CountyHousing) +
  tm_polygons("Percent!!HOUSING TENURE!!Occupied housing units!!Renter-occupied",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Pct Renter-occupied")


#INCOME

Income <- read_csv("ACS Income/Income.csv")

CountyIncome <- merge(NYtractsSF, Income, by.x = "AFFGEOID", by.y = "id")

tm_shape(CountyIncome) +
  tm_polygons("Estimate!!Households!!Median income (dollars)",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Household Median Income")


# EMPLOYMENT

Employment <- read_csv("ACS Employment Rate/Employment.csv")

CountyEmployment <- merge(NYtractsSF, Employment, by.x = "AFFGEOID", by.y = "id")

tm_shape(CountyEmployment) +
  tm_polygons("Estimate!!Unemployment rate!!Population 16 years and over",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Unemployment Rate")

tm_shape(CountyEmployment) +
  tm_polygons("Estimate!!Unemployment rate!!Population 20 to 64 years!!POVERTY STATUS IN THE PAST 12 MONTHS!!Below poverty level",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Unemployed and Below Poverty Level")

# Transportation

Transportation <- read_csv("ACS Access to Transportation/Transportation1.csv")

CountyTransportation <- merge(NYtractsSF, Transportation, by.x = "AFFGEOID", by.y = "id")

tm_shape(CountyTransportation) +
  tm_polygons("Estimate!!Total:!!No vehicle available",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Access to a vehicle")

Commuting <- read_csv("ACS Commute Time/Commuting1.csv")

CountyCommuting <- merge(NYtractsSF, Commuting, by.x = "AFFGEOID", by.y = "id")

tm_shape(CountyCommuting) +
  tm_polygons("Estimate!!Total!!Workers 16 years and over who did not work from home!!TRAVEL TIME TO WORK!!Mean travel time to work (minutes)",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Average Commute Time")


# Dot-density 

CountyDemographics <- rename(CountyDemographics, White = 'Estimate!!RACE!!Total population!!One race!!White', 
                             Black = 'Estimate!!RACE!!Total population!!One race!!Black or African American',
                             Asian = 'Estimate!!RACE!!Total population!!One race!!Asian', 
                             Hispanic_Latino = 'Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)')

CountyDemographics <- rename(CountyDemographics, Hispanic_Latino = 'Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)')

DotDensity <- data.frame(AFFGEOID = CountyDemographics$AFFGEOID, White = CountyDemographics$White, Black = CountyDemographics$Black, Asian = CountyDemographics$Asian, Hispanic = CountyDemographics$Hispanic_Latino, geometry = CountyDemographics$geometry)

pal <- c("#8dd3c7", "#ffffb3", "#ff5972", "#5bde6c", "#5bde6c")

NYtracts2 <- readOGR("NY Census Tracts/cb_2018_36_tract_500k.shp", layer="cb_2018_36_tract_500k") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

DotDensityMerged <- merge(NYtracts2@data, DotDensity, by.x = "AFFGEOID", by.y = "AFFGEOID")

NYtracts2@data

num.dots <- select(DotDensityMerged, White:Hispanic) / 5

sp.dfs <- lapply(names(num.dots), function(x) {
  dotsInPolys(CountyDemographicsSP, as.integer(num.dots[, x]), f="random")
})

par(mar = c(0,0,0,0))
plot(DotDensityMerged$geometry, lwd = 0.01, border = "white")
for (i in 1:length(sp.dfs)) {
  plot(sp.dfs[[i]], add = T, pch = 16, cex = 0.1, col = pal[i])
}

ggplot(NYtracts2, aes(long, lat, group = group)) +
  geom_path() +
  coord_map()

dfs <- lapply(sp.dfs, function(x) {
  data.frame(coordinates(x)[,1:2])
})
ethnicities <- c("White", "Black", "Asian", "Hispanic")
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]
}
dots.final <- bind_rows(dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities)
ggplot(NYtracts2) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = 0.01) +
  geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  scale_colour_manual(values = pal) +
  theme_map() +
  theme(plot.background = element_rect(fill = "black"), legend.position = "none") +
  coord_map()

