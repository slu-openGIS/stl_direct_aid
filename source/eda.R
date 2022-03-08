# =========================================================================== #
# Exploratory Data Analysis of ARPA Disbursements ####
# =========================================================================== #

## Dependencies ####
### Packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(sf)

### Functions
source("source/map_breaks.R")
source("source/save_plots.R")
source("source/sequoia_theme.R")

# =========================================================================== #

## Load Data ####
arpa <- read_csv("data/STL_ZCTA_ARPA_Disbursements.csv")
pop <- read_csv("data/STL_ZCTA_St_Louis_City_Total_Pop.csv")
race <- read_csv("data/STL_ZCTA_St_Louis_City_Race.csv")
poverty <- read_csv("data/STL_ZCTA_St_Louis_City_Poverty.csv")

zcta <- st_read("data/STL_ZCTA_St_Louis_City.geojson")

# =========================================================================== #

## Combine Data ####
### calculate rate
arpa <- left_join(arpa, pop, by = "GEOID_ZCTA") %>%
  mutate(arpa_rate = arpa_dis/total_pop*1000)

### combine rates with demographics
arpa_tbl <- left_join(arpa, poverty, by = "GEOID_ZCTA") %>%
  left_join(., race, by = "GEOID_ZCTA")

### combine rates with geometry
arpa <- mutate(arpa, GEOID_ZCTA = as.character(GEOID_ZCTA))
arpa_geo <- left_join(zcta, arpa, by = "GEOID_ZCTA")

### clean-up
rm(arpa, pop, race, poverty, zcta)

# =========================================================================== #

## Map Data ####
### re-project
arpa_geo <- st_transform(arpa_geo, crs = 26915)

### calculate breaks
arpa_geo <- map_breaks(arpa_geo, var = "arpa_rate", newvar = "map_breaks", 
                       style = "fisher", classes = 5, dig_lab = 2)

### create map
p1 <- ggplot() +
  geom_sf(data = arpa_geo, mapping = aes(fill = map_breaks), size = .2) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 1,000") +
  labs(
    title = "ARPA Disbursement Rates",
    subtitle = "City of St. Louis ZCTAs",
    caption = "Data via the City of St. Louis and the U.S. Census Bureau\nMap by Christopher Prener, PhD"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

### save map
save_plots(filename = "results/rate_map.png", plot = p1, preset = "lg")

# =========================================================================== #
  
## Plot by Poverty ####
### calculate correlation
corr <- cor.test(arpa_tbl$arpa_rate, arpa_tbl$pvty_pct)

### create scatterplot
p2 <- ggplot(data = arpa_tbl, mapping = aes(arpa_rate, pvty_pct)) +
  geom_smooth(method = "lm", size = 2, color = brewer.pal(n = 3, name = "Dark2")[2], se = TRUE) +
  geom_point(size = 4, color = brewer.pal(n = 3, name = "Dark2")[1]) +
  geom_smooth(method = "lm", size = 2, color = brewer.pal(n = 3, name = "Dark2")[2], se = FALSE) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50), limits = c(0,50)) +
  labs(
    title = "ARPA Disbursement and Poverty Rates",
    subtitle = "City of St. Louis ZCTAs",
    caption = paste0("Correlation for these values is r=", round(corr$estimate, digits = 3), 
                     " (p < .001)\nData via the City of St. Louis and the U.S. Census Bureau\nMap by Christopher Prener, PhD"),
    x = "ARPA Disbursement Rate per 1,000",
    y = "Povery Rate (%)"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

### save map
save_plots(filename = "results/poverty_plot.png", plot = p2, preset = "lg")

# =========================================================================== #

## Plot by Race ####
### calculate correlation
corr <- cor.test(arpa_tbl$arpa_rate, arpa_tbl$blk_pct)

### create scatterplot
p2 <- ggplot(data = arpa_tbl, mapping = aes(arpa_rate, blk_pct)) +
  geom_smooth(method = "lm", size = 2, color = brewer.pal(n = 4, name = "Dark2")[4], se = TRUE) +
  geom_point(size = 4, color = brewer.pal(n = 4, name = "Dark2")[3]) +
  geom_smooth(method = "lm", size = 2, color = brewer.pal(n = 4, name = "Dark2")[4], se = FALSE) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), limits = c(0,100)) +
  labs(
    title = "ARPA Disbursement and African Americans",
    subtitle = "City of St. Louis ZCTAs",
    caption = paste0("Correlation for these values is r=", round(corr$estimate, digits = 3), 
                     " (p < .001)\nData via the City of St. Louis and the U.S. Census Bureau\nMap by Christopher Prener, PhD"),
    x = "ARPA Disbursement Rate per 1,000",
    y = "African American Population (%)"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = FALSE)

### save map
save_plots(filename = "results/race_plot.png", plot = p2, preset = "lg")
