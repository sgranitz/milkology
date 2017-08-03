# Made for thisismilkology.com

# Map using CDC data for Exclusive Breastfeeding at 6 months by state
# CDC data: https://www.cdc.gov/breastfeeding/pdf/2016breastfeedingreportcard.pdf

# ggmap:
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

library(tidyverse)
library(albersusa)
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(scales)

us <- usa_composite()
us_map <- fortify(us, region="name")

bf_data <- read_csv("C:/Users/Stephan/Desktop/cdc_bf_data.csv")
bf_data <- bf_data %>% 
  mutate(grp = ifelse(exc_bf_6mth > 15, ifelse(exc_bf_6mth > 25.5, ">25.5", "15-25.5"), "<15"))
bf_data$grp <- as.factor(bf_data$grp)

bf_data$name <- bf_data$state
us2 <- as_tibble(us) %>% left_join(bf_data, bf = "name")

gg <- ggplot() + 
  geom_map(data = us_map, map = us_map,
           aes(x = long, y = lat, map_id = id),
               color = "#2b2b2b", size = 0.1, fill = NA) + 
  theme_map()

gg + 
  geom_map(data = us@data, map = us_map,
           aes(fill = us2$grp, map_id = name),
           color = "white", size = 0.1) +
  coord_map() + labs(title = "Exclusive Breastfeeding Rates") +
  scale_fill_manual(name = "Per CDC 2016 Report: Exclusive Breastfeeding at 6 Months, %",
                    values = c("#EAB79E", "#D3DFE0", "#ECDBBC")) +
  theme(legend.position = "bottom", 
        legend.key.width = unit(3, "lines"))
