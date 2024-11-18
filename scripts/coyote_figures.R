# PLOTTING/MAPPING FOR COYOTE MANUSCRIPT
# author: Jamie Clarke

# last updated: November 15 2024

# 1) set-up ---------------------------------------------------------------

# load in relevant packages
library(tidyverse)
library(PerformanceAnalytics)
library(ggplot2)
library(cowplot)
library(sf)
library(dplyr)
library(tmap)
library(tmaptools)
library(ggpubr)
library(grid)
library(sp)
library(raster)

# 2) data import ----------------------------------------------------------

# read in processed data (created using coyote_analysis script)
global_odds <-
  
  read_csv('data/processed/global_odds.csv')

# 3) odds ratio plot ------------------------------------------------------

# plot odds ratios for global model
odds_plot <-
  
  ggplot(data = global_odds %>%
           
           # customize order of labels
           mutate(label = fct_relevel(label,
                                      'natural landcover',
                                      'wide linear features',
                                      'moose',
                                      'red squirrel',
                                      'snowshoe hare',
                                      'white-tailed deer',
                                      'fisher',
                                      'grey wolf',
                                      'lynx')),
         aes(x = label,
             y = est)) +
  
  # add points for estimates
  geom_point() +
  
  # add confidence intervals
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                linewidth = 0.5,
                width = 0.4) +
  
  geom_hline(yintercept = 1,
             alpha = 0.5) +
  
  # reverse the order of labels on the x axis (ggplot annoyingly plots them in reverse order)
  scale_x_discrete(limits = rev) +
  
  # rename y axis title
  ylab('odds ratio') +
  
  # flip x and y axis 
  coord_flip() +
  
  # specify theme
  theme_bw() +
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# 4) export odds ratio plot -----------------------------------------------

# save odds_plot to 'figures' folder
ggsave('odds_ratio_plot.tiff',
       odds_plot,
       path = 'figures')

# 5) plot predicted probabilities -----------------------------------------











# 6) mapping set-up ----------------------------------------------------------

# A) read in LU polygons where CTs were deployed

# first: specify which LUs
# for 2021-2022 - LUs 2 + 3
# for 2022-2023 - LUs 1, 12, 15 + 21
target <- c(2, 3, 1, 13, 15, 21)

# then: read in data + filter for those LUs
lus <-
  
  st_read('data/spatial/ALL_LUs.shp') %>% 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  st_transform(crs = 26912) %>% 
  
  filter(lu %in% target)

# B) read in CT deployment data for 2022
cts_2022 <-
  
  st_read('data/spatial/OSM2022_SITES_ALL.shp') %>% 
  
  set_names(
    names(.) %>% 
      tolower())

# C) read in CT deployment data for 2021 per LU and combine into single dataframe
cts_2021 <-
  
  st_read('data/spatial/OSM_2021_LU2.shp') %>%
  rbind(st_read('data/spatial/OSM_2021_LU3.shp')) %>%
  
  set_names(
    names(.) %>% 
      tolower())

# D) read in provincial boundaries
# source = Statistics Canada (https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm)
provs <- 
  
  st_read('data/spatial/lpr_000b16a_e.shp') %>% 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  st_transform(crs = 26912)

# E) subset Alberta
ab_boundary <-
  
  provs %>% 
  filter(prename == 'Alberta')

# F) crop Alberta spatial info to northeast

# figure out spatial extent of LUs
extent(lus)
# xmin: 368007.3 
# xmax: 561572 
# ymin: 6038172 
# ymax: 6354387

# make bounding box of NE Alberta based on extents
ne_ab_bbox <- 
  
  st_bbox(c(xmin = 365982.3,
            xmax = 563572,
            ymin = 6036147,
            ymax = 6356412),
          crs = st_crs(26912))

# G) read in Alberta roads layer
# source = National Road Network (https://open.canada.ca/data/en/dataset/cb4911f1-89d8-47f0-92e1-2cbfac3d300b/resource/e7fa72ef-92ea-4465-bd8f-9b6b765ae0ec)
roads <- 
  
  st_read('data/spatial/NRN_AB_14_0_ROADSEG.shp') %>% 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  st_transform(crs = 26912) %>% 
  
  crop_shape(bbox1,
             polygon = TRUE)

# H) read in cities layer
# from ACME NetDrive
cities <-
  
  st_read('data/spatial/city_points.shp') %>% 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  st_transform(crs = 26912) %>% 
  
  filter(name == 'Fort McMurray')

# 7) map of Alberta in Canada ---------------------------------------------

ab_can <-
  
  tm_shape(provs) +
  tm_fill(col = 'grey20') +
  
  tm_shape(ab_boundary) +
  tm_fill(col = 'grey70') +
  
  tm_layout(frame = FALSE,
            bg.color = 'transparent')


# 8) map of LUs and CT deployments in NE Alberta w study area inset -------

### first: extend bounding box so map features (e.g., scale bar) don't overlap with map itself
bbox1 <-  st_bbox(ne_ab_bbox)

xrange <- bbox1$xmax - bbox1$xmin

bbox1[1] <- bbox1[1] - (0.4 * xrange) # xmin - left

bbox1 <- bbox1 %>%
  st_as_sfc() # make bounding box an sf polygon

### then: set tmap mode to 'plot'
tmap_mode('plot')

### then: map LUs and CT deployments in NE Alberta
lus_cts <-
  
  # this establishes the spatial extent of the map
  tm_shape(ab_boundary,
           bbox = bbox1) +
  tm_fill() +
  
  # map neighbouring provinces in the background
  tm_shape(provs) +
  tm_fill(col = 'grey20') +
  
  # layer Alberta on top
  tm_shape(ab_boundary,
           bbox = ne_ab_bbox) +
  tm_fill(col = 'grey70') +
  
  # add a polygon for LU1
  tm_shape(lus %>% 
             filter(namerefere == 'LU1')) +
  tm_fill(col = 'grey95') +
  
  # add a polygon for LU2
  tm_shape(lus %>% 
             filter(namerefere == 'LU2')) +
  tm_fill(col = 'grey95') +
  
  # add a polygon for LU3
  tm_shape(lus %>% 
             filter(namerefere == 'LU3')) +
  tm_fill(col = 'grey95') +
  
  # add a polygon for LU13
  tm_shape(lus %>% 
             filter(namerefere == 'LU13')) +
  tm_fill(col = 'grey95') +
  
  # add a polygon for LU15
  tm_shape(lus %>% 
             filter(namerefere == 'LU15')) +
  tm_fill(col = 'grey95') +
  
  # add a polygon for LU21
  tm_shape(lus %>% 
             filter(namerefere == 'LU21')) +
  tm_fill(col = 'grey95') +
  
  # layer roads and cities on top
  tm_shape(roads) +
  tm_lines(col = 'grey30',
           lwd = 0.5) +
  
  tm_shape(cities) + 
  tm_dots(col = 'grey30',
          size = 0.15,
          shape = 15) +
  tm_text('name',
          size = 0.5,
          just = c(-0.1,-0.1)) +
  
  # add points for CT deployment locations in LU1
  tm_shape(cts_2022 %>% 
             crop_shape(lus %>% 
                          filter(namerefere == 'LU1'))) +
  tm_dots(col = '#005AB5',
          size = 0.02) +
  
  # add points for CT deployment locations in LU2
  tm_shape(cts_2021 %>% 
             crop_shape(lus %>% 
                          filter(namerefere == 'LU2'))) +
  tm_dots(col = '#DC3220',
          size = 0.02) +
  
  # add points for CT deployment locations in LU3
  tm_shape(cts_2021 %>% 
             crop_shape(lus %>% 
                          filter(namerefere == 'LU3'))) +
  tm_dots(col = '#DC3220',
          size = 0.02) +
  
  # add points for CT deployment locations in LU13
  tm_shape(cts_2022 %>% 
             crop_shape(lus %>% 
                          filter(namerefere == 'LU13'))) +
  tm_dots(col = '#005AB5',
          size = 0.02) +
  
  # add points for CT deployment locations in LU15
  tm_shape(cts_2022 %>% 
             crop_shape(lus %>% 
                          filter(namerefere == 'LU15'))) +
  tm_dots(col = '#005AB5',
          size = 0.02) +
  
  # add points for CT deployment locations in LU21
  tm_shape(cts_2022 %>% 
             crop_shape(lus %>% 
                          filter(namerefere == 'LU21'))) +
  tm_dots(col = '#005AB5',
          size = 0.02) +
  
  tm_scale_bar(position = c('left', 'BOTTOM'),
               text.size = 0.7,
               width = 0.2) +
  
  tm_add_legend(type = 'symbol',
                col = c('grey95', '#DC3220', '#005AB5'),
                shape = c(15, 16, 16),
                size = c(0.5, 0.22, 0.22),
                labels = c('landscape units', '2021-2022 deployments', '2022-2023 deployments')) +
  
  tm_layout(frame = FALSE,
            bg.color = 'transparent',
            legend.text.size = 0.8,
            legend.width = 0.7,
            legend.height = -0.5,
            legend.position = c('left', 'top'))

tmap_save(lus_cts,
          'figures/lus_cts.png',
          width = 100,
          height = 100,
          units = 'mm',
          bg = 'transparent')

# save figure
tmap_save(lus_cts,
          'figures/lus_cts.png',
          width = 100,
          height = 100,
          units = 'mm',
          bg = 'transparent')

# then: set NE Alberta bbox as an sf object
ne_ab_bbox <- 
  ne_ab_bbox %>% 
  st_as_sfc()

# then: overlay map of Canada with Alberta highlighted with box of NE Alberta
ne_ab_can <-
  tm_shape(provs) +
  tm_fill(col = 'grey20') +
  
  tm_shape(ab_boundary) +
  tm_fill(col = 'grey70') +
  
  tm_shape(ne_ab_bbox) +
  tm_borders(col = 'black',
             lwd = 2) +
  
  tm_layout(frame = FALSE)

tmap_save(ne_ab_can,
          'figures/ne_ab_can.png',
          width = 183,
          height = 100,
          units = 'mm')

# then: overlay maps (in Canva because I'm a cheater)






