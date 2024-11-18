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
library(lme4)

# set ggplot theme
theme_set(theme_bw())

# 2) data import ----------------------------------------------------------

# read in processed data (created using coyote_formatting script)
coyote_data <-
  
  read_csv('data/processed/coyote_data.csv') %>% 
  
  # add wide_linear column combining all wide linear features (determined using coyote_analysis script)
  mutate(wide_linear = 
           roads + 
           seismic_lines +
           transmission_lines)

# run top model (determined using coyote_analysis script)
global <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~
      scale(nat_land) +
      scale(wide_linear) +
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(snowshoe_hare) +
      scale(grey_wolf) +
      scale(lynx) +
      scale(fisher) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# 3) calculating odds ratios --------------------------------------------------

# first: calculate odds ratios
global_odds <-
  
  # calculate confidence intervals
  confint(global,
          parm = 'beta_') %>% 
  
  # extract fixed effects coefficients
  cbind(est = fixef(global)) %>% 
  
  # exponentiate to get odds ratios
  exp() %>% 
  
  as.data.frame() %>% 
  
  # name first column and preserve it for next step
  rownames_to_column(var = 'term') %>% 
  
  # convert to tibble for easier manipulation
  as.tibble() %>%
  
  # remove intercept information
  filter(term != '(Intercept)') %>% 
  
  # rename confidence %s
  rename(lower = '2.5 %',
         upper = '97.5 %') %>% 
  
  # add a column with cleaned-up plot label names (so we don't have to do this in ggplot2)
  add_column(label = c('natural landcover',
                       'wide linear features',
                       'white-tailed deer',
                       'moose',
                       'red squirrel',
                       'snowshoe hare',
                       'grey wolf',
                       'lynx',
                       'fisher')) %>% 
  
  # change label column into a factor for plotting
  mutate(label = as.factor(label))odds ratio plot


# 4) odds ratios plot -----------------------------------------------------

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
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# 5) export odds ratio plot -----------------------------------------------

# save odds_plot to 'figures' folder
ggsave('odds_ratio_plot.tiff',
       odds_plot,
       path = 'figures')

# 6) determining predicted probabilities -----------------------------------------

predicted probabilities given covariates of interest





wide_predictions <- ggpredict(global, 
                              terms = c("wide_linear [all]")   
                              type = "fe")  # Use type "re" to include fixed effects





# plot 

ggplot(wide_predictions, aes(x = x,
                             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.8)




# TBD - random effects ggpredict ------------------------------------------------

test_wide_predictions <- ggpredict(global, 
                                   terms = c('wide_linear [all]',
                                             'array'),  # Include both nat_land and array
                                   type = "random")  # Use type "re" to include random effects





# plot 

ggplot(test_wide_predictions, aes(x = x,
                                  y = predicted)) +
  
  geom_line(aes(color = group)) +
  
  geom_ribbon(aes(fill = group,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.1)









# 7) mapping set-up ----------------------------------------------------------

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
  
  st_union()

# H) read in cities layer
# from ACME NetDrive
cities <-
  
  st_read('data/spatial/city_points.shp') %>% 
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  st_transform(crs = 26912) %>% 
  
  filter(name == 'Fort McMurray')

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
  tm_lines(col = 'grey40',
           lwd = 0.5) +
  
  tm_shape(cities) + 
  tm_dots(col = 'grey40',
          size = 0.3,
          shape = 15) +
  tm_text('name',
          size = 0.7,
          just = c(-0.11,-0.11)) +
  
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
  
  # add scale bar
  tm_scale_bar(position = c('left', 'BOTTOM'),
               text.size = 0.7,
               width = 0.2) +
  
  # add legend
  # tm_add_legend(type = 'symbol',
  #               col = c('grey95', '#DC3220', '#005AB5'),
  #               shape = c(15, 16, 16),
  #               size = c(0.5, 0.22, 0.22),
  #               labels = c('landscape units', '2021-2022 deployments', '2022-2023 deployments')) +
  
  # specify layout elements
  tm_layout(frame = FALSE,
            bg.color = 'transparent')
            # legend.bg.color = 'grey70',
            # legend.frame = 'black',
            # legend.frame.lwd = 0.7,
            # legend.text.size = 0.8,
            # legend.width = 0.6,
            # legend.height = -0.16,
            # legend.position = c('left', 'top'))

# then: set NE Alberta bbox as an sf object

### next: set NE Alberta bbox as an sf object
ne_ab_bbox <- 
  ne_ab_bbox %>% 
  st_as_sfc()

### then: overlay map of Alberta within Canada, with box around NE Alberta
ne_ab_can <-
  tm_shape(provs) +
  tm_fill(col = 'grey20') +
  
  tm_shape(ab_boundary) +
  tm_fill(col = 'grey70') +
  
  tm_shape(ne_ab_bbox) +
  tm_borders(col = 'black',
             lwd = 1) +
  
  tm_layout(frame.lwd = 2)

### next: overlay maps

# convert tmap objects into elements that cowplot can recognize
lus_cts_grob <- tmap_grob(lus_cts)
ne_ab_can_grob <- tmap_grob(ne_ab_can)

# now: inset using cowplot
inset_map <-
  
  ggdraw(lus_cts_grob) + # add first layer = study area map
  draw_plot(ne_ab_can_grob, # inset map of Canada and specify position
            x = 0.25,
            y = 0.71,
            width = 0.25,
            height = 0.25) +
  theme(panel.background =
          element_rect(
            fill = 'transparent'))

ggsave('study_area_map.png',
       inset_map,
       path = 'figures',
       width = 183,
       height = 100,
       units = 'mm',
       bg = 'transparent')





