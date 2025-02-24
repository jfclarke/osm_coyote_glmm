# PLOTTING/MAPPING FOR COYOTE MANUSCRIPT
# author: Jamie Clarke

# last updated: January 14 2025

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
library(lme4)
library(ggeffects)
library(scales)
library(rphylopic)
library(MuMIn)
library(car)

# set ggplot theme
theme_set(theme_classic())

# 2) data import ----------------------------------------------------------

# read in processed data (created using coyote_formatting script)
coyote_data <-
  
  read_csv('data/processed/coyote_data.csv') %>% 
  
  # add wide_linear column combining all wide linear features (determined using coyote_analysis script)
  mutate(wide_linear = 
           roads + 
           seismic_lines +
           transmission_lines)

# run top models for step 1 and 2 (determined using coyote_analysis script)

# step 1:
wide_lf <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(roads) +
      scale(seismic_lines) +
      scale(transmission_lines) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# step 2:
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

# step 1:
wide_lf_odds <-
  
  # calculate confidence intervals
  confint(wide_lf,
          parm = 'beta_') %>% 
  
  # extract fixed effects coefficients
  cbind(est = fixef(wide_lf)) %>% 
  
  # exponentiate to get odds ratios
  exp() %>% 
  
  as.data.frame() %>% 
  
  # name first column and preserve it for next step
  rownames_to_column(var = 'term') %>% 
  
  # convert to tibble for easier manipulation
  as_tibble() %>%
  
  # remove intercept information
  filter(term != '(Intercept)') %>% 
  
  # rename confidence %s
  rename(lower = '2.5 %',
         upper = '97.5 %') %>% 
  
  # add a column with cleaned-up plot label names (so we don't have to do this in ggplot2)
  add_column(label = c('roads',
                       'seismic lines',
                       'transmission lines')) %>% 
  
  # change label column into a factor for plotting
  mutate(label = as.factor(label)) %>% 
  
  # add column for whether error bars overlap with 1 (for plotting)
  add_column(overlap = c('N',
                         'N',
                         'Y'))

# step 2:
global_odds <-
  
  confint(global,
          parm = 'beta_') %>% 
  
  cbind(est = fixef(global)) %>% 
  
  exp() %>% 
  
  as.data.frame() %>% 
  
  rownames_to_column(var = 'term') %>% 
  
  as_tibble() %>%
  
  filter(term != '(Intercept)') %>% 

  rename(lower = '2.5 %',
         upper = '97.5 %') %>% 
  
  add_column(label = c('natural landcover',
                       'wide linear features',
                       'white-tailed deer',
                       'moose',
                       'red squirrel',
                       'snowshoe hare',
                       'grey wolf',
                       'lynx',
                       'fisher')) %>% 
  
  mutate(label = as.factor(label)) %>% 
  
  add_column(overlap = c('N',
                         'N',
                         'Y',
                         'Y',
                         'Y',
                         'N',
                         'N',
                         'N',
                         'Y'))

# 4) odds ratios plot -----------------------------------------------------

# step 1
odds_plot_1 <-
  
  ggplot(data = wide_lf_odds,
         aes(x = label,
             y = est,
             color = overlap)) +
  
  # add line at 1
  geom_hline(yintercept = 1,
             alpha = 0.7) +
  
  # add points for estimates
  geom_point(size = 3) +
  
  # add confidence intervals
  geom_linerange(aes(ymin = lower,
                     ymax = upper),
                 linewidth = 0.5) +
  
  # set colour based on whether error bars overlap with 1
  scale_color_manual(values = c('black', 'grey70' ),
                     labels = c('N', 'Y')) +
  
  # reverse the order of labels on the x axis (ggplot annoyingly plots them in reverse order)
  scale_x_discrete(limits = rev) +
  
  # rename y axis title
  ylab('odds ratio') +
  
  # flip x and y axis 
  coord_flip() +
  
  theme_classic() +
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = 'none',
        axis.title.x = element_text(size = 14))

# step 2
odds_plot_2 <-
  
  ggplot(data = global_odds %>%
           
           # customize order of labels
           mutate(label = fct_relevel(label,
                                      'natural landcover',
                                      'wide linear features',
                                      'fisher',
                                      'lynx',
                                      'grey wolf',
                                      'red squirrel',
                                      'snowshoe hare',
                                      'white-tailed deer',
                                      'moose')),
         aes(x = label,
             y = est,
             color = overlap)) +

  # add background for landscape covariates
  annotate('rect',
           xmin = 7.5,
           xmax = 10,
           ymin = -Inf,
           ymax = Inf,
           fill = 'darkseagreen',
           alpha = 0.4) +
  
  # add background for competitor covariates
  annotate('rect',
           xmin = 4.5,
           xmax = 7.5,
           ymin = -Inf,
           ymax = Inf,
           fill = 'lightsteelblue1',
           alpha = 0.4) +
  
  # add background for prey covariates
  annotate('rect',
           xmin = 0,
           xmax = 4.5,
           ymin = -Inf,
           ymax = Inf,
           fill = 'tomato',
           alpha = 0.2) +
  
  # add line at 1
  geom_hline(yintercept = 1,
             alpha = 0.7) +
  
  # add points for estimates
  geom_point(size = 3) +
  
  # add confidence intervals
  geom_linerange(aes(ymin = lower,
                     ymax = upper),
                 linewidth = 0.5) +
  
  # set colour based on whether error bars overlap with 1
  scale_color_manual(values = c('black', 'grey70' ),
                     labels = c('N', 'Y')) +
  
  # reverse the order of labels on the x axis (ggplot annoyingly plots them in reverse order)
  scale_x_discrete(limits = rev) +
  
  # rename y axis title
  ylab('odds ratio') +
  
  # flip x and y axis 
  coord_flip() +
  
  theme_classic() +
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = 'none',
        axis.title.x = element_text(size = 14))

# 5) export odds ratio plots -----------------------------------------------

# save odds_plot_1 to 'figures' folder
ggsave('odds_ratio_lf.tiff',
       odds_plot_1,
       width = 150,
       height = 100,
       units = 'mm',
       path = 'figures')

# save odds_plot_2 to 'figures' folder
ggsave('odds_ratio_h.tiff',
       odds_plot_2,
       width = 150,
       height = 100,
       units = 'mm',
       path = 'figures')

# 6) determining predicted probabilities -----------------------------------------

# predicted probabilities given each covariates of interest

pp_nat_land <-
  
  ggpredict(global,
            terms = 'nat_land [all]',
            type = 'fe') # fe = fixed effects

pp_wide_linear <-
  
  ggpredict(global,
            terms = 'wide_linear [all]',
            type = 'fe')

pp_moose <-
  
  ggpredict(global,
            terms = 'moose [all]',
            type = 'fe')

pp_red_squirrel <-
  
  ggpredict(global,
            terms = 'red_squirrel [all]',
            type = 'fe')

pp_snowshoe_hare <-
  
  ggpredict(global,
            terms = 'snowshoe_hare [all]',
            type = 'fe')

pp_white_tailed_deer <-
  
  ggpredict(global,
            terms = 'white_tailed_deer [all]',
            type = 'fe')

pp_fisher <-
  
  ggpredict(global,
            terms = 'fisher [all]',
            type = 'fe')

pp_grey_wolf <-
  
  ggpredict(global,
            terms = 'grey_wolf [all]',
            type = 'fe')

pp_lynx <-
  
  ggpredict(global,
            terms = 'lynx [all]',
            type = 'fe')

# 7) predicted probabilities plots -------------------------------------

# NOTE: to find the PhyloPic uuid (= image ID), type the following code in the console:
# pick_phylopic(name = 'species name', n = #)
# where 'species name' is the scientific or common name of the species of interest and n = # is the number of options you want to peruse
# when you find the image you want: select it and find the uuid in the console output

# plot predicted probability of coyote occurence given proportion of natural landcover
plot_nat_land <- 
  
  ggplot(pp_nat_land,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'darkseagreen',
              alpha = 0.4) +
  
  scale_x_continuous(limits = c(0.7, 1),
                     breaks = seq(0.7, 1, by = 0.1),
                     expand = c(0, 0)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 0.65, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('prop natural land') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35))

# plot predicted probability of coyote occurence given proportion of wide linear features
plot_wide_linear <- 
  
  ggplot(pp_wide_linear,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'darkseagreen',
              alpha = 0.4) +
  
  scale_x_continuous(expand = c(0, 0),
                     labels = label_wrap(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 0.65, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('prop wide LFs') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35))

# plot predicted probability of coyote occurrence given total independent moose detections
plot_moose <-
  
  ggplot(pp_moose,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'tomato',
              alpha = 0.2) +
  
  scale_x_continuous(expand = c(0, 0),
                     labels = label_wrap(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('moose detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add moose silhouette
  add_phylopic(uuid = '74eab34a-498c-4614-aece-f02361874f79',
               x = 10,
               y = 0.55,
               height = 0.2)

# plot predicted probability of coyote occurrence given total independent red squirrel detections
plot_red_squirrel <-
  
  ggplot(pp_red_squirrel,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'tomato',
              alpha = 0.2) +
  
  scale_x_continuous(limits = c(0, 150),
                     breaks = seq(0, 150, by = 50),
                     expand = c(0, 0),
                     labels = label_wrap(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('squirrel detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add squirrel silhouette
  add_phylopic(uuid = 'dad08fea-5263-4f57-a37b-c27cbe0eb9a5',
               x = 30,
               y = 0.54,
               height = 0.16)

# plot predicted probability of coyote occurrence given total independent snowshoe hare detections
plot_snowshoe_hare <-
  
  ggplot(pp_snowshoe_hare,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'tomato',
              alpha = 0.2) +
  
  scale_x_continuous(expand = c(0,0),
                     labels = label_wrap(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('hare detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add hare silhouette
  add_phylopic(uuid = '44f9db61-88e2-4a44-82f5-57dea9b3798d',
               x = 50,
               y = 0.55,
               height = 0.2)

# plot predicted probability of coyote occurrence given total independent white-tailed deer detections
plot_white_tailed_deer <-
  
  ggplot(pp_white_tailed_deer,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'tomato',
              alpha = 0.2) +
  
  scale_x_continuous(limits = c(0, 130),
                     breaks = seq(0, 130, by = 30),
                     expand = c(0, 0),
                     labels = label_wrap(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('deer detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add deer silhouette
  add_phylopic(uuid = '4584be20-4514-4673-a3e8-97e2a6a10e57',
               x = 29,
               y = 0.53,
               height = 0.24)

# plot predicted probability of coyote occurrence given total independent fisher detections
plot_fisher <-
  
  ggplot(pp_fisher,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'lightsteelblue1',
              alpha = 0.4) +
  
  scale_x_continuous(limits = c(0, 12.5),
                     breaks = seq(0, 12.5, by = 3),
                     expand = c(0, 0),
                     labels = wrap_format(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('fisher detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add fisher silhouette
  add_phylopic(uuid = '735066c6-2f3e-4f97-acb1-06f55ae075c9',
               x = 3.5,
               y = 0.55,
               height = 0.12)

# plot predicted probability of coyote occurrence given total independent grey wolf detections
plot_grey_wolf <-
  
  ggplot(pp_grey_wolf,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'lightsteelblue1',
              alpha = 0.4) +
  
  scale_x_continuous(limits = c(0, 13),
                     breaks = seq(0, 13, by = 3),
                     expand = c(0, 0),
                     labels = wrap_format(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('wolf detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add wolf silhouette
  add_phylopic(uuid = '8cad2b22-30d3-4cbd-86a3-a6d2d004b201',
               x = 3.5,
               y = 0.55,
               height = 0.17)

# plot predicted probability of coyote occurrence given total independent lynx detections
plot_lynx <-
  
  ggplot(pp_lynx,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              fill = 'lightsteelblue1',
              alpha = 0.4) +
  
  scale_x_continuous(expand = c(0, 0),
                     labels = wrap_format(10)) +
  
  scale_y_continuous(limits = c(0, 0.65),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  
  xlab('lynx detections') +
  
  ylab(' ') +
  
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 14),
        axis.title.y.left = element_text(size = 35)) +
  
  # add lynx silhouette
  add_phylopic(uuid = '27a2173a-5903-46fc-83c5-29ed7f421046',
               x = 5.9,
               y = 0.55,
               height = 0.17)

# 8) predicted probabilities plot panel -----------------------------------

# combine predicted probabilities plots together in one figure
pp_plot <-
  
  ggarrange(plot_nat_land,
            plot_wide_linear,
            plot_fisher,
            plot_lynx,
            plot_grey_wolf,
            plot_red_squirrel,
            plot_snowshoe_hare,
            plot_white_tailed_deer,
            plot_moose,
            labels = 'auto',
            label.x = 0.88,
            font.label = list(size = 14),
            ncol = 5,
            nrow = 2) %>% 
  
  annotate_figure(left = text_grob('predicted coyote occurrence',
                                   rot = 90,
                                   vjust = 0.5,
                                   size = 16))

# 9) export predicted probabilities figure ---------------------------------

ggsave('predicted_probabilities_panel.png',
       pp_plot,
       path = 'figures',
       width = 250,
       height = 100,
       units = 'mm',
       bg = 'white')

# 10) predicted probabilities w/ random effects ------------------------------------------------

# determining and plotting an example graph of predicted probabilities with random effects

# choosing wide linear features as the fixed effect
re_pp_wide_linear <-
  
  ggpredict(global, 
            terms = c('wide_linear [all]', # fixed effect
                      'array'), # random effect
            type = "random")

# plot predicted probability of coyote occurrence given total independent lynx detections and random effects of array
re_plot <-
  
  ggplot(re_pp_wide_linear,
         aes(x = x,
             y = predicted)) +
  
  geom_line(aes(color = group)) +
  
  # using colourblind-friendly Tol colour scheme
  # more info at https://personal.sron.nl/~pault/
  scale_fill_manual(values = c('#4477AA',
                               '#66CCEE',
                               '#228833',
                               '#CCBB44',
                               '#EE6677',
                               '#AA3377')) +
  
  geom_ribbon(aes(fill = group,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.1) +
  
  scale_color_manual(values = c('#4477AA',
                                '#66CCEE',
                                '#228833',
                                '#CCBB44',
                                '#EE6677',
                                '#AA3377')) +
  
  labs(color = NULL,
       fill = NULL,
       x = 'proportion of wide LFs',
       y = 'predicted coyote occurrence')

# save plot
ggsave('re_predicted_probability.png',
       re_plot,
       path = 'figures',
       width = 183,
       height = 100,
       units = 'mm')

# 11) variance inflation factor ------------------------------------------

# calculate variance inflation factors (VIFs)
vif_model <-
  
  vif(global) %>% 
  
  # convert into a format that can be plotted
  as_tibble() %>% 
  
  # add a column labelling covariate name
  add_column(label = c('natural landcover',
                       'wide linear features',
                       'fisher',
                       'lynx',
                       'grey wolf',
                       'red squirrel',
                       'snowshoe hare',
                       'white-tailed deer',
                       'moose')) %>% 
  
  # add a column specifying covariate type
  add_column(cov_type = c('land',
                          'land',
                          'comp',
                          'comp',
                          'comp',
                          'prey',
                          'prey',
                          'prey',
                          'prey'))

# plot VIFs
vif_plot <-
  
  ggplot(data = vif_model %>% 
           
           # customize order of labels
           mutate(label = fct_relevel(label,
                                      'natural landcover',
                                      'wide linear features',
                                      'fisher',
                                      'lynx',
                                      'grey wolf',
                                      'red squirrel',
                                      'snowshoe hare',
                                      'white-tailed deer',
                                      'moose')),
         aes(x = label,
             y = value,
             fill = cov_type)) +
  
  geom_col() +
  
  # set colour based on covariate type
  scale_fill_manual(values = c('lightsteelblue1', 'darkseagreen', 'tomato'),
                     labels = c('land', 'comp', 'prey')) +
  
  # add line at 1
  geom_hline(yintercept = 1,
             alpha = 0.5,
             linetype = 'dashed') +
  
  # reverse the order of labels on the x axis (ggplot annoyingly plots them in reverse order)
  scale_x_discrete(limits = rev) +
  
  scale_y_continuous(limits = c(0, 1.35),
                     breaks = seq(0, 1.3, by = 0.5),
                     expand = c(0, 0)) +
  
  # rename x axis title
  ylab('VIF values') +
  
  # flip x and y axis 
  coord_flip() +
  
  theme_classic() +
  
  # specify theme elements
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = 'none',
        axis.title.x = element_text(size = 14))

# save plot
ggsave('vif_plot.png',
       vif_plot,
       path = 'figures',
       width = 100,
       height = 100,
       units = 'mm')

# 12) mapping set-up ----------------------------------------------------------

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
raster::extent(lus)
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

# 13) map of study area -------

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
  tm_dots(col = 'black',
          size = 0.3) +
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
  
  # specify layout elements
  tm_layout(frame = FALSE,
            bg.color = 'transparent')

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
             lwd = 0.75) +
  
  tm_layout(frame.lwd = 2)

### next: overlay maps

# convert tmap objects into elements that cowplot can recognize
lus_cts_grob <- tmap_grob(lus_cts)
ne_ab_can_grob <- tmap_grob(ne_ab_can)

# now: inset using cowplot
inset_map <-
  
  ggdraw(lus_cts_grob) + # add first layer = study area map
  draw_plot(ne_ab_can_grob, # inset map of Canada and specify position
            x = 0.13,
            y = 0.69,
            width = 0.27,
            height = 0.27) +
  theme(panel.background =
          element_rect(
            fill = 'transparent'),
        panel.border = 
          element_rect(
            colour = NA,
            fill = NA))

# 14) export study area map with inset ---------------------------------------------------------------------

ggsave('study_area_map.png',
       inset_map,
       path = 'figures',
       width = 100,
       height = 105,
       units = 'mm',
       bg = 'transparent')
