# ANALYZING DATA FOR COYOTE MANUSCRIPT
# author: Jamie Clarke
# adapted from code written by Larissa Bron and Marissa Dyck

# last updated: November 15 2024

# 1) set-up ---------------------------------------------------------------

# load in relevant packages
library(tidyverse)
library(PerformanceAnalytics)
library(lme4) 
library(MuMIn)
library(purrr)
library(broom.mixed)

# 2) data import ----------------------------------------------------------

# read in processed data (created using coyote_formatting script)
coyote_data <-
  
  read_csv('data/processed/coyote_data.csv')

# 3) linear feature model set ---------------------------------------------------------------------

# first: doing some exploratory analyses to decide which linear features to include in further models

# H0: null model
null <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 1 +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H1: global model (all uncorrelated linear features)
global_lf <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(roads) +
      scale(seismic_lines) +
      scale(seismic_lines_3D) +
      scale(trails) +
      scale(transmission_lines) + 
      (1 | array),
    data = coyote_data,
    family = binomial)

# H2: pipelines (on their own since hard to classify, variable widths)
pipeline_lf <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(pipeline) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H3: narrow linear features (~5 m wide)
narrow_lf <-
  
  glmer(
  cbind(coyote_pres, coyote_abs) ~ 
    scale(seismic_lines_3D) +
    scale(trails) +
    (1 | array),
  data = coyote_data,
  family = binomial)

# H4: wide linear features (>5 m wide)
wide_lf <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(roads) +
      scale(seismic_lines) +
      scale(transmission_lines) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H5: vegetated linear features (not paved/graveled)
veg_lf <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(seismic_lines) +
      scale(seismic_lines_3D) +
      scale(trails) +
      scale(transmission_lines) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H6: un-vegetated linear features (paved/graveled)
unveg_lf <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(roads) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# 4) linear feature model selection ---------------------------------------

lf_sel <-
  
  model.sel(null,
            global_lf,
            pipeline_lf,
            narrow_lf,
            wide_lf,
            veg_lf,
            unveg_lf)

# result: wide_lf model best supported by delta > 2.00
#         global_lf model second-best supported, unveg_lf close third


# 6) creating a wide linear feature variable ------------------------------

coyote_data <-
  
  coyote_data %>% 
  
  mutate(wide_linear = 
           roads + 
           seismic_lines +
           transmission_lines)


# 5) hypothesis-testing model set -----------------------------------------

# H1: global model (natural landcover, wide linear features, all mammals)
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

# H2: natural landcover
lc <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(nat_land) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H3: wide linear features and natural landcover
wide_lc <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(wide_linear) +
      scale(nat_land) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H4: prey species
prey <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(snowshoe_hare) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H5: prey species and natural landcover
prey_lc <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(snowshoe_hare) +
      scale(nat_land) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H6: competitor species 
comp <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(grey_wolf) +
      scale(lynx) +
      scale(fisher) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H7: competitor species and natural landcover
comp_lc <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(grey_wolf) +
      scale(lynx) +
      scale(fisher) +
      scale(nat_land) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H8: prey species and wide linear features
prey_wide <-
  
  glmer(
  cbind(coyote_pres, coyote_abs) ~ 
    scale(white_tailed_deer) +
    scale(moose) +
    scale(red_squirrel) +
    scale(snowshoe_hare) +
    scale(wide_linear) +
    (1 | array),
  data = coyote_data,
  family = binomial)

# H9: competitor species and wide linear features
comp_wide <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(grey_wolf) +
      scale(lynx) +
      scale(fisher) +
      scale(wide_linear) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H10: prey species, wide linear features and natural landcover
prey_wide_lc <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(snowshoe_hare) +
      scale(wide_linear) +
      scale(nat_land) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H11: competitor species, wide linear features and natural landcover
comp_wide_lc <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(grey_wolf) +
      scale(lynx) +
      scale(fisher) +
      scale(nat_land) +
      scale(wide_linear) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H12: global interaction model (natural landcover, all mammals, and interactions between wide linear features and top prey/competitor species)
global_int <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~
      scale(nat_land) +
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(lynx) +
      scale(fisher) +
      scale(wide_linear) * scale(snowshoe_hare) +
      scale(wide_linear) * scale(grey_wolf) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H13: prey interaction model (all mammals and interaction between wide linear features and top prey species)
prey_int <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(wide_linear) * scale(snowshoe_hare) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H13: competitor interaction model (all mammals and interaction between wide linear features and top competitor species)
comp_int <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(lynx) +
      scale(fisher) +
      scale(wide_linear) * scale(grey_wolf) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H14: global prey interaction model (all mammals, natural landcover and interaction between wide linear features and top prey species)
prey_lc_int <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(nat_land) +
      scale(wide_linear) * scale(snowshoe_hare) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H15: global competitor interaction model (all mammals, natural landcover and interaction between wide linear features and top competitor species)
comp_lc_int <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(lynx) +
      scale(fisher) +
      scale(nat_land) +
      scale(wide_linear) * scale(grey_wolf) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# 6) hypothesis-testing model selection -----------------------------------

h_sel <-
  
  model.sel(null,
            global,
            lc,
            wide_lc,
            prey,
            prey_lc,
            comp,
            comp_lc,
            prey_wide,
            comp_wide,
            prey_wide_lc,
            comp_wide_lc,
            global_int,
            prey_int,
            comp_int,
            prey_lc_int,
            comp_lc_int)

# result: global model best supported by delta > 2.00
#         global_int model second-best supported

# 7) odds ratios --------------------------------------------------

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
  mutate(label = as.factor(label))

# 8) save odds ratio tibble -----------------------------------------------

# save global_odds as .csv in 'processed' data folder
write_csv(global_odds,
          'data/processed/global_odds.csv')

# 9) predicted probabilities ----------------------------------------------





wide_predictions <- ggpredict(global, 
                              terms = "wide_linear [all]",  # Include both nat_land 
                              type = "fe")  # Use type "re" to include random effects





# plot 

ggplot(wide_predictions, aes(x = x,
                             y = predicted)) +
  
  geom_line(aes()) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.8)




# random effects ggpredict ------------------------------------------------



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





