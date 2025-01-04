# ANALYZING DATA FOR COYOTE MANUSCRIPT
# author: Jamie Clarke
# adapted from code written by Larissa Bron and Marissa Dyck

# last updated: November 18 2024

# 1) set-up ---------------------------------------------------------------

# load in relevant packages
library(tidyverse)
library(PerformanceAnalytics)
library(lme4) 
library(MuMIn)
library(purrr)
library(broom.mixed)
library(car)

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

# H2: pipelines (on their own since hard to classify, variable widths + correlated with other features)
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


# 5) creating a wide linear feature variable ------------------------------

coyote_data <-
  
  coyote_data %>% 
  
  mutate(wide_linear = 
           roads + 
           seismic_lines +
           transmission_lines)

# 6) hypothesis-testing model set -----------------------------------------

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

# H14: competitor interaction model (all mammals and interaction between wide linear features and top competitor species)
comp_int <-
  
  glmer(
    cbind(coyote_pres, coyote_abs) ~ 
      scale(lynx) +
      scale(fisher) +
      scale(wide_linear) * scale(grey_wolf) +
      (1 | array),
    data = coyote_data,
    family = binomial)

# H15: global prey interaction model (all mammals, natural landcover and interaction between wide linear features and top prey species)
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

# H16: global competitor interaction model (all mammals, natural landcover and interaction between wide linear features and top competitor species)
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

# 7) comparing fixed vs random effect models ---------------------------------------------------------------------

# testing an example model (global) with a random effect for array against the same model without a random effect

# H17: global model (natural landcover, wide linear features, all mammals) without random effect
fe_global <-
  
  glm(
    cbind(coyote_pres, coyote_abs) ~
      scale(nat_land) +
      scale(wide_linear) +
      scale(white_tailed_deer) +
      scale(moose) +
      scale(red_squirrel) +
      scale(snowshoe_hare) +
      scale(grey_wolf) +
      scale(lynx) +
      scale(fisher),
    data = coyote_data,
    family = binomial)

# run model selection
model.sel(global,
          fe_global)

# result: random effects model best supported

# 8) hypothesis-testing model selection -----------------------------------

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

# 9) detection data ----------------------------------------------

# sum the number of independent detections of each focal species
coyote_data %>% 
  
  select_if(is.numeric) %>% # only consider numeric data
  
  map_dbl(sum) # sum down each column

# count the number of camera stations where coyotes were detected
sum(coyote_data$coyote_pres != 0,
    na.rm = TRUE)
