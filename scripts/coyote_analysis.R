# ANALYZING DATA FOR COYOTE MANUSCRIPT
# author: Jamie Clarke
# adapted from code written by Larissa Bron and Marissa Dyck

# 1) set-up ---------------------------------------------------------------

# load in relevant packages
library(tidyverse)
library(PerformanceAnalytics)
library(lme4) 
library(MuMIn)
library(purrr)
library(broom.mixed)
library(car)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(broom.mixed)
library(insight)

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

# 9) detection data summaries ----------------------------------------------

# sum the number of independent detections of each focal species
coyote_data %>% 
  
  select_if(is.numeric) %>% # only consider numeric data
  
  map_dbl(sum) # sum down each column

# count the number of camera stations where coyotes were detected
sum(coyote_data$coyote_pres != 0,
    na.rm = TRUE)

# mean number of presences/absences
coyote_data %>% 
  
  select(coyote_pres, coyote_abs) %>% 
  
  summary()

# 10) evaluation by simulation --------------------------------------------

# this section is adapted from Ariel Muldoon's 'Simulate! Simulate!' series (https://aosmith.rbind.io/) and work by Dr Andrew Barnas, with heaps of coding help from Andrew (thank you!).

# define overall set-up
n_cts <- 40 # approximate number of cameras per array
n_arrays <- 6 # number of arrays
n_obs <- 1 # number of years cameras were deployed
n <- n_cts * n_arrays * n_obs # total observations

# define model parameters
b0 <- -1.4 # intercept value
b1 <- -0.40 # slope coefficient for nat_land
b2 <- 0.50 # slope coefficient for wide_linear
b3 <- 0.06 # slope coefficient for white_tailed_deer
b4 <- -0.06 # slope coefficient for moose
b5 <- 0.08 # slope coefficient for red_squirrel
b6 <- 0.19 # slope coefficient for snowshoe_hare
b7 <- 0.19 # slope coefficient for grey_wolf
b8 <- 0.17 # slope coefficient for lynx
b9 <- 0.02 # slope coefficient for fisher
array_sd <- 0.28

# assign camera and array numbers so we can check them in the dataframe
ct <- rep(1:(n_cts * n_arrays),
          each = n_obs)

array <-rep (1:n_arrays,
             each = n_cts * n_obs) %>% 
  
  as.factor() # since array is a group, not a numeric variable

# simulate 'collected' data
# for landscape data: proportional coverage ranges from 0 to 1
sim_nat_land <- rep(runif(n_cts * n_arrays,
                          min = 0,
                          max = 1), # uniform draws from 0 to 1
                    each = n_obs) 

sim_wide_lf <- rep(runif(n_cts * n_arrays,
                         min = 0,
                         max = 1), # uniform draws from 0 to 1
                   each = n_obs)

# for independent detection data: number of detections ranges from scaled min to scaled max for that species, based on actual data
sim_wtd <- rep(runif(n_cts * n_arrays,
                     min = min(scale(coyote_data$white_tailed_deer)), # from min of scaled empirical data
                     max = max(scale(coyote_data$white_tailed_deer))), # from max of scaled empirical data
               each = n_obs)

sim_moose <- rep(runif(n_cts * n_arrays,
                       min = min(scale(coyote_data$moose)),
                       max = max(scale(coyote_data$moose))),
                 each = n_obs)

sim_squirrel <- rep(runif(n_cts * n_arrays,
                          min = min(scale(coyote_data$red_squirrel)),
                          max = max(scale(coyote_data$red_squirrel))),
                    each = n_obs)

sim_hare <- rep(runif(n_cts * n_arrays,
                      min = min(scale(coyote_data$snowshoe_hare)),
                      max = max(scale(coyote_data$snowshoe_hare))),
                each = n_obs)

sim_wolf <- rep(runif(n_cts * n_arrays,
                      min = min(scale(coyote_data$grey_wolf)),
                      max = max(scale(coyote_data$grey_wolf))),
                each = n_obs)

sim_lynx <- rep(runif(n_cts * n_arrays,
                      min = min(scale(coyote_data$lynx)),
                      max = max(scale(coyote_data$lynx))),
                each = n_obs)

sim_fisher <- rep(runif(n_cts * n_arrays,
                        min = min(scale(coyote_data$fisher)),
                        max = max(scale(coyote_data$fisher))),
                  each = n_obs)


# simulate random effect of array
array_effect <- rep(rnorm(n_arrays,
                          mean = 0,
                          sd = array_sd),
                    each = n_cts * n_obs)

# check this by wrapping into a dataframe
df <- data.frame(ct,
                 array,
                 array_effect,
                 sim_nat_land,
                 sim_wide_lf,
                 sim_wtd,
                 sim_moose,
                 sim_squirrel,
                 sim_hare,
                 sim_wolf,
                 sim_lynx,
                 sim_fisher)

# calculate the linear predictor for each observation
linear_pred <-
  
  b0 +
  b1 * sim_nat_land +
  b2 * sim_wide_lf +
  b3 * sim_wtd +
  b4 * sim_moose +
  b5 * sim_squirrel +
  b6 * sim_hare +
  b7 * sim_wolf +
  b8 * sim_lynx +
  b9 * sim_fisher +
  array_effect

# convert linear predictors to probabilities, using logit link function
prob <- plogis(linear_pred)

# simulate Bernoulli trials based on probabilities, with variable effort per camera
# defining 4 to 15 sampling opportunities (= months) per trial - these are min/max deployment durations (where camera = active)
total_trials <- sample(4:15,
                       n,
                       replace = TRUE)

# use random number generator to determine number of successes (presences) and failures (absences)
# run as many times as observations in dataset
present <- rbinom(n,
                  size = total_trials,
                  prob = prob)

absent <- total_trials - present

# wrap everything into the dataframe
df <- data.frame(ct,
                 array,
                 array_effect,
                 sim_nat_land,
                 sim_wide_lf,
                 sim_wtd,
                 sim_moose,
                 sim_squirrel,
                 sim_hare,
                 sim_wolf,
                 sim_lynx,
                 sim_fisher,
                 linear_pred,
                 prob,
                 total_trials,
                 present,
                 absent)

# fit GLMM to simulated data
sim_global <- glmer(
  cbind(present, absent) ~
    sim_nat_land +
    sim_wide_lf +
    sim_wtd +
    sim_moose +
    sim_squirrel +
    sim_hare +
    sim_wolf +
    sim_lynx +
    sim_fisher +
    (1|array),
  data = df,
  family = binomial)

summary(sim_global)

# OK!! now everything looks good and works as it should!
# wrap everything above into a function so we can simulate heaps of times

# data-generation, modelling and model selection function
coyote_glmm_sim = function(n_cts = 40,
                           n_arrays = 6,
                           n_obs = 1,
                           n = n_cts * n_arrays * n_obs,
                           b0 = -1.4,
                           b1 = -0.40,
                           b2 = 0.50,
                           b3 = 0.06,
                           b4 = -0.06,
                           b5 = 0.08,
                           b6 = 0.19,
                           b7 = 0.19,
                           b8 = 0.17,
                           b9 = 0.02,
                           array_sd = 0.28)
{
  ct <- rep(1:(n_cts * n_arrays),
            each = n_obs)
  
  array <- rep (1:n_arrays,
               each = n_cts * n_obs) %>% 
    as.factor()
  
  sim_nat_land <- rep(runif(n_cts * n_arrays,
                            min = 0,
                            max = 1), # uniform draws from 0 to 1
                      each = n_obs) 
  
  sim_wide_lf <- rep(runif(n_cts * n_arrays,
                           min = 0,
                           max = 1), # uniform draws from 0 to 1
                     each = n_obs)
  
  sim_wtd <- rep(runif(n_cts * n_arrays,
                       min = min(scale(coyote_data$white_tailed_deer)), # from min of scaled empirical data
                       max = max(scale(coyote_data$white_tailed_deer))), # from max of scaled empirical data
                 each = n_obs)
  
  sim_moose <- rep(runif(n_cts * n_arrays,
                         min = min(scale(coyote_data$moose)),
                         max = max(scale(coyote_data$moose))),
                   each = n_obs)
  
  sim_squirrel <- rep(runif(n_cts * n_arrays,
                            min = min(scale(coyote_data$red_squirrel)),
                            max = max(scale(coyote_data$red_squirrel))),
                      each = n_obs)
  
  sim_hare <- rep(runif(n_cts * n_arrays,
                        min = min(scale(coyote_data$snowshoe_hare)),
                        max = max(scale(coyote_data$snowshoe_hare))),
                  each = n_obs)
  
  sim_wolf <- rep(runif(n_cts * n_arrays,
                        min = min(scale(coyote_data$grey_wolf)),
                        max = max(scale(coyote_data$grey_wolf))),
                  each = n_obs)
  
  sim_lynx <- rep(runif(n_cts * n_arrays,
                        min = min(scale(coyote_data$lynx)),
                        max = max(scale(coyote_data$lynx))),
                  each = n_obs)
  
  sim_fisher <- rep(runif(n_cts * n_arrays,
                          min = min(scale(coyote_data$fisher)),
                          max = max(scale(coyote_data$fisher))),
                    each = n_obs)
  
  array_effect <- rep(rnorm(n_arrays,
                            mean = 0,
                            sd = array_sd),
                      each = n_cts * n_obs)
  
  linear_pred <-
    
    b0 +
    b1 * sim_nat_land +
    b2 * sim_wide_lf +
    b3 * sim_wtd +
    b4 * sim_moose +
    b5 * sim_squirrel +
    b6 * sim_hare +
    b7 * sim_wolf +
    b8 * sim_lynx +
    b9 * sim_fisher +
    array_effect
  
  prob <- plogis(linear_pred)
  
  total_trials <- sample(4:15,
                         n,
                         replace = TRUE)
  
  present <- rbinom(n,
                    size = total_trials,
                    prob = prob)
  
  absent <- total_trials - present
  
  # wrap everything into the dataframe
  df <- data.frame(ct,
                   array,
                   array_effect,
                   sim_nat_land,
                   sim_wide_lf,
                   sim_wtd,
                   sim_moose,
                   sim_squirrel,
                   sim_hare,
                   sim_wolf,
                   sim_lynx,
                   sim_fisher,
                   linear_pred,
                   prob,
                   total_trials,
                   present,
                   absent)
  
  # fit GLMMs to simulated data
  sim_global <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wide_lf +
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_hare +
      sim_wolf +
      sim_lynx +
      sim_fisher +
      (1|array),
    data = df,
    family = binomial)
 
  sim_lc <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      (1|array),
    data = df,
    family = binomial)
  
  sim_wide_lc <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wide_lf +
      (1|array),
    data = df,
    family = binomial)
  
  sim_prey <- glmer(
    cbind(present, absent) ~
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_hare +
      (1|array),
    data = df,
    family = binomial)
  
  sim_prey_lc <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_hare +
      (1|array),
    data = df,
    family = binomial)
  
  sim_comp <- glmer(
    cbind(present, absent) ~
      sim_wolf +
      sim_lynx +
      sim_fisher +
      (1|array),
    data = df,
    family = binomial)
  
  sim_comp_lc <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wolf +
      sim_lynx +
      sim_fisher +
      (1|array),
    data = df,
    family = binomial)
  
  sim_prey_wide <- glmer(
    cbind(present, absent) ~
      sim_wide_lf +
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_hare +
      (1|array),
    data = df,
    family = binomial)
  
  sim_comp_wide <- glmer(
    cbind(present, absent) ~
      sim_wide_lf +
      sim_wolf +
      sim_lynx +
      sim_fisher +
      (1|array),
    data = df,
    family = binomial)
  
  sim_prey_wide_lc <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wide_lf +
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_hare +
      (1|array),
    data = df,
    family = binomial)
    
  sim_comp_wide_lc <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wide_lf +
      sim_wolf +
      sim_lynx +
      sim_fisher +
      (1|array),
    data = df,
    family = binomial)
  
  sim_global_int <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_lynx +
      sim_fisher +
      sim_wide_lf * sim_hare +
      sim_wide_lf * sim_wolf +
      (1|array),
    data = df,
    family = binomial)
  
  sim_prey_int <- glmer(
      cbind(present, absent) ~
        sim_wtd +
        sim_moose +
        sim_squirrel +
        sim_wide_lf * sim_hare +
        (1|array),
      data = df,
      family = binomial)
  
  sim_comp_int <- glmer(
    cbind(present, absent) ~
      sim_lynx +
      sim_fisher +
      sim_wide_lf * sim_wolf +
      (1|array),
    data = df,
    family = binomial)

  sim_prey_lc_int <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_wtd +
      sim_moose +
      sim_squirrel +
      sim_wide_lf * sim_hare +
      (1|array),
    data = df,
    family = binomial)
    
  sim_comp_lc_int <- glmer(
    cbind(present, absent) ~
      sim_nat_land +
      sim_lynx +
      sim_fisher +
      sim_wide_lf * sim_wolf +
      (1|array),
    data = df,
    family = binomial)
  
  sim_hsel <- model.sel(null,
                        sim_global,
                        sim_lc,
                        sim_wide_lc,
                        sim_prey,
                        sim_prey_lc,
                        sim_comp,
                        sim_comp_lc,
                        sim_prey_wide,
                        sim_comp_wide,
                        sim_prey_wide_lc,
                        sim_comp_wide_lc,
                        sim_global_int,
                        sim_prey_int,
                        sim_comp_int,
                        sim_prey_lc_int,
                        sim_comp_lc_int)
  
  # returns parameter name and beta coefficient for the global model
  sim_pars <- get_parameters(sim_global)
  
  # transforms sim_hsel into a tibble with models as rownames and extracts the top model
  top_mod <-  sim_hsel %>% 
    
    as_tibble(rownames = 'model') %>%
    
    select(model) %>% 
    
    slice(1)
  
  # return just the bits we want
  return(list(sim_pars,
              top_mod))
} 

# replicate function many times
glmm_sim <- replicate(1000,
                 coyote_glmm_sim(),
                 simplify = FALSE)

# extract model coefficients
sim_results <- 
  
  glmm_sim %>%
  
  map_dfr(bind_rows,
          .id = 'simulation')

# save simulation results as .csv in 'processed' data folder
write_csv(sim_results,
          'data/processed/simulation_output.csv')

# 6) visualizations ----------------------------------------------------------------------

# move onto coyote_figures script :-)
