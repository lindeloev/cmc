# TO DO
# # Save this for legacy but USE OLD DATA with 68299 ROWS!

library(tidyverse)
library(brms)

# Set number of cores to do parallel NUTS sampling in brms::brm. Leave one to do housework
n_parallel = parallel::detectCores() - 1
options(mc.cores = n_parallel)


################
# LOAD RT DATA #
################
D = read.csv('CMC_rt_all.csv') %>%
  filter(!is.na(congruent_rating)) %>%  # Use the same data for all models
  mutate(
    fixation_duration = factor(fixation_duration),
    CMC_name = relevel(CMC_name, 'pitch:shape')  # (post-hoc: Best represents the other RTs. Better for MCMC mixing)
  )


##############
# FIT MODELS #
##############
# Models
model_exp = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_exp|id) + (0 + task|id)  # One-size fits all
model_rat = rt ~ CMC_name*congruent_rating + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_rating|id) + (0 + task|id)  # Rating defined

# Priors
get_prior(model_rat, D, family=shifted_lognormal())
prior = c(
  set_prior('normal(-1.0, 0.5)', class='Intercept'),  # Remember that this is *in addition to* the shift (ndt parameter)
  set_prior('normal(0, 0.5)', class='b'),  # Possibly too wide for the congruent_* parameters, but it's tedious to put priors on every single parameter
  set_prior('normal(-0.1, 0.3)', coef='no_global'),  # Perhaps a 100 ms speedup from start to finish, but open to larger effects either way
  set_prior('normal(0, 0.1)', coef='fixation_duration75'),  # +/- 100 ms if Intercept = -0.8.
  set_prior('normal(0.3, 0.4)', coef='bad_lag'),  # Around 100 ms increase in rt, but from 0ms to 500ms not unlikely
  set_prior('normal(0, 0.2)', class='sd'),  # Would be surprised to see SDs larger than 100 ms in random effects.
  set_prior('normal(0.5, 0.5)', class='sigma')  # Corresponds to around SD = 500 ms
)

# Fit models!
fit_exp = brm(model_exp, data = D, prior = prior, family = shifted_lognormal(), file='jobs/exp', save_all_pars = TRUE, chains = 3, refresh = 3, warmup = 500, iter = 4000)
fit_rat = brm(model_rat, data = D, prior = prior, family = shifted_lognormal(), file='jobs/rat', save_all_pars = TRUE, chains = 3, refresh = 3, warmup = 500, iter = 4000)

# Fit on non-conforming participants
D_sub = filter(D, congruent_exp != congruent_rating)
fit_sub = brm(model_rat, data = D_sub, prior = prior, family = shifted_lognormal(), file='jobs/sub', save_all_pars = TRUE, chains = 2, refresh = 3, warmup = 500, iter = 4000)



####################################
# MAKE TABLE WITH FITTED ESTIMATES #
####################################
# Because of the shifted-lognormal model, computing an RT in ms (raw scale)
# is a bit convoluted, given model parameters. Something like:
# rt = ndt + exp(Intercept + b_lightness:pitch + b_lightness:pitch:CMC_namelightness:pitch)
# It is easier to just let brms compute the posterior for a congruent and an 
# incongruent trial (using `fitted`), and subtract the two. This also generalizes
# to other models with different families and link functions.

make_table = function(fit) {
  # For each of these CMCs...
  tibble(name = c(
    'lightness:pitch',
    'pitch:lightness',
    'lightness:timbre',
    'timbre:lightness',
    'shape:pitch',
    'pitch:shape',
    'shape:timbre',
    'timbre:shape'
  )) %>%
    rowwise() %>%
    
    # ... do this:
    mutate(
      # Add congruent and incongruent trial. list(list()) makes dplyr "see" just one element.
      newdata_c = list(list(congruent_rating = 1, congruent_exp = 1, no_global = 0, bad_lag=0, fixation_duration = 50, CMC_name=name)),
      newdata_ic = list(list(congruent_rating = 0, congruent_exp = 0, no_global = 0, bad_lag=0, fixation_duration = 50, CMC_name=name)),
      
      # Now get posteriors for each trial and their difference (the CMC effect)
      fitted_c = list(fitted(fit, newdata=newdata_c, re_formula = ~ 1, summary = FALSE)),  # Congruent
      fitted_ic = list(fitted(fit, newdata=newdata_ic, re_formula = ~ 1, summary = FALSE)),  # Incongruent
      effect = list((fitted_c - fitted_ic) * 1000),  # samples of CMC effect In ms
      
      # Calculate summaries: median and Highest Density Interval
      median = median(effect),  # Corresponds to the mean (and median) on the log-transformed fit
      low = tidybayes::hdi(effect)[1],
      high = tidybayes::hdi(effect)[2],
      BF = sum(effect < 0) / sum(effect >= 0),  # BF of one-sided hypothesis
      N = length(unique(filter(fit$data, CMC_name == name)$id))  # How many participants contributerd to this
    )
}

# Function to print it nicely (one decimal places)
print_table = function(tab) {
  tab %>%
    mutate(
      #CMC = sprintf('%.1f [%.1f, %.1f]', median, low, high),
      median = round(median, 2),
      CI = sprintf('[%.2f, %.2f]', low, high)
    ) %>%
    select(name, median, CI, BF, N) %>%
    mutate_if(is.numeric, round, digits=2)
}

# Now make the tables
table_exp = make_table(fit_exp) %>% print_table()
table_rat = make_table(fit_rat) %>% print_table()
table_sub = make_table(fit_sub) %>% print_table()

# Merge the tables for the manuscript
tab_all = table_exp %>%
  left_join(table_rat, by='name', suffix = c('_exp', '_rat')) %>%
  left_join(table_sub, by='name')
tab_all

# Save
write.table(tab_all, 'tables/table_fits.csv', sep=',', row.names = FALSE)


# How much faster for preferences?
mean(tab_all$median_exp) - mean(tab_all$median)

# Evidence for "all CMC directions follow preference-ratings"
prod(tab_all$BF)


############################
# FIGURE FOR SUPPLEMENTARY #
############################
# Individual CMC effects
library(tidybayes)
D_fitted_CMCs = D %>%
  # First, select one row per participant per condition
  filter(bad_lag == 0, fixation_duration == 50) %>%
  group_by(id, task, CMC_name, congruent_rating) %>%
  slice(1) %>%
  select(id, task, CMC_name, congruent_rating, rt, bad_lag, no_global, fixation_duration) %>%
  
  # Make predictions
  add_fitted_draws(fit_rat) %>%  # set n = 100 i
  
  # Now nest MCM draws for each data row
  ungroup() %>%
  select(-.row, -no_global, -rt, -.draw) %>%  # These vary from trial to trial
  nest(.value) %>%
  
  # Now make one column for each level of congruent_rating ("wider") and subtract the draws
  spread(congruent_rating, data) %>%
  mutate(con = map2(`0`, `1`, ~ as_tibble(.y - .x))) %>%  # congruent - incongruent
  
  # Back to "longer" format and give proper names
  unnest() %>%
  rename(rt_ic = .value, rt_c = .value1, CMC_effect = .value2)  # ic=incongruent, c=congruent


# Plot with one panel per particiant
ggplot(D_fitted_CMCs, aes(x = CMC_name, y = CMC_effect * 1000)) +
  stat_interval() +
  facet_wrap(~id, ncol=11) +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Individual CMC effects by ID', y='CMC effect (ms). Negative = faster RT for congruent trials.', x = 'Condition')

# Plot with one panel per condition
ggplot(D_fitted_CMCs, aes(x = id, y = CMC_effect * 1000)) +
  stat_interval() +
  facet_grid(CMC_name ~ ., scales = 'free_y') +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title='Individual CMC effects by condition', y='CMC effect (ms). Negative = faster RT for congruent trials.', x = 'Participant ID')