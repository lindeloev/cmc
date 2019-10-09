library(tidyverse)
library(brms)

source('CMC_misc.R')  # test_interactions function
# Set number of cores to do parallel NUTS sampling in brms::brm. Leave one to do housework
n_parallel = parallel::detectCores() - 1
options(mc.cores = n_parallel)


#############
# LOAD DATA #
#############
R_all = read.csv('CMC_rating_all.csv') %>%
  # Good order so that RT-tested CMCs appear first (or last?)
  mutate(
    right = fct_rev(fct_relevel(right, 'smooth:sine', 'smooth:low', 'dark:sine', 'dark:low')),
    left = fct_rev(fct_relevel(left, 'smooth:square', 'smooth:high', 'dark:square', 'dark:high'))
  )



#################################################
# HYPOTHESIS: TEST PUBLISHED CMCs ON PREFERENCE #
#################################################
# # Only use ratings for CMCs where there are directional hypotheses (RT data)
# R_brm = R_all %>%
#   #filter(!is.na(congruent_exp)) %>%
#   #mutate_at(vars(left, right), as.character) %>%
#   mutate(key = ordered(key))
# 
# # Priors to regularize it. Otherwise the sampling is terrible
# #formula_rating = key ~ right*congruent_exp + (1|id)
# formula_rating = key ~ right + (1|id)
# get_prior(formula_rating,
#           family=cumulative(link='logit'),
#           data=R_brm)
# 
# priors_rating = c(set_prior('normal(0, 2)', class='b'),
#                   set_prior('normal(0, 2)', class='Intercept'))
# 
# # Start sampling
# fit_rating = brm(
#   formula_rating,
#   family=cumulative(link='logit'),
#   prior = priors_rating,
#   data = R_brm,
#   chains=n_parallel,
#   save_all_pars = TRUE,
#   warmup = 70, iter = 1000,
#   file = 'fit_rating_all'
# )
# stanfit = fit_rating$fit  # Hack to avoid re-compiling
# 
# # Test whether ratings go in the hypothesized direction
# test_rating = test_interactions(fit_rating, test_that = '> 0') %>%
#   mutate(
#     #Hypothesis = str_remove(Hypothesis, 'right'),
#     Hypothesis = c('lightness:pitch', 'lightness:timbre', 'shape:pitch', 'shape:timbre'),
#     Estimate = sprintf('%.1f (%.1f)', Estimate, CI.Lower)
#   ) %>%
#   select(-CI.Upper, -Post.Prob, -CI.Lower) %>%
#   arrange(match(Hypothesis, c('lightness:timbre', 'lightness:pitch', 'shape:timbre', 'shape:pitch'))) %>%
#   print(digits=3)
# 
# fit = fit_rating

# For fun: would you get approximately the same estimates if you went metric rather than ordinal?
fit_rating_metric = brm(
  key ~ 0 + right + (0 + right|id),
  prior = set_prior('normal(0, 2)', class='b'),
  sample_prior = TRUE,
  data = R_all,
  chains = n_parallel,
  iter = 2000,
  file = 'jobs/fit_rating_metric'
)
# test_interactions(fit_rating_metric)


####################################################
# UNPLANNED: TESTING INDIVIDUAL RATING PREFERENCES #
####################################################
# Run hypotheses inference using brms::hypothesis 
hypotheses = paste('right', unique(R_all$right), ' = 0', sep='')
T_preferences = hypothesis(fit_rating_metric, hypotheses)$hypothesis  %>% 
  # Nicer labels
  rename(in_favor_of = Hypothesis) %>%
  mutate(dimension = unique(paste(R_all$task, R_all$inducer, sep=':'))) %>%
  select(dimension, in_favor_of, Estimate, CI.Lower, CI.Upper, Evid.Ratio) %>%
  
  # Nicer display of numbers
  mutate_at(c('Estimate', 'CI.Lower', 'CI.Upper'), round, digits=2) %>%
  mutate(rating_CI = paste(Estimate, ' (', CI.Lower, ' to ', CI.Upper, ')', sep='')) %>%
  mutate(
    BF = ifelse(Evid.Ratio < 0, 0, Evid.Ratio),
    BF = ifelse(BF > 1, round(BF, 3), round(1/BF, 3)),
    BF = ifelse(BF > 10^6, '> 10^6', BF),
    in_favor_of = gsub('(right', '', in_favor_of, fixed=TRUE),
    in_favor_of = gsub(') = 0', '', in_favor_of, fixed=TRUE)
  ) %>%
  select(dimension, in_favor_of, rating_CI, BF)

write.csv(T_preferences, 'tables/table preference inference.csv', row.names = FALSE)
