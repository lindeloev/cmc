##############################
# TESTING MODELS FOR PAPER 2 #
##############################
library(brms)
library(tidyverse)

n_parallel = parallel::detectCores() - 1
options(mc.cores = n_parallel)

# save(D, file='jobs/D_nofilter.RData')
load('jobs/D_nofilter.RData')
D = D %>%
  filter(!is.na(congruent_rating)) %>%  # Use the same data for all models
  mutate(CMC_name = relevel(factor(CMC_name), 'pitch:shape'))


print('TEST 2')


###########################
# TRY ON SMALL MODEL/DATA #
###########################
model_1 = rt ~ congruent_exp +  no_global + fixation_duration + (congruent_exp|id)

# # Small dataset
D_test = filter(D, CMC_name == 'lightness:pitch')
# 
# # Fit models
fit_full2 = brm(model_1, D_test, file='jobs/full2', save_all_pars = TRUE, chains=2, warmup=300, iter=3000)
# fit_skew2 = brm(model_1, D_test, file='jobs/skew2', save_all_pars = TRUE, chains=2, warmup=300, iter=3000, family=skew_normal())
# fit_ex2 =   brm(model_1, D_test, file='jobs/ex2', save_all_pars = TRUE, chains=2, warmup=300, iter=3000, family=exgaussian())
fit_slog2 = brm(model_1, D_test, file='jobs/slog2', save_all_pars = TRUE, chains=2, warmup=300, iter=3000, family=shifted_lognormal())
# 
# # Add loo
# fit_full2 = add_loo(fit_full2)
# fit_skew2 = add_loo(fit_skew2)
# fit_ex2 = add_loo(fit_ex2)
# fit_slog2 = add_loo(fit_slog2)
# 
# # Inspect fits
# loo_compare(fit_full2, fit_skew2, fit_ex2, fit_slog2)
library('patchwork')
# (pp_check(fit_full2) + pp_check(fit_skew2)) / 
#   (pp_check(fit_ex2) + pp_check(fit_slog2))

plot_gauss = pp_check(fit_full2)
plot_slog = pp_check(fit_slog2)

windowsFonts('arial' = windowsFont("arial"))
(plot_gauss + ggtitle('Normal') + plot_slog + ggtitle('Shifted log-normal') + geom_vline(xintercept=0.189, lty=3, size=1)) * 
  geom_vline(xintercept=0, lty=2) *
  labs(x='Reaction Time', y='Density') * 
  theme(
    text = element_text('arial'),
    plot.title = element_text(size=15),
    legend.position = c(0.7, 0.8),
    legend.text = element_text(size=12)
  ) * 
  scale_color_manual(values=c('black', '#3377FF', 'red'), labels=c('Observed', 'Model prediction', 'shift'), name = NULL) *
  scale_x_continuous(limits = c(-0.2, 2))

plot_gauss + 
  labs(title='Posterior predictive check', x='Reaction TIme', y='Density') + 
  theme(
    plot.title = element_text('arial', size=20),
    legend.position = c(0.8, 0.8),
    legend.text = element_text(size=15)
  ) + 
  scale_color_manual(values=c('black', 'blue'), labels=c('Observed', 'Model fit'), name = NULL) + 
  scale_x_continuous(limits = c(-0.5, 2))


################
# BIGGER MODEL #
################
model_2 = rt ~ CMC_name*congruent_exp + (0 + CMC_name:congruent_exp|id) + (0 + task|id)

# # # Fit models
# fit_slog2_all =   brm(model_2, D, file='jobs/slog2_all', save_all_pars = TRUE, chains=2, refresh=1, warmup=200, iter=2000, family=shifted_lognormal())
# fit_slog2_all2 = brm(model_2, D, file='jobs/slog2_all2', save_all_pars = TRUE, chains=2, refresh=1, warmup=200, iter=2000, family=shifted_lognormal())  # Same again
# 
# # Add loo
# fit_slog2_all = add_loo(fit_slog2_all)
# fit_slog2_all2 = add_loo(fit_slog2_all2)
# 
# # Inspect fits
# loo_compare(fit_slog2_all, fit_slog2_all2)



##############
# FULL MODEL #
##############
model_3 = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + (0 + CMC_name:congruent_exp|id) + (0 + task|id)
model_3_rating = rt ~ congruent_rating*CMC_name + no_global + fixation_duration + (0 + congruent_rating:CMC_name|id) + (0 + task|id)

# # Fit models
# fit_full3_all = brm(model_3, D, file='jobs/full3_all', save_all_pars = TRUE, chains=2, refresh=1, warmup=200, iter=3000)
# fit_slog3_all = brm(model_3, D, file='jobs/slog3_all', save_all_pars = TRUE, chains=2, refresh=1, warmup=200, iter=3000, family=shifted_lognormal())
# fit_ex3 =       brm(model_3, D, file='jobs/fit_ex3',   save_all_pars = TRUE, chains=2, refresh=1, warmup=200, iter=3000, family=exgaussian())  # NOT RUN YET
# 
# # Add loo
# options(mc.cores = NULL)
# fit_full3_all = add_loo(fit_full3_all, cores=2, pointwise = TRUE)  # NOT RUN
# fit_slog3_all = add_loo(fit_slog3_all)  # NOT RUN
# fit_ex3 = add_loo(fit_ex3)  # NOT RUN YET
# 
# # Inspect fits
# loo_compare(fit_full3_all, fit_slog3_all, fit_ex3)



##############
# WITH PRIOR #
##############
# get_prior(model_3, D, family=shifted_lognormal())
prior_slog = c(
  set_prior('normal(-1.0, 0.5)', class='Intercept'),  # Remember that this is *in addition to* the shift (ndt parameter)
  set_prior('normal(0, 0.5)', class='b'),  # Possibly too wide for the congruent_* parameters, but it's tedious to put priors on every single parameter
  set_prior('normal(-0.1, 0.3)', coef='no_global'),  # Perhaps a 100 ms speedup from start to finish, but open to larger effects either way
  set_prior('normal(0, 0.1)', coef='fixation_duration75'),  # +/- 100 ms if Intercept = -0.8.
  set_prior('normal(0.3, 0.4)', coef='bad_lag'),  # Around 100 ms increase in rt, but from 0ms to 500ms not unlikely
  #set_prior('normal(0, 0.2)', class='sd'),  # Would be surprised to see SDs larger than 100 ms in random effects.
  set_prior('normal(0.5, 0.5)', class='sigma')  # Corresponds to around SD = 500 ms
)

prior_gauss = c(
  set_prior('normal(0, 0.2)', class='b'),  # Only effects of a few hundred milliseconds are expected here
  set_prior('normal(0, 0.2)', class='sd'),  # Some individual differences, but only a few hundred ms (probably much less on congruency-terms than task terms, but singling them out is too much code when it's just regularization)
  set_prior('gamma(4, 7)', class='Intercept'),  # Most mass between 200 ms and 1 second
  set_prior('normal(-0.1, 0.2)', coef='no_global'),  # RT decrease over time. -100 ms +/- 200ms from start to finish
  set_prior('normal(0, 0.1)', coef='fixation_duration75'),  # Likely a very small effect
  set_prior('normal(0, 0.3)', class='sigma'))  # Stability of remaining "unexplained" RTs in general, when model parameters have been accounted for


# # Fit models
# fit_slog3_prior =  brm(model_3, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog3_prior',  save_all_pars = TRUE, chains=2, refresh=1, warmup=300, iter=2000)  # Testing
# fit_slog3_vect =   brm(model_3, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog3_vect',   save_all_pars = TRUE, chains=2, refresh=1, warmup=300, iter=2000)
# fit_slog3_prior = add_loo(fit_slog3_prior)
# fit_slog3_vect = add_loo(fit_slog3_vect)


# # Now on rating model!
# fit_slog3_rating =  brm(model_3_rating, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog3_rating',  save_all_pars = TRUE, chains=2, refresh=1, warmup=300, iter=2000)
# fit_slog3_rating = add_loo(fit_slog3_rating)



###################
# LESS COVARIANCE #
###################
model_4 = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + (0 + CMC_name:congruent_exp||id) + (0 + task|id)
# fit_slog4_prior = brm(model_4, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog4_prior',   save_all_pars = TRUE, chains=2, refresh=1, warmup=300, iter=2000)
# fit_slog4_prior = add_loo(fit_slog4_prior)


#################
# NO COVARIANCE #
#################
# Model: variance-only + participant-specific RT offset (should be better for loo)
model_5_exp = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + (0 + CMC_name:congruent_exp||id) + (1 + task||id)
model_5_rat = rt ~ CMC_name*congruent_rating + no_global + fixation_duration + (0 + CMC_name:congruent_rating||id) + (1 + task||id)

# Fit to model 5
# fit_slog5_rat = brm(model_5_rat, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog5_rat', chains=2, refresh=1, warmup=300, iter=2000)  # NOT RUN
# fit_slog5_exp = brm(model_5_exp, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog5_exp', chains=2, refresh=1, warmup=300, iter=2000)
# fit_gauss5_rat = brm(model_5_rat, data = D, prior = prior_gauss, file='jobs/gauss5_rat', chains=2, refresh=1, warmup=300, iter=2000)  # NOT RUN
# fit_gauss5_exp = brm(model_5_exp, data = D, prior = prior_gauss, file='jobs/gauss5_exp', chains=2, refresh=1, warmup=300, iter=2000)
# fit_slog5_exp = add_loo(fit_slog5_exp)
# fit_slog5_rat = add_loo(fit_slog5_rat)  # NOT RUN
# fit_gauss5_exp = add_loo(fit_gauss5_exp)
# fit_gauss5_rat = add_loo(fit_gauss5_rat)  # NOT RUN


###################
# INCLUDE BAD_LAG #
###################
model_6_exp1 = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_exp||id) + (1 + task||id)
model_6_exp2 = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_exp|id) + (1 + task|id)
model_6_rat1 = rt ~ CMC_name*congruent_rating + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_rating||id) + (1 + task||id)
model_6_rat2 = rt ~ CMC_name*congruent_rating + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_rating|id) + (1 + task|id)

# Fit to model 6
# fit_slog6_exp1 = brm(model_6_exp1, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog6_exp1', save_all_pars = TRUE, chains = 2, refresh = 1, warmup = 500, iter = 2000)
# fit_slog6_exp2 = brm(model_6_exp2, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog6_exp2', save_all_pars = TRUE, chains = 2, refresh = 1, warmup = 500, iter = 2000)
# fit_slog6_rat1 = brm(model_6_rat1, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog6_rat1', save_all_pars = TRUE, chains = 2, refresh = 1, warmup = 500, iter = 2000)
# fit_slog6_rat2 = brm(model_6_rat2, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/slog6_rat2', save_all_pars = TRUE, chains = 2, refresh = 1, warmup = 500, iter = 2000)

# fit_slog6_exp1 = add_loo(fit_slog6_exp1)
# fit_slog6_exp2 = add_loo(fit_slog6_exp2)
# fit_slog6_rat1 = add_loo(fit_slog6_rat1)
# fit_slog6_rat2 = add_loo(fit_slog6_rat2)


# loo_compare(fit_slog6_exp1, fit_slog6_exp2, fit_slog6_rat1, fit_slog6_rat2)



##############
# FINAL RUN! #
##############
model_exp = rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_exp|id) + (0 + task|id)
model_rat = rt ~ CMC_name*congruent_rating + no_global + fixation_duration + bad_lag + (0 + CMC_name:congruent_rating|id) + (0 + task|id)

# Fit it!
fit_exp = brm(model_exp, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/exp', save_all_pars = TRUE, chains = 3, refresh = 3, warmup = 500, iter = 4000)
fit_rat = brm(model_rat, data = D, prior = prior_slog, family = shifted_lognormal(), file='jobs/rat', save_all_pars = TRUE, chains = 3, refresh = 3, warmup = 500, iter = 4000)

# Non-conforming participants
D_sub = filter(D, congruent_exp != congruent_rating)
fit_sub = brm(model_rat, data = D_sub, prior = prior_slog, family = shifted_lognormal(), file='jobs/sub', save_all_pars = TRUE, chains = 2, refresh = 3, warmup = 500, iter = 4000)

# x = loo_model_weights(fit_exp, fit_rat, weights = 'pseudobma', nsamples=1000)


# fit_test = brm(rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag + (1|id), 
#                data = D, prior_slog, family=shifted_lognormal(), chains=3, refresh=5, warmup=500, iter=1500, file='jobs/testing')
# fit_test2 = brm(rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag,
#                data = D, prior = prior_slog, chains=3, refresh=5, warmup=500, iter=1500, file='jobs/testing2')
# fit_test3 = brm(rt ~ CMC_name*congruent_exp + no_global + fixation_duration + bad_lag + (CMC_name|id),
#                 data = sample_n(D, 5000), prior = prior_slog, chains=3)



############################
# FIGURE FOR SUPPLEMENTARY #
############################

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




#################
# COMPARE STUFF #
#################
# loo_compare(loo_slog3_prior, loo_slog3_vect, loo_slog4_prior, loo_slog2_all1, loo_slog2_all2)
# loo_compare(loo_slog3_prior, loo_slog3_vect, loo_slog4_prior, loo_slog2_all1, loo_slog2_all2, loo_slog3_all, loo_ex3)


# slog3_all vs. full2_all3: (without priors) how much better is slog than gauss?
# slog3_all vs. slog_all2: Does no_global and fixation_duration add anything?
# slog3_all vs. slog3_prior: Is prior better?
# slog3_prior vs. slog4_prior: is variance-only better?
# slog3_prior vs. slog3_vect: is vectorization better?
# loo_ex3, loo_slog3_all: exgaussian or shifted lognormal?


# loo_compare(fit_slog5_exp, fit_gauss5_exp, fit_slog4_prior, fit_slog3_prior, fit_slog3_vect, fit_slog2_all)

# library(patchwork)
# (pp_check(fit_slog6_exp) + pp_check(fit_slog5_exp)) * coord_cartesian(xlim=c(0.2, 1.5))




#######################
# FREQUENTIST VERSION #
#######################
#q = lme4::lmer(rt ~ CMC_name*congruent_exp + no_global + fixation_duration + (0 + CMC_name:congruent_exp||id) + (1 + task||id), D)



####################################
# TESTING VECTORIZED PRIOR SETTING #
####################################
# library(tidyverse)
# priors_default = get_prior(model_3, D)
# priors_CMC = filter(priors_default, class == 'b', str_detect(coef, 'congruent'))
# priors_task = filter(priors_default, class == 'b', str_detect(coef, '^CMC_name'))
# priors = c(
#   set_prior('normal(0, 1)', class = 'b', coef = priors_CMC$coef),
#   set_prior('gamma(4, 7)', class='b', coef = priors_task$coef)
# )




###############
# CONCLUSIONS #
###############

# WHAT I LEARNED:
# Priors don't change predictions here.
# exgaussian and shifted-lognormal are approximately on par. Gaussian and skew-normal is terrible.
# DON'T TOUCH OPTIONS(MC.CORES) WHEN RUNNING LOO! It will make memory explode.
# No covariance yields same fit (elpd diff < 3*SE), and random intercepts but slightly faster loo.
# (A lot about jobs, code style and loo)
# Six chains --> 100% CPU; 5 chains --> 90% CPU. RUnning 8 chains just prolongs sampling.
# Increasing warmup from 300 to 500 god rid of divergences.




#########
# TO DO #
#########
# Check out loo_predictive_interval vs. "predict"
# fit_slog6_rat2 had only 50 effective samples for Intercept. Does the new more informative prior help?
# Why does gauss sample faster now than before?



######################
# PREDICT USING BRMS #
######################
# Because of the shifted-lognormal model, computing an RT in ms (raw scale)
# is a bit convoluted, given model parameters. Something like:
# rt = ndt + exp(Intercept + b_lightness:pitch + b_lightness:pitch:CMC_namelightness:pitch)
# It is easier to just let brms compute the posterior for a congruent and an 
# incongruent trial (using `fitted`), and subtract the two. This also generalizes
# to other models with different families and link functions.



use_fit = fit_exp
x = tibble(name = c(
  'lightness:pitch',
  'pitch:lightness',
  'lightness:timbre',
  'timbre:lightness',
  'shape:pitch',
  'pitch:shape',
  'shape:timbre',
  'timbre:shape'
)) %>%
  # For each CMC effect...
  rowwise() %>%
  mutate(
    # Add congruent and incongruent trial. list(list()) makes dplyr "see" just one element.
    newdata_c = list(list(congruent_rating = 1, congruent_exp = 1, no_global = 0, bad_lag=0, fixation_duration = 50, CMC_name=name)),
    newdata_ic = list(list(congruent_rating = 0, congruent_exp = 0, no_global = 0, bad_lag=0, fixation_duration = 50, CMC_name=name)),

    # Now get posteriors for each trial
    fitted_c = list(fitted(use_fit, newdata=newdata_c, re_formula = ~ 1, summary = FALSE)),  # Congruent
    fitted_ic = list(fitted(use_fit, newdata=newdata_ic, re_formula = ~ 1, summary = FALSE)),  # Incongruent

    # Calculate the CMC posterior and summaries
    effect = list((fitted_c - fitted_ic) * 1000),  # CMC effect In ms
    median = median(effect),
    low = tidybayes::hdi(effect)[1],
    high = tidybayes::hdi(effect)[2],
    BF = sum(effect < 0) / sum(effect >= 0),
    N = length(unique(filter(use_fit$data, CMC_name == name)$id))  # How many participants contributerd to this
  )




# load('jobs/tables.RData')
# save(table_full3_all, table_gauss5_exp, table_gauss5_rat, table_rt_exp, table_slog5_exp, table_slog6_exp1, table_slog6_exp2, table_slog6_rat1, table_slog6_rat2, table_rat, table_exp, table_sub, loo_exp, loo_rating, file='jobs/tables.RData')

# 
# # Predicted data
# newdata_c$CMC_name = 'pitch:shape'
# hist(filter(D, congruent_rating == 1, fixation_duration == 50, CMC_name == newdata_c$CMC_name)$rt, freq=F, breaks=seq(0, 3, 0.02), xlim=c(0.2, 1), main=paste('observed', newdata_c$CMC_name))
# hist(predict(use_fit, newdata=newdata_c, re_formula = ~ 1, summary = FALSE), freq=F, breaks=seq(0, 3, 0.02), xlim=c(0.2, 1), main=paste('fitted slog', newdata_c$CMC_name))
# hist(predict(fit_slog5_exp, newdata=newdata_c, re_formula = ~ 1, summary = FALSE), freq=F, breaks=seq(-5, 5, 0.02), xlim=c(0.2, 1), main=paste('fitted gauss', newdata_c$CMC_name))
