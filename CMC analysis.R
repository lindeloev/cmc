library(tidyverse)
library(lme4)
library(brms)


################
# TRANSLATIONS #
################

# These are equivalent. Used to homogenize naming
CMC_codes = list(
  # Lightness:Pitch
  'dark:high'='dark:high',
  'light:low'='dark:high',
  'dark:low'='dark:low',
  'light:high'='dark:low',
  
  # Pitch:Lightness
  'high:dark'='high:dark',
  'low:light'='high:dark',
  'low:dark'='low:dark',
  'high:light'='low:dark',
  
  # Lightness:Timbre
  'dark:sine'='dark:sine',
  'light:square'='dark:sine',
  'dark:square'='dark:square',
  'light:sine'='dark:square',
  
  # Timbre:Lightness
  'sine:dark'='sine:dark',
  'square:light'='sine:dark',
  'square:dark'='square:dark',
  'sine:light'='square:dark',
  
  # Shape:Pitch
  'smooth:low'='smooth:low',
  'pointy:high'='smooth:low',
  'smooth:high'='smooth:high',
  'pointy:low'='smooth:high',
  
  # Pitch:Shape
  'low:smooth'='low:smooth',
  'high:pointy'='low:smooth',
  'low:pointy'='low:pointy',
  'high:smooth'='low:pointy',
  
  # Shape:Timbre
  'smooth:square'='smooth:square',
  'pointy:sine'='smooth:square',
  'smooth:sine'='smooth:sine',
  'pointy:square'='smooth:sine',
  
  # Timbre:Shape
  'square:smooth'='square:smooth',
  'sine:pointy'='square:smooth',
  'sine:smooth'='sine:smooth',
  'square:pointy'='sine:smooth'
)

# Experimenter-defined CMC directions. 
# 1 = congruent. 
# 0 = incongruent
CMC_directions = list(
  # lightness:pitch
  'dark:high'=0,
  'dark:low'=1,
  'light:high'=1,
  'light:low'=0,
  
  # lightness:timbre
  'dark:sine'=0,
  'dark:square'=1,
  'light:sine'=1,
  'light:square'=0,
  
  # shape:pitch
  'pointy:high'=1,
  'pointy:low'=0,
  'smooth:high'=0,
  'smooth:low'=1,
  
  # shape:timbre
  'pointy:sine'=0,
  'pointy:square'=1,
  'smooth:sine'=1,
  'smooth:square'=0
)

#  Add in names in reverse order
for(i in 1:length(CMC_directions)) {
  exploded = strsplit(names(CMC_directions[i]), ':')[[1]]
  identical_name = paste(exploded[2], exploded[1], sep=':')
  CMC_directions[identical_name] = CMC_directions[[i]]
}



# These are identical. Used to homogonize naming
CMC_types = list(
  'lightness:pitch'='lightness:pitch',
  'pitch:lightness'='lightness:pitch',
  'lightness:timbre'='lightness:timbre',
  'timbre:lightness'='lightness:timbre',
  'shape:pitch'='shape:pitch',
  'pitch:shape'='shape:pitch',
  'shape:timbre'='shape:timbre',
  'timbre:shape'='shape:timbre'
)



##########################
# PREPROCESS RATING DATA #
##########################
test_features = c('lightness', 'shape', 'pitch', 'timbre')

# Load data and preprocess it
R_all = readbulk::read_bulk('data exp 2018.11 rate/', sep=';') %>%
  # Remove subjects
  filter(id != 509) %>%  # Had a 75% hearing loss
  filter(id != 510) %>%  # Technical problems and noise in the lab
  
  # RE-format a few things
  mutate_at(vars(task, inducer, right), as.character) %>%  # Make character, not factor
  
  # Add congruency codings
  mutate(
    key = key - 4,  # Center key ratings. Positive = preference. Negative = opposite.
    congruent_rating = case_when(
      key < 0 ~ 0,  # incongruent with right
      key > 0 ~ 1,  # congruent
      key == 0 ~ NA_real_
    ),  # No preference #sign(key)*0.5+0.5,  # 0=incongruent, 1=congruent
    congruent_exp = as.numeric(as.character(CMC_directions[right]))  # 0=incongruent, 1=congruent
  ) %>%
  
  # Select
  select(id, task, inducer, right, key, congruent_rating, congruent_exp)

# Select RT-relevant R data and summarise it
R = R_all %>%
  # Select relevant
  filter(inducer %in% test_features, task %in% test_features) %>%
  
  # Mean ratings for each
  group_by(id, task, inducer, right) %>%
  summarise(key = mean(key)) %>%
  
  # Arrange nicely for aesthetics
  arrange(id, task, inducer) %>%
  mutate(CMC_name = paste(task, inducer, sep=':'))



########################
# PREPROCESS TEST DATA #
########################
# Load the raw data
D_all = readbulk::read_bulk('data exp 2018.11 test/', sep=';')

# Too low accuracy on catch trials
bad_subjects_catch = D_all %>%
  filter(phase == 'exp', catch == 1) %>%
  group_by(id, task, superblock) %>%
  summarise(score = mean(score, na.rm=T)) %>%
  filter(score < 0.75, score > 0.25)

# Too low accuracy on task trials
bad_subjects_task = D_all %>%
  filter(phase == 'exp', catch == 0) %>%
  group_by(id, task, superblock) %>%
  summarise(score = mean(score, na.rm=T)) %>%
  filter(score < 0.75)

bad_subjects_catch
bad_subjects_task


# Data
D_all = D_all %>%
  # Select the data we want to work with
  anti_join(bad_subjects_catch, c('id', 'task', 'superblock')) %>%
  anti_join(bad_subjects_task, c('id', 'task', 'superblock')) %>%
  
  # Remove subjects
  filter(id != 509) %>%  # Had a 75% hearing loss
  filter(id != 510) %>%  # Technical problems and noise in the lab
  
  # Re-format a few things
  mutate_at(vars(task, inducer, CMC_code), as.character) %>%  # Make character, not factor
  mutate_at(vars(id, flip, fixation_duration), as.factor) %>%  # Make factor, not float
  mutate(
    CMC_name = paste(task, inducer, sep=':'),
    CMC_type = as.character(CMC_types[CMC_name]),  # Translate it to be direction-agnostic
    no_global = (no_global - mean(no_global))/1736  # scale+center to improve sampling. Scaled so that slope is difference from start to finish.
  )

# Filter for main analysis
D = D_all %>%
  filter(phase == 'exp', catch == 0) %>%  # Experimental condition
  filter(catch_key=='', score == 1)  # Correct responses

# Add feature combinations
for(row in 1:nrow(D)) {
  D$CMC_code[row] = paste(D[row, D$task[row]], D[row, D$inducer[row]], sep=':')
  D$CMC_code[row] = CMC_codes[[D$CMC_code[row]]]  # Simplify by ignoring order
}



###################################################
# COMPUTE CONGRUENCY BY MERGING RATINGS INTO TEST #
###################################################

# For each participant
for(id_this in unique(D$id)) {
  # Get the ratings from R for this participant
  ratings = filter(R, id==id_this)
  
  # Loop through each rating and update D with congruent/incongruent for this
  for(rating in 1:nrow(ratings)) {
    # Handy variables
    rating = ratings[rating, ]  # one row
    task_inducer = c(rating$task, rating$inducer)
    rows = D$id == id_this & 
      D$task %in% task_inducer & 
      D$inducer %in% task_inducer
    
    # Add rating for feature pairs congruent with rating$right
    same_order = c(rating$right, paste(rev(strsplit(rating$right, ':')[[1]]), collapse=':'))
    rows_same_order = rows & D$CMC_code %in% same_order
    D$congruency[rows_same_order] = rating$key
    
    
    # Add inverse rating for feature pairs incongruent (the rest)
    rows_reversed_order = rows & is.na(D$congruency)
    D$congruency[rows_reversed_order] = -rating$key
    
    # Congruent based on rating?
    D$congruent_rating = as.integer(D$congruency > 0)
    D$congruent_rating[D$congruency == 0] = NA
    
    # Add experimenter-definde congruency
    D$congruent_exp = as.numeric(CMC_directions[D$CMC_code])
  }
}

# Remove based on RTs
# Remove RTs below 1.5 SD for each participant-task combination 
D = D %>%
  # Remove too fast RTs and too slow in absolute terms
  filter(rt > 0.200) %>%
  
  # Remove too slow rts for each id:task:congruent combination
  filter(rt < 3) %>%  # Just because very long RTs will affect the SD a lot and are clearly not a part of the decision process
  group_by(id, task, congruent_rating) %>%
  filter(rt < mean(rt) + sd(rt)*2) %>%  # Ratcliff (1993)
  ungroup()

write.table(D, 'CMC_rt_all.csv', sep=',', row.names = FALSE)
write.table(R_all, 'CMC_rating_all.csv', sep=',', row.names = FALSE)



####################
# LOOK AT THE DATA #
####################

# Load data and add factors suitable for inference
D = read.csv('CMC_rt_all.csv') %>%
  mutate(
    fixation_duration = factor(fixation_duration),
    CMC_name = relevel(CMC_name, 'pitch:shape')  # (post-hoc: Best represents the other RTs. Better for MCMC mixing)
  )


# CMC crude estimate (not weighted according to N trials)
q_exp = aggregate(rt ~ congruent_exp + task + inducer, D, FUN=mean)  # Experimenter-defined 
q = aggregate(rt ~ congruent_rating + task + inducer, D, FUN=mean)  # participant-defined congruency
q$ms_diff = round(c(rbind(rep(NA, nrow(q) / 2), diff(q$rt)[seq(1, nrow(q), 2)])), 3)*1000
q$ms_diff_exp = round(c(rbind(rep(NA, nrow(q_exp) / 2), diff(q_exp$rt)[seq(1, nrow(q_exp), 2)])), 3)*1000
q

# Individual subjects grouped by CMC_name
ggplot(D, aes(x=id, y=rt, color=factor(congruent_rating))) + 
  stat_summary(fun.data=mean_cl_boot) + 
  facet_wrap(~CMC_name, scales='free_y') + 
  theme(axis.text.x = element_text(angle=90))

# Individual subjects grouped by subject
ggplot(D, aes(x=CMC_name, y=rt, color=factor(congruent_rating))) + 
  stat_summary(fun.data=mean_cl_boot) + 
  facet_wrap(~id, scales='free_y') + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


# Congruency strength (as rated)
ggplot(D, aes(x=congruency, y=rt, color=CMC_name)) + 
  stat_summary(fun.data=mean_cl_boot) + 
  stat_summary(fun.y=mean, geom='line') +
  facet_wrap(~id, scales='free_y')


# Effect of time on rt.
# Some blocks may have been filtered out in pre-processing
ggplot(D, aes(x=no_global, y=rt, color=paste(superblock, CMC_name))) + 
  geom_smooth() + 
  facet_grid(id~., scales='free_y')



######################
# RATING CONSISTENCY #
######################

# Plot consistency. Summarising for each individual
ggplot(R_all, aes(x=right, y=key, group=id)) + 
  stat_summary(fun.y=mean, geom='point', position=position_jitter(width = 0.1, height = 0)) +  # Points
  geom_abline(intercept=0, slope=0, lty=2) +  # no preference line
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90))



#######################################
# ANALYSIS: TEST PUBLISHED CMCs ON RT #
#######################################
# Set number of cores to do parallel NUTS sampling in brms::brm. Leave one to do housework
n_parallel = parallel::detectCores() - 1
options(mc.cores = n_parallel)

# Model for the experimenter-defined congruency on reaction time for each CMC
# formula_rt_exp = rt ~ 1 + CMC_name*congruent_exp + (0 + task|id)  # Should be cheap and identifiable
# 
# # Regularize using priors - pretty uninformative.
# get_prior(formula_rt_exp, D)
# priors_rt_exp = c(set_prior('normal(0, 0.2)', class='b'),  # Only effects of a few hundred milliseconds are expected here
#            set_prior('gamma(4, 7)', class='Intercept'))
# 
# # Begin sampling!
# fit_rt_exp = brm(
#   formula = formula_rt_exp,
#   prior = priors_rt_exp,
#   data = D,
#   warmup = 100, iter = 600, chains=n_parallel,  # Sampling options
#   save_all_pars = TRUE)

# Complex modelling of design factors and random effects for each CMC per individual
formula_rt_exp_full = rt ~ 1 + CMC_name * congruent_exp + no_global + fixation_duration + (0 + CMC_name:congruent_exp|id) + (0 + task|id)
get_prior(formula_rt_exp_full, D)
curve(dgamma(x, 4, 7), from=0, to=3)  # Visualize the prior on mean RTs
priors_rt_exp_full = c(set_prior('normal(0, 0.2)', class='b'),  # Only effects of a few hundred milliseconds are expected here
                   set_prior('normal(0, 0.2)', class='sd'),  # Some individual differences, but only a few hundred ms (probably much less on congruency-terms than task terms, but singling them out is too much code when it's just regularization)
                   set_prior('gamma(4, 7)', class='Intercept'),  # Most mass between 200 ms and 1 second
                   set_prior('normal(-0.1, 0.2)', coef='no_global'),  # RT decrease over time. -100 ms +/- 200ms from start to finish
                   set_prior('normal(0, 0.1)', coef='fixation_duration75'))  # Likely a very small effect


fit_rt_exp = brm(
  formula = formula_rt_exp_full,
  data = D,
  prior = priors_rt_exp_full,
  warmup = 100, iter = 1000, chains = n_parallel,
  refresh = 1,  # It's very slow, so every sample counts :-)
  save_all_pars = TRUE,  # save_all_pars for bayes_factor
  file = 'fit_rt_exp',
  control = list(max_treedepth = 11),
  sample_file = 'fit_rt_exp_chains')  # Be able to inspect chains while sampling

stanfit = fit_rt_exp$fit  # hack to avoid recompiling. See https://discourse.mc-stan.org/t/avoid-recompiling-the-exact-same-stan-model-with-brms/6129/14


# Handy function to test the directionality of the "congruent" terms using brms::hypothesis
test_interactions = function(fit, test_that='< 0') {
  # First, make a hypothesis for each :congruent interaction
  S = data.frame(p = names(fit$fit)) %>%
    filter(grepl('^b_.*congruent', p)) %>%  # Only effects dealing with congruency
    mutate(test = paste(gsub('b_', '', p), ifelse(p =='b_congruent_exp', '', '+ congruent_exp'), test_that))  # Create hypothesis strings
  
  # Now apply brms::hypothesis and print the results nicely
  hypothesis(fit, S$test)$hypothesis %>%
    mutate(
      Hypothesis = gsub('congruent_exp|CMC_name|\\(|\\)|\\+', '', Hypothesis),
      Estimate = round(Estimate * 1000, 1),
      CI.Upper = round(CI.Upper*1000, 1)
    ) %>%  # Just remove clutter
    select(-Est.Error, -Star) %>%  # Remove uninformative info
    print(digits=3)
}

# Actually do the test
test_interactions(fit_rt_exp)

# MODEL FIT: Visualize posterior predictive accuracy
D$predicted = colMeans(posterior_predict(fit_rt_exp))  # Mean prediction
D$residual = D$rt - D$predicted

# Histogram of all residuals and normal approximation
hist(D$resid, breaks=100, freq=FALSE)
curve(dnorm(x, mean(D$resid), sd(D$resid)), add=T)

# For each subject-condition
ggplot(D, aes(x=rt, y=predicted, color=CMC_name)) + 
  #geom_point() +
  stat_summary()
  geom_abline(slope=1, intercept=0) + 
  facet_wrap(~id, scales='free')


#################################################
# HYPOTHESIS: TEST PUBLISHED CMCs ON PREFERENCE #
#################################################
# Only use ratings for CMCs where there are directional hypotheses (RT data)
R_brm = R_all %>%
  filter(!is.na(congruent_exp)) %>%
  mutate_at(vars(key), ordered)

# Priors to regularize it. Otherwise the sampling is terrible
formula_rating = key ~ right*congruent_exp + (1|id)
get_prior(formula_rating,
          family=cumulative(link='logit'),
          data=R_brm)

priors_rating = c(set_prior('normal(0, 2)', class='b'),
           set_prior('normal(0, 2)', class='Intercept'))

# Start sampling
fit_rating = brm(
  formula_rating,
  family=cumulative(link='logit'),
  prior = priors_rating,
  data = R_brm,
  chains=n_parallel,
  save_all_pars = TRUE,
  warmup = 100, iter = 1000,
  file = 'fit_rating'
)
stanfit = fit_rating$fit  # Hack to avoid re-compiling

# Test whether ratings go in the hypothesized direction
test_interactions(fit_rating, test_that = '> 0')


# For fun: would you get approximately the same estimates if you went metric rather than ordinal?
# fit_rating_metric = brm(
#   as.numeric(key) ~ right*congruent_exp + (1|id),
#   prior = priors,
#   data = R_brm
# )
# test_interactions(fit_rating_metric)


##############################################################
# HYPOTHESIS: self-rated is better than experimenter-defined #
##############################################################

# Fit the same model as above, just with participant-delievered ratings
# (congruent_rating) rather than experimenter-defined (congruent_exp)
formula_rt_rating = rt ~ 1 + CMC_name * congruent_rating + no_global + fixation_duration + (0 + CMC_name:congruent_rating|id) + (0 + task|id)

# Run the model. 
fit_rt_rating = brm(
  formula = formula_rt_rating,
  prior = priors_rt_exp,  # We can re-use the priors
  data = D,
  warmup = 100, iter = 1000, chains = n_parallel,
  refresh = 1,  # It's very slow, so every sample counts :-)
  save_all_pars = TRUE,  # save_all_pars for bayes_factor
  file = 'fit_rt_rating',
  control = list(max_treedepth = 11),
  sample_file = 'fit_rt_rating_chains')  # Be able to inspect chains while sampling

# Testing the hypothesis using model comparison!
# BF: larger favors rating over experimenter-defined CMCs
bayes_factor(fit_rt_rating, fit_rt_exp)

# For fun: see if there are evidence for CMCs in rating-defined CMCs.
test_interactions(fit_rt_rating)



#################################
# EXPLORATIVE: FACTOR STRUCTURE #
#################################

# Predict for all combinations of id and CMC_name (all rows in D_predict)
# ... and for a particular no_global and fixation_duration
D_predict = expand.grid(levels(D$CMC_name), unique(D$id)) %>%
  rename(CMC_name = Var1, id = Var2) %>%
  mutate(
    task = unlist(map(CMC_name, function(x) unlist(str_split(x, ':'))[1])),
    no_global = 800,
    fixation_duration = 30,
    congruent_exp = 1  # OBS! Congruent :-)
  )

# Add predictions from model!
D_predict$congruent_rt = predict(fit_rt_rating, D_predict)[,1]
D_predict$congruent_exp = 0
D_predict$incongruent_rt = predict(fit_rt_rating, D_predict)[,1]

# Compute CMC and put it into a wide format for factor analysis/SEM
D_CMC_wide = D_predict %>%
  mutate(CMC_effect = D_predict$incongruent_rt - D_predict$congruent_rt) %>%
  select(id, CMC_name, CMC_effect) %>%
  spread(CMC_name, CMC_effect) %>%
  column_to_rownames('id')  # Makes the first column the row name


# # Compute mean CMC effect from raw data for each subject
# # CLoser to the data, but also more prone to effects of missing data, outliers, etc.
# D_incongruent = D %>%
#   filter(congruent_rating == 0) %>%
#   group_by(id, CMC_name) %>%
#   summarise(rt_incongruent = mean(rt)) %>%
#   arrange(id, CMC_name)
# 
# D_CMC_wide = D %>%
#   # Calculate per-subject-CMC mean rt for congruent trials
#   filter(congruent_rating == 1) %>%
#   group_by(id, CMC_name) %>%
#   summarise(rt_congruent = mean(rt)) %>%
#   ungroup() %>%
#   
#   # Add the incongruent means.
#   inner_join(D_incongruent, by=c('id', 'CMC_name')) %>%
#   
#   # Compute the CMC effect
#   mutate(CMC_effect = rt_incongruent - rt_congruent) %>%
#   select(-rt_incongruent, -rt_congruent) %>%  # We don't need these anymore
#   
#   # Wide format
#   spread(CMC_name, value=CMC_effect) %>%
#   select(-id)

# Exploratory factor analysis
library(BayesFM)
fa = befa(D_CMC_wide, Nid = 2)
fa = post.sign.switch(post.column.switch(fa))
summary(fa)


# See alternative factor structures
# summary(fa, what='all', min.prob=0.01)



######################################
# CONFIRMATORY: COMPARING SEM MODELS #
######################################
library(blavaan)  # Bayesian version of lavaan, implemented in JAGS. Make sure to install JAGS from https://sourceforge.net/projects/mcmc-jags/files/JAGS/

# Prepare
names(D_CMC_wide) = gsub(':', '', names(D_CMC_wide))  # remove colons, so lavaan does not think that these are interaction terms

# One-factor model
efa_model_all = 'CMC =~ lightnesspitch + lightnesstimbre + shapepitch + shapetimbre + pitchlightness + pitchshape + timbrelightness + timbreshape'
fit_efa_all = bcfa(efa_model_all, data=D_CMC_wide, bcontrol=list(method="rjparallel"), test='none')

# Task Modality model
efa_model_modality = '
  visual =~ lightnesspitch + lightnesstimbre + shapepitch + shapetimbre
  auditory =~ pitchlightness + pitchshape + timbrelightness + timbreshape
  CMC ~ visual + auditory
'
fit_efa_modality = bcfa(efa_model_modality, data=D_CMC_wide, bcontrol=list(method="rjparallel"), test='none')

# CMC bidirectionality model
efa_model_bidirectional = '
  lp =~ lightnesspitch + pitchlightness
  lt =~ lightnesstimbre + timbrelightness
  sp =~ shapepitch + pitchshape
  st =~ shapetimbre + timbreshape
  CMC ~ lp + lt + sp + st
'
fit_efa_bidirectional = bcfa(efa_model_bidirectional, data=D_CMC_wide, bcontrol=list(method="rjparallel"), test='none')

# Comparing all models
blavCompare(fit_efa_modality, fit_efa_all)
blavCompare(fit_efa_bidirectional, fit_efa_all)
blavCompare(fit_efa_bidirectional, fit_efa_modality)



####################
# BIDIRECTIONALITY #
####################
library(BayesFactor)

# Each row indicates columns in D_CMC_wide which will be correlated
CMC_bi = data.frame(
  first = c('shapepitch', 'shapetimbre', 'lightnesspitch', 'lightnesstimbre'),
  second = c('pitchshape', 'timbreshape', 'pitchlightness', 'timbrelightness')
)

for(i in 1:nrow(CMC_bi)) {
  # Do the two-sided correlation on an interval null on a flat prior
  # I would personally argue for one-sided positive (c(-0.1, 1)) but you can't always get it your way :-)
  BF_data = correlationBF(
    x = D_CMC_wide[, CMC_bi[row, 'first']], 
    y = D_CMC_wide[, CMC_bi[row, 'second']], 
    nullInterval = c(-0.1, 0.1),
    rscale = 1  # flat prior (beta(x, 1/rscale, 1/rscale))
  )
  
  # Add BF to CMC_bi
  #CMC_bi$BF[row] = BF_data[1] / BF_data[2]  # Shorter version - not tested
  CMC_bi$BF[row] = exp(BF_data@BayesFactor$bf[2] - BF_data@BayesFactor$bf[1])  # Evidence for the alternative "intervals" over the null interval
  
  # Add correlation estimates to CMC_bi
  # Due to a bug in BayesFactor::correlationBF, posterior is only correct without nullInterval
  BF_rho = correlationBF(BF_data@data$x, BF_data@data$y, posterior=TRUE, iterations=10^4)
  quantiles = summary(BF_rho)$quantiles[1,]
  CMC_bi$'2.5%' = quantiles[1]
  CMC_bi$'50%' = quantiles[3]
  CMC_bi$'97.5%' = quantiles[5]
}



##################################
# PERSONALITY AND CMC PROPENSITY #
##################################
# PRE-REGISTRATION NOTE:
# I have no idea what the scale of the personality-data looks like
# so priors will have to be set post-hoc for regularization.

#### PERSONALITY
# Mock-up data for pre-registration
D_neo = data.frame(
  id = rownames(D_CMC_wide),
  O = rep(CMC_propensities/10, 3)*1000,  # Fake a correlation, in ms
  C = rnorm(18),
  E = rnorm(18),
  A = rnorm(18),
  N = rnorm(18))

# Add propensity. Simply the mean CMC effect for that subject
#D_neo$propensity = rowMeans(D_CMC_wide)  
D_neo$propensity = rep(rowMeans(D_CMC_wide), 3)*1000  # in ms

# Regression model: set priors
formula_neo = propensity ~ O + C + E + A + N
get_prior(formula, D_neo)
priors_neo = c(
  set_prior('normal(0, 1)', class='b'),
  set_prior('normal(0, 1)', class='Intercept')
)

# Run the model
fit_neo = brm(
  formula_neo, 
  prior = priors_neo,
  data = D_neo,
  warmup = 100, iter = 2000, chains = n_parallel,
  save_all_pars = TRUE
)

# Did it fit OK?
plot(fit_neo)

# Get parameter estimates
summary(fit_neo)

# Test particular personality traits using Bayes Factors:
test_terms = c(~.-O, ~.-C, ~.-E, ~.-A, ~.-N)
for(term in test_this) {
  fit_neo_noO = update(fit_neo, formula = update(formula_neo, ~.-O))
  bf = bayes_factor(fit_neo, fit_neo_noO, verbose = FALSE)
  print(paste('Bayes Factor for term', term, '=', bf$bf))
}