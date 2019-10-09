library(tidyverse)
library(brms)

################
# LOAD RT DATA #
################
D = read.csv('CMC_rt_all.csv') %>%
  mutate(
    fixation_duration = factor(fixation_duration),
    CMC_name = relevel(CMC_name, 'pitch:shape')  # (post-hoc: Best represents the other RTs. Better for MCMC mixing)
  )

# Lazy way of loading the fit
fit_rat = brm(rt ~ congruent_rating, D, file='jobs/rat')
fit_exp = brm(rt ~ congruent_rating, D, file='jobs/exp')

#################################
# EXPLORATIVE: FACTOR STRUCTURE #
#################################

# Predict for all combinations of id and CMC_name (all rows in D_predict)
# ... and for a particular no_global and fixation_duration
D_CMC_wide = expand.grid(levels(D$CMC_name), unique(D$id)) %>%
  rename(CMC_name = Var1, id = Var2) %>%
  mutate(
    task = unlist(map(CMC_name, function(x) unlist(str_split(x, ':'))[1])),
    no_global = 0,
    fixation_duration = 50,
    bad_lag = 0,
    
    congruent_rating = 1,  # Get ready for congruent...
  ) %>%
  
  # Add predictions from model! Congruent:
  mutate(
    congruent_rt = predict(fit_rat, .)[,1],  # I don't feel good about this. Should use the median?
    congruent_rating = 0, # Get ready for incongruent...
  ) %>% 
  # Add predictions from model! Incongruent:
  mutate(
    
    incongruent_rt = predict(fit_rat, .)[,1]
  ) %>%
  # Compute the CMC effect:
  mutate(CMC_effect = incongruent_rt - congruent_rt) %>%
  
  # Put it into a wide format for factor analysis/SEM
  select(id, CMC_name, CMC_effect) %>%
  spread(CMC_name, CMC_effect) %>%
  column_to_rownames('id')  # Makes the first column the row name


# Exploratory factor analysis
library(BayesFM)
fa = befa(D_CMC_wide, Nid = 2)
fa = post.sign.switch(post.column.switch(fa))
summary(fa)


# See alternative factor structures (if there are any)
summary(fa, what='all', min.prob=0.001)




######################################
# CONFIRMATORY: COMPARING SEM MODELS #
######################################
library(blavaan)  # Bayesian version of lavaan, implemented in JAGS. Make sure to install JAGS from https://sourceforge.net/projects/mcmc-jags/files/JAGS/

# Prepare
names(D_CMC_wide) = gsub(':', '', names(D_CMC_wide))  # remove colons, so lavaan does not think that these are interaction terms

# One-factor model
efa_model_all = 'CMC =~ lightnesspitch + lightnesstimbre + shapepitch + shapetimbre + pitchlightness + pitchshape + timbrelightness + timbreshape'
fit_efa_all = bcfa(efa_model_all, data=D_CMC_wide, bcontrol=list(method="rjparallel"))

# Task Modality model
efa_model_modality = '
  visual =~ lightnesspitch + lightnesstimbre + shapepitch + shapetimbre
  auditory =~ pitchlightness + pitchshape + timbrelightness + timbreshape
  CMC =~ visual + auditory
'
fit_efa_modality = bcfa(efa_model_modality, data=D_CMC_wide, bcontrol=list(method="rjparallel"))

# CMC bidirectionality model
efa_model_bidirectional = '
  lp =~ lightnesspitch + pitchlightness
  lt =~ lightnesstimbre + timbrelightness
  sp =~ shapepitch + pitchshape
  st =~ shapetimbre + timbreshape
  CMC =~ lp + lt + sp + st
'
fit_efa_bidirectional = bcfa(efa_model_bidirectional, data=D_CMC_wide, bcontrol=list(method="rjparallel"))

# Comparing all models
blavCompare(fit_efa_modality, fit_efa_all)
blavCompare(fit_efa_bidirectional, fit_efa_all)
blavCompare(fit_efa_bidirectional, fit_efa_modality)  # Strictly not neccessary since it can be computed from the others



####################
# BIDIRECTIONALITY #
####################
# # Is this not superseeded by the 
# library(BayesFactor)
# 
# # Each row indicates columns in D_CMC_wide which will be correlated
# CMC_bi = data.frame(
#   first = c('shapepitch', 'shapetimbre', 'lightnesspitch', 'lightnesstimbre'),
#   second = c('pitchshape', 'timbreshape', 'pitchlightness', 'timbrelightness')
# ) %>%
#   mutate_at(c('first', 'second'), as.character)
# 
# for(row in 1:nrow(CMC_bi)) {
#   
#   print(CMC_bi[row, ])
#   print('')
#   # Do the two-sided correlation on an interval null on a flat prior
#   # I would personally argue for one-sided positive (c(-0.1, 1)) but you can't always get it your way :-)
#   BF_data = correlationBF(
#     x = D_CMC_wide[, CMC_bi[row, 'first']], 
#     y = D_CMC_wide[, CMC_bi[row, 'second']], 
#     nullInterval = c(-0.1, 0.1),
#     rscale = 1  # flat prior (beta(x, 1/rscale, 1/rscale))
#   )
#   
#   # Add BF to CMC_bi
#   #CMC_bi$BF[row] = BF_data[1] / BF_data[2]  # Shorter version - not tested
#   print(BF_data)
#   CMC_bi$BF[row] = exp(BF_data@bayesFactor$bf[2] - BF_data@bayesFactor$bf[1])  # Evidence for the alternative "intervals" over the null interval
#   
#   # Add correlation estimates to CMC_bi
#   # Due to a bug in BayesFactor::correlationBF, posterior is only correct without nullInterval
#   BF_rho = correlationBF(BF_data@data$x, BF_data@data$y, posterior=TRUE, iterations=10^4)
#   quantiles = summary(BF_rho)$quantiles[1,]
#   CMC_bi$'2.5%'[row] = quantiles[1]
#   CMC_bi$'50%'[row] = quantiles[3]
#   CMC_bi$'97.5%'[row] = quantiles[5]
# }
# 
# # Show the results
# CMC_bi



######################################################
# SUPPLEMENTARY: INDIVIDUAL CORRELATION COEFFICIENTS #
######################################################
# (Includes an assessment of bidirectionality)

# Extract correlations from random effects
ran_corrs = VarCorr(fit_exp)$id$cor[1:8, 1:4, 1:8] %>%  # Only relevant correlations
  reshape2::melt() %>%  # Flatten it to data.frame
  rename(CMC1 = Var1, CMC2 = Var3) %>%
  
  # Get just the name of the CMC effects
  mutate(
    CMC1 = str_remove(CMC1, 'CMC_name'),
    CMC1 = str_remove(CMC1, ':congruent_rating'),
    CMC1 = str_remove(CMC1, ':congruent_exp'),
    CMC2 = str_remove(CMC2, 'CMC_name'),
    CMC2 = str_remove(CMC2, ':congruent_rating'),
    CMC2 = str_remove(CMC2, ':congruent_exp')
  ) %>%
  spread(Var2, value) %>%
  rename(r = Estimate) %>%
  
  # Select unique vlues
  filter(CMC1 != CMC2, !duplicated(CMC1, CMC2)) %>%
  filter(!duplicated(paste0(pmax(CMC1, CMC2), pmin(CMC1, CMC2))))  # OK, this is copy-pasted from SO. Almost understand it.

ran_corrs %>% filter(abs(r) > 0.4)


# Plot it
ggplot(ran_corrs, aes(x=paste(CMC1, CMC2, sep=' ~ '), y=r)) +
  geom_point() +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5)) + 
  coord_flip() + 
  #theme(axis.text.x = element_text(angle=90)) + 
  geom_hline(yintercept=0) + 
  labs(title='CMC effects correlations', x = 'CMC pair', y = 'Pearson\'s r')

# Seems to be distributed aroudn zero:
hist(ran_corrs$r)
stem(ran_corrs$r)

# Some summaries
sum(abs(ran_corrs$r) > 0.4)
sum(ran_corrs$r < 0)

# Alternative: plot from predicted RTs (too optimistic because it assumes no measurement error)
GGally::ggpairs(D_CMC_wide) + 
  theme_bw(13) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) + 
  ggtitle('Pairwise correlations between scores')
