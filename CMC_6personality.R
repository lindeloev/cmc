
##################################
# PERSONALITY AND CMC PROPENSITY #
##################################
# PRE-REGISTRATION NOTE:
# I have no idea what the scale of the personality-data looks like
# so priors will have to be set post-hoc for regularization.

# Load the data.
D_neo = read.csv('NEO-PI-3 201811281124.csv', sep='\t', header = TRUE) %>%
  # Remove subjects
  filter(IndividualCode %in% rownames(D_CMC_wide)) %>%
  
  # More sensible variable names to make the code more readable
  rename(
    # Participant info
    id = Name,
    sex = Sex,
    
    # Trait scores, normalized z-scores
    O = dimension.Aabenhed.norm,
    C = dimension.Samvittighedsfuldhed.norm,
    E = dimension.Ekstroversion.norm,
    A = dimension.Venlighed.norm,
    N = dimension.Neurotiscisme.norm,
    
    # Session info
    duration = Duration,
    norm = Norm
  ) %>%
  
  # Only select the relevant data
  select(id, sex, O, C, E, A, N, duration, norm)

# Inspect response distributions to check if we have a bias towards certain traits
D_neo %>% 
  select(O, C, E, A, N) %>% 
  gather() %>%  # To long format
  ggplot(aes(x=key, y=value)) +
  geom_jitter(width=0.2, height=0)

# Add propensity. Simply the mean CMC effect for that subject
# This works because the IDs are in the same order. Otherwise it would not.
D_neo$propensity = rowMeans(D_CMC_wide)  
#D_neo$propensity = rep(rowMeans(D_CMC_wide), 3)*1000  # in ms

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
  warmup = 70, iter = 1000, chains = n_parallel,
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


# SUPPLEMENTARY
# A table of individual trait-CMC correlations. 
bind_cols(D_CMC_wide, D_neo) %>% 
  select(-id, -sex, -duration, -norm) %>%
  cor() %>%
  
  # It's now a big correlation matrix. 
  # Everything below simply cuts out the relevant info
  data.frame() %>%
  rownames_to_column() %>%
  select(rowname, O, C, E, A, N) %>%
  filter(!rowname %in% c('O', 'C', 'E', 'A', 'N')) %>%
  column_to_rownames() %>%
  round(2)
