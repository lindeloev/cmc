library(brms)
library(tidyverse)

# ABOUT:
# remove_unconverged was needed to deal with bad behavior of the guassian model, initially planned.
# test_interactions 


# Handy function to test the directionality of the "congruent" terms using brms::hypothesis
test_interactions = function(fit, test_that='< 0', mult=1, congruent_name='congruent_exp') {
  # First, make a hypothesis for each :congruent interaction
  S = data.frame(p = names(fit$fit)) %>%
    filter(grepl('^b_.*congruent', p)) %>%  # Only effects dealing with congruency
    mutate(test = paste(gsub('b_', '', p), ifelse(p ==paste('b_', congruent_name, sep=''), '', paste('+', congruent_name)), test_that))  # Create hypothesis strings
  
  # Now apply brms::hypothesis and print the results nicely
  hypothesis(fit, S$test)$hypothesis %>%
    mutate(
      Hypothesis = gsub(paste(congruent_name, '|CMC_name|\\(|\\)|\\+', sep=''), '', Hypothesis),
      Hypothesis = ifelse(Hypothesis == ' < 0', 'pitch:shape: < 0', Hypothesis),
      #Hypothesis = fct_recode(Hypothesis, 'pitch:shape < 0' = ' < 0'),
      Estimate = round(Estimate * mult, 1),
      CI.Upper = round(CI.Upper * mult, 1),
      CI.Lower = round(CI.Lower * mult, 1),
      Post.Prob = sprintf('%.1f%%', Post.Prob*100)
    ) %>%  # Just remove clutter
    select(-Est.Error, -Star) %>%  # Remove uninformative info
    #arrange(Hypothesis) %>%
    arrange(match(Hypothesis, c('lightness:timbre: < 0', 'timbre:lightness: < 0', 'lightness:pitch: < 0', 'pitch:lightness: < 0', 'shape:timbre: < 0', 'timbre:shape: < 0', 'shape:pitch: < 0', 'pitch:shape: < 0')))
}




# the function!
remove_unconverged = function(brm_fit, convergence_start) {
  # brm_fit is the output of brms::brm
  # convergence_start is a vector of length [N_chains] with the iteration number to skip until
  sim = brm_fit$fit@sim  # Shorthand
  n_goal = floor(mean(sim$iter - convergence_start))  # Number of post-warmup iterations in each chain after processing
  
  # Loop through chain
  good_samples = data.frame()  # This will hold good samples to transfer to other chains
  for (chain in order(convergence_start)) {  # In order to make sure to build up good samples before transferring them
    S = data.frame(sim$samples[[chain]])
    
    # Remove bad initial samples so that post-warmup iterations now marks the start of convergence
    S = slice(S, convergence_start[chain]:n())
    
    # Add samples from good chains to the "good_samples" pool when there are more than n_goal
    n_donate = (sim$iter - convergence_start[chain]) - floor(n_goal)
    if(n_donate > 0) {
      good_samples = bind_rows(good_samples, tail(S, n_donate))
      S = slice(S, 1:(n() - n_donate))
    }
    
    # When there are less than n_goal, extract from the good_samples pool
    if(n_donate < 0) {  # An "else if" statement
      S = bind_rows(S, tail(good_samples, -(n_donate + 1)))  # Add good samples to S from the tail
      good_samples = slice(good_samples, 1:(n() + n_donate))  # Remove them from good_samples
    }
    
    # A check
    print(sprintf('chain: %i, n_donated: %i, n_pool: %i, n_chain: %i,', chain, n_donate, nrow(good_samples),  nrow(S)))
    
    # Add back to brm_fit, undoing the data.frame stuff
    sim$samples[[chain]] = as.list(S)
    
    # Update stan_args for each chain:
    brm_fit$fit@stan_args[[chain]]$iter = n_goal
  }

  # Update "sim" and add it back to brm_fit
  sim$samples = sim$samples
  sim$iter = n_goal  # Total number of "accepted" iterations
  sim$n_save = n_goal
  brm_fit$fit@sim = sim
  
  # Return
  brm_fit
}


# # Try it out
# x = brm(file = 'fit_rt_exp')  # Load the saved data
# plot(x, pars='sigma')  # Plot it to determine where each chain reaches convergence
# x = remove_unconverged(x, c(0,0,0,0,200,200))
# plot(x, pars='sigma')