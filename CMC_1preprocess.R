library(tidyverse)


###############################
# LOAD CONGRUENCY DEFINITIONS #
###############################
# Load congruency table and bind it with bidirectional coding (rev = reversed task/inducer)
# congruency_codings.csv contains "translations" between task, features, and one-size-fits-all congruency
# Note, another much more convoluted method was used in the pre-reg script. This is just much
# easier and flexible.
C = read.csv('congruency_codings.csv', stringsAsFactors=FALSE) %>%
  mutate(
    order = 1,
    CMC_code = paste(stim1, stim2, sep=':'),
    CMC_type = paste(feature1, feature2, sep=':')
  )
C_switch = C %>%  # switching order of features but retaining type and code order
  rename(stim1='stim2', stim2='stim1', feature2='feature1', feature1='feature2') %>%
  mutate(order = 2)
C_both = bind_rows(C, C_switch) %>%
  mutate(CMC_name = paste(stim1, stim2, sep=':'))  # For use with rating-data



##########################
# PREPROCESS RATING DATA #
##########################
test_features = c('lightness', 'shape', 'pitch', 'timbre')

# Load data and preprocess it
R_all = readbulk::read_bulk('data exp 2018.11 rate', sep=';', stringsAsFactors = FALSE) %>%
  # Remove subjects
  filter(id != 509) %>%  # Had a 75% hearing loss
  filter(id != 510) %>%  # Technical problems and noise in the lab
  
  # Copy over experimenter-defined congruency:
  left_join(select(C_both, CMC_name, congruent_exp),
            by = c('right' = 'CMC_name')) %>%
  
  # RE-format a few things
  #mutate_at(vars(task, inducer, right, left), as.character) %>%  # Make character, not factor
  # Score key
  mutate(
    key = key - 4,  # Center key ratings. Positive = preference. Negative = opposite.
    congruent_rating = case_when(
      key < 0 ~ 0,  # incongruent with right
      key > 0 ~ 1,  # congruent
      key == 0 ~ NA_real_
    ),  # No preference #sign(key)*0.5+0.5,  # 0=incongruent, 1=congruent
    
    # Is this congruent as pre-defined? 
    # If yes, shift the scale so that right == congruent and left == incongruent
    # If NA, keep as is
    #congruent_exp = as.numeric(as.character(CMC_directions[right])),  # 0=incongruent, 1=congruent
    key = ifelse(congruent_exp | is.na(congruent_exp), key, -key),
    key = ordered(key),
    
    tmp = right,
    right = ifelse(congruent_exp | is.na(congruent_exp), right, left),
    left = ifelse(congruent_exp | is.na(congruent_exp), left, tmp),
    
    # Now everything with congruent_exp is congruent == right
    congruent_exp = ifelse(!is.na(congruent_exp), 1, NA_real_)
  ) %>%
  
  # Select
  select(id, task, inducer, right, left, key, congruent_rating, congruent_exp, -tmp)



# Select RT-relevant R data and summarise it
R = R_all %>%
  # Select relevant
  filter(inducer %in% test_features, task %in% test_features) %>%
  
  # Mean ratings for each
  group_by(id, task, inducer, right) %>%
  summarise(key = mean(as.numeric(as.character(key)))) %>%
  
  # Arrange nicely for aesthetics
  arrange(id, task, inducer) %>%
  mutate(CMC_name = paste(task, inducer, sep=':'))



######################
# PREPROCESS RT DATA #
######################
# Load the raw data
D_all = readbulk::read_bulk('data exp 2018.11 test', sep = ';', stringsAsFactors = FALSE)


# REMOVE BAD DATA
# Identify blocks with too low accuracy on catch trials or regular trials
bad_blocks = D_all %>%
  filter(phase == 'exp') %>%
  group_by(id, task, superblock, catch) %>%
  summarise(score = mean(score, na.rm=T)) %>%
  filter(score < 0.75, score > 0.25)

# Let's see...
bad_blocks


# Now filter out bad data
D1 = D_all %>%
  # Filter out blocks with too poor performance
  anti_join(bad_blocks, c('id', 'task', 'superblock')) %>%
  
  # Remove subjects
  filter(id != 509) %>%  # Had a 75% hearing loss
  filter(id != 510) %>%  # Technical problems and noise in the lab
  
  # Mark trials which were preceeded by catch, wrong response, or was the first trial
  # RTs are likely affected here, and we model this explicitly 
  group_by(id, superblock, task) %>%
  mutate(bad_lag = lag(catch, 1) == 1 | lag(score, 1) == 0 | no_task == 1,
         bad_lag = as.integer(bad_lag)) %>%  # 0 and 1 dummy coded
  ungroup() %>%
  
  # Remove non-data trials
  filter(phase == 'exp', catch == 0) %>%  # Experimental condition only
  filter(catch_key=='', score == 1) %>%  # Correct responses only
  
  # Remove too fast RTs and too slow in absolute terms
  filter(rt > 0.200, rt < 3.0)

  # Remove too slow rts for each id:task:congruent combination
  #group_by(id, task, congruent_rating) %>%
  #filter(rt < mean(rt) + sd(rt)*2) %>%  # Ratcliff (1993)
  #ungroup()


# NICER DATA FORMAT
D2 = D1 %>%
  #mutate_at(vars(task, inducer, CMC_code), as.character) %>%  # Make character, not factor
  mutate_at(vars(id, flip, fixation_duration), as.factor) %>%  # Make factor, not float
  mutate(CMC_name = paste(task, inducer, sep=':')) %>%
  select(-CMC_code)  %>%  # Will be re-computed later
  mutate(
    #CMC_type = as.character(CMC_types[CMC_name]),  # Translate it to be direction-agnostic
    no_global = (no_global - mean(no_global))/1736  # scale+center to improve sampling. Scaled so that slope is difference from start to finish.
  )
  

# ADD ONE-SIZE-FITS-ALL CONGRUENCY in congruent_exp COLUMN
# Collect the varying features in the current trial in the columns task_feature and inducer_feature
for(i in 1:nrow(D2)) D2$task_feature[i] = as.character(D2[i, D2$task[i]])  # Slow, but short code.
for(i in 1:nrow(D2)) D2$inducer_feature[i] = as.character(D2[i, D2$inducer[i]])  # Slow, but short code.


# Add congruency scores and labels
D3 = D2 %>%
  # Add congruent_exp and new CMC_code
  left_join(
    y = select(C_both, stim1, stim2, congruent_exp, CMC_code),
    by = c('task_feature'='stim1', 'inducer_feature'='stim2')
  ) %>%
  
  # Add CMC_type
  left_join(
    y = select(C_both %>% group_by(feature1, feature2) %>% slice(1), feature1, feature2, CMC_type),  # Just one row per feature1-feature2 combination
    by = c('task' = 'feature1', 'inducer' = 'feature2')
  )



###################################################
# COMPUTE CONGRUENCY BY MERGING RATINGS INTO TEST #
###################################################

# For each participant
for(id_this in unique(D3$id)) {
  # Get the ratings from R for this participant
  ratings = filter(R, id==id_this)
  
  # Loop through each rating and update D3 with congruent/incongruent for this
  for(rating in 1:nrow(ratings)) {
    # Handy variables
    rating = ratings[rating, ]  # one row
    task_inducer = c(rating$task, rating$inducer)
    rows = D3$id == id_this & 
      D3$task %in% task_inducer & 
      D3$inducer %in% task_inducer
    
    # Add rating for feature pairs congruent with rating$right
    same_order = c(rating$right, paste(rev(strsplit(rating$right, ':')[[1]]), collapse=':'))
    rows_same_order = rows & D3$CMC_code %in% same_order
    D3$congruency[rows_same_order] = rating$key
    
    # Add inverse rating for feature pairs incongruent (the rest)
    rows_reversed_order = rows & is.na(D3$congruency)
    D3$congruency[rows_reversed_order] = -rating$key
    
    # Congruent based on rating?
    D3$congruent_rating = as.integer(D3$congruency > 0)
    D3$congruent_rating[D3$congruency == 0] = NA
  }
}



##################
# ALL DONE, SAVE #
##################
write.table(D3, 'CMC_rt_all.csv', sep=',', row.names = FALSE)
write.table(R_all, 'CMC_rating_all.csv', sep=',', row.names = FALSE)

