library(tidyverse)


#############
# LOAD DATA #
#############
# ... and add factors suitable for inference
D = read.csv('CMC_rt_all.csv') %>%
  mutate(
    fixation_duration = factor(fixation_duration),
    CMC_name = relevel(CMC_name, 'pitch:shape')  # (post-hoc: Best represents the other RTs. Better for MCMC mixing)
  )


################
# DEMOGRAPHICS #
################
D %>%
  group_by(id) %>% 
  slice(1) %>%  # Just one row
  select(id, age, gender) %>%
  summary()



####################
# LOOK AT THE DATA #
####################

# CMC crude estimate (not weighted according to N trials)
q_exp = aggregate(rt ~ congruent_exp + task + inducer, D, FUN=mean)  # Experimenter-defined 
q = aggregate(rt ~ congruent_rating + task + inducer, D, FUN=mean)  # participant-defined congruency
q$ms_diff = round(c(rbind(rep(NA, nrow(q) / 2), diff(q$rt)[seq(1, nrow(q), 2)])), 3)*1000
q$ms_diff_exp = round(c(rbind(rep(NA, nrow(q_exp) / 2), diff(q_exp$rt)[seq(1, nrow(q_exp), 2)])), 3)*1000
filter(q, congruent_rating == 1) %>% select(-rt, -congruent_rating)

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

R_all = read.csv('CMC_rating_all.csv') %>%
  # Good order so that RT-tested CMCs appear first (or last?)
  mutate(
    right = fct_rev(fct_relevel(right, 'smooth:sine', 'smooth:low', 'dark:sine', 'dark:low')),
    left = fct_rev(fct_relevel(left, 'smooth:square', 'smooth:high', 'dark:square', 'dark:high'))
  )

# PREREGISTERED PLOT
# Plot consistency. Summarising for each individual
ggplot(R_all, aes(x=right, y=key, group=id)) + 
  stat_summary(fun.y=mean, geom='point', position=position_jitter(width = 0.1, height = 0)) +  # Points
  geom_abline(intercept=0, slope=0, lty=2) +  # no preference line
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90))


# NUMBERS FOR PUBLICATION (and for the updated plot)
R_perc = R_all %>%
  # Mean per participant
  group_by(id, right, left, task, inducer) %>%
  summarise(
    mean_key = mean(key)
  ) %>%
  
  # Percentage of participants (means) to each side or neutral (zero).
  group_by(right, left, task, inducer) %>%
  summarise(
    # Reverse?
    reverse = ifelse(mean(mean_key) < 0, TRUE, FALSE),
    sum_key = ifelse(reverse, -1, 1) * sum(mean_key),
    
    
    # Direction preference
    perc_left = round(100 * ifelse(reverse, sum(mean_key > 0), sum(mean_key < 0)) / n()),
    perc_right = round(100 * ifelse(reverse, sum(mean_key < 0), sum(mean_key > 0)) / n()),
    perc_max = max(perc_right, perc_left),
    
    # Strength of preference (% participants more extreme than x)
    perc_0 = round(100 * sum(mean_key == 0) / n()),
    perc_1 = round(100 * sum(mean_key <= -1 | mean_key >= 1) / n()),
    perc_2 = round(100 * sum(mean_key <= -2 | mean_key >= 2) / n()),
    
    # Let through
    mean_key = ifelse(reverse, -mean(mean_key), mean(mean_key))  # For arrange
  ) %>%
  
  # Include percentages in labels
  ungroup() %>%
  mutate_at(c('left', 'right'), as.character) %>%
  mutate(
    right_label = sprintf('(%i%%) %s', perc_right, ifelse(reverse, left, right)),
    #left_label = sprintf('%s --- %s (%i%%)', str_to_upper(paste(task, inducer, sep=':')), ifelse(reverse, right, left), perc_left)
    left_label = str_to_upper(paste(task, inducer, sep=':'))
  ) %>%
  arrange(mean_key)
  #arrange(match(right, c('smooth:square', 'smooth:high', 'dark:square', 'dark:high')), mean_key)

# How often was the "no preference" option used in raw data?
sum(R_all$key == 0) / nrow(R_all)

# Degree of consensus on RT-tested and non-RT-tested CMCs
RT_rows = c('12', '13', '14', '15')
R_perc %>%
  group_by(rownames(.) %in% RT_rows) %>%  # One row for tested and one for non-tested
  summarise(
    direction = mean(perc_max),
    plus1 = mean(perc_1),  # % participants more extreme than 1
    plus2 = mean(perc_2)  # % participants more extreme than 2
  )

# UPDATED PLOT
# Order the factor levels of R_all$right acording to mean key
R_plot = R_all %>%
  left_join(select(R_perc, task, inducer, reverse), by=c('task', 'inducer')) %>%
  mutate(
    key = ifelse(reverse, -key, key),
    right = reorder(right, key, mean)  # 
  )


ggplot(R_plot, aes(x=as.numeric(right), y=key, group=id)) + 
  # Add mean rating
  stat_summary(fun.y = mean, geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y.., group=1),
               width = .75, lwd=3, color='gray') + 
  
  # Points and center line
  stat_summary(fun.y=mean, geom='point', size=1, alpha=0.5, position=position_jitter(width = 0.15, height = 0.1)) +  # Points
  geom_abline(intercept=0, slope=0, lty=2) +  # no preference line
  
  # Theming
  theme_bw() + 
  theme(panel.grid.minor.y = element_blank()) + 
  
  # Some axis work
  coord_flip() + 
  scale_y_continuous(breaks=-3:3) + 
  
  # Hack to make double-axis
  # Thanks to https://stackoverflow.com/questions/45361904/duplicating-and-modifying-discrete-axis-in-ggplot2
  scale_x_continuous(
    breaks = 1:nrow(R_perc), 
    labels = R_perc$left_label,
    sec.axis=sec_axis(
      trans = ~.,
      breaks = 1:nrow(R_perc), labels=R_perc$right_label)
  ) +
  
  labs(title='Dimension Matching Task preferences', x=NULL, y='Average preference rating')

ggsave('figures/CMMT Preferences.png', width=4, height=4, scale=1.5)



##########################################
# DESCRIPTIVE RT TABLE FOR SUPPLEMENTARY #
##########################################

# Summary data for descriptives table
T_summary = D %>%
  #filter(!is.na(congruent_rating)) %>%  # Switch on if analyzing using congruent_rating
  mutate(
    rt = 1000*rt,
    congruent_exp = ifelse(congruent_rating, 'congruent', 'incongruent')
  ) %>%
  filter(!is.na(congruent_exp)) %>%
  group_by(CMC_name, congruent_exp) %>%
  summarise(
    mean_sd = paste(round(mean(rt)), ' (', round(sd(rt)), ')', sep=''),
    median_iqr = paste(round(median(rt)), ' (', round(quantile(rt, 0.25)), ' to ', round(quantile(rt, 0.75)), ')', sep='')
  )

# Colums for means
T_mean = T_summary %>%
  select(CMC_name, congruent_exp, mean_sd) %>%
  spread(congruent_exp, mean_sd)

# Additional columns for medians
T_median = T_summary %>%
  select(CMC_name, congruent_exp, median_iqr) %>%
  spread(congruent_exp, median_iqr)

# All together now
T_all = bind_cols(T_mean, T_median) %>%
  select(-CMC_name1)
write.csv(T_all, 'tables/table descriptives rt rating.csv', row.names = FALSE)


