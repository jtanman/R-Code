# By: James Tan

# Date: 6/19/2019

p_load(reshape2, beepr, scales, zoo, tidyverse, RColorBrewer, viridis, dbscan)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/generalized_markov_model_helper.R')

STATES <- c('$0 Churn', '$5 Churn', '$20 Churn', '$100+ Churn', '$0 DAU', '$5 DAU', '$20 DAU', '$100+ DAU')

markovmodel <- readRDS('niso_markov_data_2017-06-28_2019-02-10_markov_model.RData')
transitiondata <- markovmodel$transitiondata
install_transition <- markovmodel$install_transition
transition_matrix <- markovmodel$transition_matrix
arpdaudata <- markovmodel$arpdaudata
transitionMatrices <- markovmodel$transitionMatrices
state_probs <- markovmodel$state_probs
simulation_results <- markovmodel$simulation_results
result <- markovmodel$result
result_summary <- markovmodel$result_summary

summary(transition_matrix)

# transition_matrix <- transition_matrix %>%
#   mutate(
#     state = lvls_revalue(state, STATES),
#     next_state = lvls_revalue(next_state, STATES)
#   )

cluster_data <- pivot_wider(filter(transition_matrix, cohortday != max(cohortday)), id_cols = cohortday, names_from = c(state, next_state), names_prefix='trans_', values_from = trans_prob)
dist_data <- select(cluster_data, -cohortday)
dist_data[is.na(dist_data)] = 0

trans_var <- cluster_data %>%
  select(-cohortday) %>%
  pivot_longer(cols=everything(), names_to = 'transition') %>%
  group_by(transition) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) %>%
  arrange(desc(sd))

# View(filter(trans_var, transition == 'trans_4_0'))

cl <- hdbscan(dist_data, minPts = 4)

cluster_data$cluster <- fct_recode(as_factor(cl$cluster), 'outlier' = '0')
cluster_data <- cluster_data %>%
  group_by(cluster) %>%
  mutate(
    cluster_name = ifelse(cluster == 'outlier', 'outlier', sprintf('%d_%d', min(cohortday), max(cohortday)))
  ) %>%
  ungroup() %>%
  mutate(
    cluster_name = as_factor(cluster_name)
  ) %>%
  select(cohortday, cluster, cluster_name, everything())

ggplot(cluster_data, aes(cohortday, cluster_name, color=cluster_name)) +
  geom_point() +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = 'Cohortday Clusters', caption='Based on transition probabilities') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('markov_cohortday_clusters.png')



ggplot(cluster_data, aes(x=trans_4_4, y=trans_5_5, color=cluster_name)) +
  geom_point() +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = 'Clusters by Retention and RPDAU Retention', caption='Only graphing 2 out of 40 transition probabilities') +
  xlab('$0 DAU to $0 DAU') +
  scale_x_continuous(labels = percent) +
  ylab('$5 DAU to $5 DAU') +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('markov_clusters_by_transition_rates.png')

plotdata <- filter(cluster_data, cohortday >= 3)
ggplot(plotdata, aes(x=trans_4_4, y=trans_5_5, color=cluster_name)) +
  geom_point() +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = 'Clusters by Retention and RPDAU Retention', caption='Only graphing 2 out of 40 transition probabilities') +
  xlab('$0 DAU to $0 DAU') +
  scale_x_continuous(labels = percent) +
  ylab('$5 DAU to $5 DAU') +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('markov_clusters_by_transition_rates_d3+.png')

cluster_summary <- cluster_data %>%
  filter(cluster != 'outlier') %>%
  pivot_longer(cols=contains('trans'), names_to='transition', values_to = 'probability') %>%
  group_by(cluster_name, transition) %>%
  summarise(
    probability = mean(probability, na.rm=TRUE)
  )

plotdata <- cluster_summary %>%
  mutate(
    transition = as_factor(str_remove(transition, 'trans_')),
  )

trans_labels <- levels(plotdata$transition)

plotdata <- plotdata %>%
  mutate(
    transition = as.integer(transition)
  )

ggplot(plotdata, aes(transition, probability, color=cluster_name)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=1:max(plotdata$transition),
                     labels=trans_labels,
                     sec.axis = dup_axis(name=NULL)) +
  labs(title = 'Transition Probabilities by Cluster') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_color_brewer(palette = 'Spectral')

ggsave_default('transition_probabilities_by_cluster.png')


cl$cluster_scores


cl