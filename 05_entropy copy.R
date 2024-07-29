#===============================================================================
# 29/07/2024
# Entropy Across Cohorts
# Diverse Pathways to Permanent Childlessness in Singapore
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================


# Entropy Across Cohorts --------------------------------------------------

entropy_cohort <- df_LCA %>% 
  select(PID, n_byear, predclass) %>% 
  mutate(predclass = as.numeric(as.character(predclass))) %>% 
  # Frequency of each class by cohort
  group_by(n_byear, predclass) %>% 
  summarise(count = n()) %>% 
  mutate(freq = count / sum(count)) %>% 
  ungroup() %>% 
  # Number of unique class by cohort
  left_join(df_LCA %>% 
              select(PID, n_byear, predclass) %>% 
              mutate(predclass = as.numeric(as.character(predclass))) %>% 
              group_by(n_byear) %>% 
              summarise(n_class = n_distinct(predclass)) %>% 
              ungroup(),
            by = "n_byear") %>% 
  # Number of individual by cohort
  left_join(df_LCA %>% 
              select(PID, n_byear) %>% 
              group_by(n_byear) %>% 
              summarise(n = n()) %>% 
              ungroup(),
            by = "n_byear") %>% 
  group_by(n_byear) %>% 
  # Calculate Entropy
  mutate(entropy = -sum(freq * log(freq) / log(n_class), na.rm = TRUE)) %>% 
  select(n_byear, n, entropy) %>% 
  filter(n_byear >= 1950) %>% 
  distinct()

# Make graph
entropy_cohort %>% 
  ggplot(aes(x = n_byear, y = entropy, group = 1)) + 
  geom_point(size = 8) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "darkgrey", linewidth = 8) + 
  labs(title = "",
       x = "",
       y = "Entropy")