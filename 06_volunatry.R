#===============================================================================
# 29/07/2024
# Percentages of Voluntary Childlessness
# Diverse Pathways to Permanent Childlessness in Singapore
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================


# Voluntary Childlessness -------------------------------------------------

cohort_marry_fertile <- df_LCA %>% 
  select(n_age_fmarry_tile, n_infertility, n_byear) %>% 
  # Ever-married and fertile
  mutate(n_marry_fertile = ifelse((n_age_fmarry_tile == 2 | n_age_fmarry_tile == 3) & n_infertility == 1, 1, 0)) %>% 
  group_by(n_byear) %>% 
  summarise(marry_fertile_percent = mean(n_marry_fertile),
            n = n()) %>% 
  filter(n_byear >= 1950)

# Make graph
cohort_marry_fertile %>% 
  ggplot(aes(x = n_byear, y = marry_fertile_percent)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "darkgrey") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "",
       x = "",
       y = "Percentage of Voluntary Childlessness")