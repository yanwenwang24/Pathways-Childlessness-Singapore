#===============================================================================
# 29/07/2024
# Descriptive Statistics by Latent Class
# Diverse Pathways to Permanent Childlessness in Singapore
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================


# Description -------------------------------------------------------------

# Join LCA class with the main dataset
childless_df <- data %>% 
  filter(n_childless == 1, n_deceased == 0) %>% 
  select(PID, Weights, n_female, n_age, n_age1, n_byear, n_race, n_born, n_nsb, n_fedu, n_chfinance, n_chhealth, n_chrlshpprt) %>% 
  left_join(df_LCA %>% select(PID, predclass, n_age_fmarry_tile, n_marriage_end, n_infertility, n_edu, n_ocp20s, n_ocp30s, n_flexible, n_familyleave),
            by = "PID") %>% 
  mutate(n_female = case_when(n_female == 0 ~ "Male",
                              n_female == 1 ~ "Female"),
         n_female = factor(n_female, levels = c("Male", "Female"))) %>% 
  mutate(n_fedu = case_when(n_fedu <= 2 ~ "Low",
                            n_fedu == 3 ~ "Middle",
                            n_fedu >= 4 ~ "High"),
         n_fedu = factor(n_fedu, levels = c("Low", "Middle", "High"))) %>% 
  mutate(n_cohort = case_when(n_byear < 1960 ~ "<1960",
                              n_byear >= 1960 & n_byear < 1965 ~ "1960-1964",
                              n_byear >= 1965 & n_byear < 1970 ~ "1965-1969",
                              n_byear >= 1970 ~ ">=1970"),
         n_cohort = factor(n_cohort, levels = c("<1960", "1960-1964", "1965-1969", ">=1970"))) %>% 
  mutate(n_female = factor(n_female),
         n_born = factor(n_born)) %>% 
  mutate(n_race = ifelse(n_race == 1, 1, 0)) %>% 
  select(PID, Weights, predclass, n_female, n_age, n_age1, n_byear, n_cohort, n_race, n_born, n_nsb, n_fedu, n_chfinance, n_chhealth, n_chrlshpprt, n_age_fmarry_tile, n_marriage_end, n_infertility, n_edu, n_ocp20s, n_ocp30s, n_flexible, n_familyleave) %>% 
  na.omit()

# Categorical variables
categorical <- bind_rows(
  ctab(childless_df, n_female, predclass),
  ctab(childless_df, n_cohort, predclass),
  ctab(childless_df, n_race, predclass),
  ctab(childless_df, n_born, predclass),
  ctab(childless_df, n_fedu, predclass)
)

# Continuous variables
continuous <- childless_df %>% 
  select(predclass,
         n_age, n_nsb,
         n_chfinance, n_chhealth, n_chrlshpprt) %>% 
  
  group_by(predclass) %>% 
  summarise_if(is.numeric, list(mean, sd), na.rm = T) %>% 
  pivot_longer(cols = -predclass,
               names_to = c("variable", "fn"),
               names_sep = -4,
               values_to = "value") %>% 
  mutate(fn = case_when(fn == "_fn1" ~ "mean",
                        fn == "_fn2" ~ "sd"))  %>% 
  pivot_wider(id_cols = c(predclass, variable),
              names_from = fn,
              values_from = value) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(m_sd = paste0(mean, " (", sd, ")")) %>% 
  pivot_wider(id_cols = variable,
              names_from = predclass,
              values_from = m_sd)

# Print tables
bind_rows(categorical, continuous) %>% 
  kableExtra::kbl(caption = "Descriptive statistics by cluster") %>% 
  kableExtra::kable_classic(html_font = "Cambria", full_width = F)