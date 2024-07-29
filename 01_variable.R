#===============================================================================
# 29/07/2024
# Variable construction
# Diverse Pathways to Permanent Childlessness in Singapore
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================

######################################################################
# Loading packages
######################################################################
library(tidyverse)
library(haven)
library(janitor)
library(lmtest)
library(sandwich)
library(stargazer)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(margins)
library(Hmisc)
library(ggeffects)
library(poLCA)
library(patchwork)

source("functions.R", local = knitr::knit_global())

######################################################################
# Loading data
######################################################################
load("to-be-filled")



# Construct LCA Indicators ------------------------------------------------

df_LCA <- data %>% 
  # Select those who had never given birth to any children
  filter(n_childless == 1, n_deceased == 0) %>% 
  # Select relevant variables
  select(PID, Weights, 
         # Demographic information for multinomial regressions
         n_female, n_age, n_byear, n_race, n_born, n_nsb, n_econwbch, n_chhealth, 
         # Partnership-related LCA indicators
         n_age_fmarry_tile, n_marriage_end, n_infertility,
         # Education indicator
         n_edu,
         # Occupation-related indicators
         n_ocp20s, n_ocp30s, n_flexible20s, n_flexible30s, n_familyleave20s, n_familyleave30s) %>% 
  # Marital timing
  mutate(n_age_fmarry_tile = case_when(n_age_fmarry_tile == "no" ~ 1,
                                       n_age_fmarry_tile == "early" ~ 2,
                                       n_age_fmarry_tile == "norm" ~ 2,
                                       n_age_fmarry_tile == "late" ~ 3),
         n_age_fmarry_tile = factor(n_age_fmarry_tile)) %>% 
  # Infertility
  mutate(n_infertility = ifelse(is.na(n_infertility), 0, n_infertility),
         n_infertility = factor(n_infertility + 1)) %>% 
  # Education
  mutate(n_edu = case_when(n_edu <= 2 ~ 1,
                           n_edu == 3 ~ 2,
                           n_edu >= 4 ~ 3),
         n_edu = factor(n_edu)) %>%  
  # Occupation in 20s
  mutate(n_ocp20s = case_when(n_ocp20s == "Unemployed" ~ 1,
                              n_ocp20s == "Blue-collared" ~ 2,
                              n_ocp20s == "Semi-professional" ~ 3,
                              n_ocp20s == "Professional" ~ 4)) %>% 
  # Occupation in 30s
  mutate(n_ocp30s = case_when(n_ocp30s == "Unemployed" ~ 1,
                              n_ocp30s == "Blue-collared" ~ 2,
                              n_ocp30s == "Semi-professional" ~ 3,
                              n_ocp30s == "Professional" ~ 4)) %>% 
  mutate(n_ocp20s = factor(n_ocp20s),
         n_ocp30s = factor(n_ocp30s)) %>% 
  # Flexibilty in 20s and 30s, combined
  mutate(n_flexible20s = ifelse(n_ocp20s == 1, 3, n_flexible20s)) %>% 
  mutate(n_flexible30s = ifelse(n_ocp30s == 1, 3, n_flexible30s)) %>% 
  mutate(n_flexible20s = ifelse(n_flexible20s >= 2, 2, n_flexible20s),
         n_flexible30s = ifelse(n_flexible30s >= 2, 2, n_flexible30s)) %>% 
  mutate(n_flexible = case_when(n_flexible20s == 1 & n_flexible30s == 1 ~ 1,
                                n_flexible20s == 2 | n_flexible30s == 2 ~ 2),
         n_flexible = factor(n_flexible)) %>% 
  # Access to family leave benefits in 20s and 30s, combined
  mutate(n_familyleave20s = n_familyleave20s + 1,
         n_familyleave20s = ifelse(n_ocp20s == 1, 2, n_familyleave20s)) %>% 
  mutate(n_familyleave30s = n_familyleave30s + 1,
         n_familyleave30s = ifelse(n_ocp30s == 1, 2, n_familyleave30s)) %>% 
  mutate(n_familyleave = case_when(n_familyleave20s == 1 & n_familyleave30s == 1 ~ 1,
                                   n_familyleave20s == 2 | n_familyleave30s == 2 ~ 2),
         n_familyleave = factor(n_familyleave)) %>% 
  # Demographic information
  mutate(n_female = factor(n_female),
         n_race = factor(n_race),
         n_born = factor(n_born)) %>% 
  mutate(n_race = ifelse(n_race == 1, "Chinese", "Non-Chinese")) %>% 
  mutate(n_age = 2022 - n_byear)

# Drop respondents with missing variables for LCA indicators
df_LCA <- df_LCA %>% 
  dplyr::select(PID, n_age_fmarry_tile, n_marriage_end, n_infertility, n_edu, n_ocp20s, n_ocp30s, n_flexible, n_familyleave) %>% 
  na.omit() %>% 
  left_join(df_LCA %>% 
              dplyr::select(PID, Weights, n_female, n_age, n_byear, n_race, n_born, n_econwbch, n_chhealth),
            by = "PID")
