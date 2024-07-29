#===============================================================================
# 29/07/2024
# Latent Class Analysis
# Diverse Pathways to Permanent Childlessness in Singapore
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================


# Latent Class Analysis ---------------------------------------------------

# * Identify Latent Classes -----------------------------------------------

set.seed(333)

f1 <- as.formula(cbind(n_age_fmarry_tile, n_marriage_end, n_infertility, n_edu, n_ocp20s, n_ocp30s, n_flexible, n_familyleave) ~ 1)

LCA2 <- poLCA(f1, df_LCA, nclass = 2)
LCA3 <- poLCA(f1, df_LCA, nclass = 3)
LCA4 <- poLCA(f1, df_LCA, nclass = 4)
LCA5 <- poLCA(f1, df_LCA, nclass = 5)
LCA6 <- poLCA(f1, df_LCA, nclass = 6)
LCA7 <- poLCA(f1, df_LCA, nclass = 7)
LCA8 <- poLCA(f1, df_LCA, nclass = 8)
LCA9 <- poLCA(f1, df_LCA, nclass = 9)


# * Fit Measures ----------------------------------------------------------

# Obtain SABIC
n <- nrow(df_LCA)
SABIC2 = log(n * (n + 2) / 24) * LCA2$np - 2 * LCA2$ll
SABIC3 = log(n * (n + 2) / 24) * LCA3$np - 2 * LCA3$ll
SABIC4 = log(n * (n + 2) / 24) * LCA4$np - 2 * LCA4$ll
SABIC5 = log(n * (n + 2) / 24) * LCA5$np - 2 * LCA5$ll
SABIC6 = log(n * (n + 2) / 24) * LCA6$np - 2 * LCA6$ll
SABIC7 = log(n * (n + 2) / 24) * LCA7$np - 2 * LCA7$ll
SABIC8 = log(n * (n + 2) / 24) * LCA8$np - 2 * LCA8$ll
SABIC9 = log(n * (n + 2) / 24) * LCA9$np - 2 * LCA9$ll

# Obtain fit measures and make graph
df_fit <- data.frame(
  class = seq(2, 9),
  BIC = c(LCA2$bic, LCA3$bic, LCA4$bic, LCA5$bic, LCA6$bic, LCA7$bic, LCA8$bic, LCA9$bic),
  SABIC = c(SABIC2, SABIC3, SABIC4, SABIC5, SABIC6, SABIC7, SABIC8, SABIC9),
  AIC = c(LCA2$aic, LCA3$aic, LCA4$aic, LCA5$aic, LCA6$aic, LCA7$aic, LCA8$aic, LCA9$aic),
  EntropyR2 = c(entropy.R2(LCA2), entropy.R2(LCA3), entropy.R2(LCA4), entropy.R2(LCA5), entropy.R2(LCA6), entropy.R2(LCA7), entropy.R2(LCA8), entropy.R2(LCA9)),
  Chisq = c(LCA2$Chisq, LCA3$Chisq, LCA4$Chisq, LCA5$Chisq, LCA6$Chisq, LCA7$Chisq, LCA8$Chisq, LCA9$Chisq)
)

# Create table
df_fit %>% 
  pivot_longer(cols = -class,
               names_to = "measure",
               values_to = "fit") %>% 
  pivot_wider(id_cols = measure,
              names_from = class,
              values_from = fit) %>% 
  mutate_if(is.numeric, round, digits = 2) 

# Create graph
df_fit %>% 
  pivot_longer(cols = -class,
               names_to = "measure",
               values_to = "fit") %>% 
  filter(measure %in% c("BIC", "SABIC")) %>% 
  
  ggplot(aes(x = class, y = fit, color = measure)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2, 9, 1)) + 
  geom_vline(xintercept = 5, linetype = "dashed", color = "grey") + 
  labs(title = "Number of Latent Classes and Fit Measures")


# * Distribution of LCA Indicators ----------------------------------------

# Marital timing
LCA5$probs$n_age_fmarry_tile %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "age_marriage",
               values_to = "prob") %>% 
  pivot_wider(id_cols = age_marriage,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Marital disruption
LCA5$probs$n_marriage_end %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "end_marriage",
               values_to = "prob") %>% 
  pivot_wider(id_cols = end_marriage,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Infertility
LCA5$probs$n_infertility %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "infertility",
               values_to = "prob") %>% 
  pivot_wider(id_cols = infertility,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Education
LCA5$probs$n_edu %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "edu",
               values_to = "prob") %>% 
  pivot_wider(id_cols = edu,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Occupation in 20s
LCA5$probs$n_ocp20s %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "ocp20",
               values_to = "prob") %>% 
  pivot_wider(id_cols = ocp20,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Occupation in 30s
LCA5$probs$n_ocp30s %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "ocp30",
               values_to = "prob") %>% 
  pivot_wider(id_cols = ocp30,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Flexibiltiy in 20s and 30s
LCA5$probs$n_flexible %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "flex",
               values_to = "prob") %>% 
  pivot_wider(id_cols = flex,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`)

# Family leave in 20s and 30s
LCA5$probs$n_familyleave %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(class = rowname) %>% 
  pivot_longer(cols = -class,
               names_to = "familyleave",
               values_to = "prob") %>% 
  pivot_wider(id_cols = familyleave,
              names_from = class,
              values_from = prob) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(`class 1` = `class 4: `,
         `class 2` = `class 5: `,
         `class 3` = `class 3: `,
         `class 4` = `class 1: `,
         `class 5` = `class 2: `) %>% 
  select(1, `class 1`, `class 2`, `class 3`, `class 4`, `class 5`) 

# Add predicted class to dataset
df_LCA$predclass <- LCA5$predclass

df_LCA <- df_LCA %>% 
  mutate(predclass = factor(predclass))

df_LCA <- df_LCA %>% 
  mutate(predclass = case_when(predclass == 4 ~ 1,
                               predclass == 5 ~ 2,
                               predclass == 3 ~ 3,
                               predclass == 1 ~ 4,
                               predclass == 2 ~ 5),
         predclass = factor(predclass))

# Relative percentage of each latent class
df_LCA %>% tabyl(predclass) %>% adorn_pct_formatting(2)