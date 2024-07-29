#===============================================================================
# 29/07/2024
# Multinomial Regressions
# Diverse Pathways to Permanent Childlessness in Singapore
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================


# Multinomial Regressions -------------------------------------------------

# Models
mods <- list(
  mod1 <- multinom(predclass ~ n_female + n_age + I(n_age^2) + n_race + n_born + n_nsb + n_fedu + n_chfinance + n_chhealth + n_chrlshpprt, 
                   data = childless_df,
                   weights = Weights),
  mod2 <- multinom(predclass ~ n_female*n_age + n_female*I(n_age^2) + n_race + n_born + n_nsb + n_fedu + n_chfinance + n_chhealth + n_chrlshpprt, 
                   data = childless_df,
                   weights = Weights),
  mod3 <- multinom(predclass ~ n_female*n_race + n_age + I(n_age^2) + n_born + n_nsb + n_fedu + n_chfinance + n_chhealth + n_chrlshpprt,
                   data = childless_df,
                   weights = Weights),
  mod4 <- multinom(predclass ~ n_female*n_born + n_age + I(n_age^2) + n_race + n_nsb + n_fedu + n_chfinance + n_chhealth + n_chrlshpprt,
                   data = childless_df,
                   weights = Weights),
  mod5 <-  multinom(predclass ~ n_female*n_nsb + n_age + I(n_age^2) + n_race + n_born + n_fedu + n_chfinance + n_chhealth + n_chrlshpprt,
                    data = childless_df,
                    weights = Weights),
  mod6 <- multinom(predclass ~ n_female*n_fedu + n_age + I(n_age^2) + n_race + n_born + n_nsb + n_chfinance + n_chhealth + n_chrlshpprt,
                   data = childless_df,
                   weights = Weights),
  mod7 <- multinom(predclass ~ n_female*n_chfinance + n_age + I(n_age^2) + n_race + n_born + n_nsb + n_fedu + n_chhealth + n_chrlshpprt,
                   data = childless_df,
                   weights = Weights),
  mod8 <- multinom(predclass ~ n_female*n_chhealth + n_age + I(n_age^2) + n_race + n_born + n_nsb + n_fedu + n_chfinance + n_chrlshpprt, 
                   data = childless_df,
                   weights = Weights),
  mod9 <- multinom(predclass ~ n_female*n_chrlshpprt + n_age + I(n_age^2) + n_race + n_born + n_nsb + n_fedu + n_chfinance + n_chhealth, 
                   data = childless_df,
                   weights = Weights))