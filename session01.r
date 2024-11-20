# ---- LIBRARY ----
library(pacman)
pacman::p_load(
  tidyverse,
  readr
)

# ---- 1.4 Rによるメールマーケティングの効果の検証 ----
# load data
email_data <- readr::read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# 1.4.2 Aggregation of RCT data and significance test ----
# preparing data
## remove record that sent to female
male_df <- email_data %>%
  dplyr::filter(segment != 'Womens E-Mail') %>%
  dplyr::mutate(
    treatment = if_else(segment == 'Mens E-Mail', 1, 0)
  )

# overview
summary_by_segment <- male_df %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(
    conversion_rate = mean(conversion),
    spend_mean = mean(spend),
    count_n = n()
  )

# Since these data were obtained in an RCT, selection bias is not considered a problem
# Therefore, we can evaluate the result by performing a significance test on the data.
mens_mail <- male_df %>%
  dplyr::filter(treatment == 1) %>%
  # Use 'pull' instead of 'select' if only one column is needed.
  pull(spend)

no_mail <- male_df %>%
  dplyr::filter(treatment == 0) %>%
  pull(spend)

# Run a significance test on the difference between the means of 'mens-email' and 'no-email'.
rct_ttest <- t.test(mens_mail, no_mail, var.equal = TRUE)
# result: t = 5.3001, p-value = 1.163e-07
# The null hypothesis is rejected because the p-value. Assume there is a significant difference.

# 1.4.3 Verification of Effectiveness with Biased Data ----
## Creating data with selection bias

# setting
set.seed(1)

obs_rate_c <- 0.5
obs_rate_t <- 0.5

# Creating data
# Delete 50% of data the meets the following criteria
# * less than 300 purchases in the last year (history < 300)
# * last purchase was less than 6 (recency < 6)
# * Multiple contact channels (channel = Multichannel)
biased_data <- male_df %>%
  dplyr::mutate(
    obs_rate_c = if_else(
      (history > 300) | (recency < 6) | (channel == 'Multichannel'), obs_rate_c, 1),
    obs_rate_t = if_else(
      (history > 300) | (recency < 6) | (channel == 'Multichannel'), 1, obs_rate_t),
    # uniform distribution
    random_number = runif(n = NROW(male_df))
    ) %>%
  # treatment = 0 では5当てはまる50%が選ばれない, treatment = 1 では当てはまらない50%が選ばれない
  dplyr::filter(
    (treatment == 0 & random_number < obs_rate_c)
    | (treatment == 1 & random_number < obs_rate_t)
  )

summary_by_segment_biased <- biased_data %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(
    conversion_rate = mean(conversion),
    spend_mean = mean(spend),
    count_n = n()
  )

mens_mail_biased <- biased_data %>%
  dplyr::filter(treatment == 1) %>%
  pull(spend)

no_mail_biased <- biased_data %>%
  dplyr::filter(treatment == 0) %>%
  pull(spend)

rct_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = TRUE)
