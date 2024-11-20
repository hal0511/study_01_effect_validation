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
