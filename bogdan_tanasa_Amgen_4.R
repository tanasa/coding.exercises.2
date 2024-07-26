# setwd("home/bogdan/Desktop/Amgen_code")
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(car)
library(lmtest)
library(nortest)
library(multcomp)
library(emmeans)
# library(tidyverse)



x = read.delim("input.txt", header = T, sep="\t", stringsAsFactors = FALSE)
colnames(x)



# DATA EXPLORATION, in concordance with the longitudinal treatment evaluation

print("the subjects enrolled in the study :")
table(x$subject)
print("the number of subjects is :")
length(unique(x$subject))
print("the timepoints :")
table(x$timepoint)
print("the markers :")
table(x$marker)
print("the treatment groups :")
table(x$treatment_group)

# coding subject, timepoint, marker, treatment_group as FACTORS
x$subject <- factor(x$subject)
x$timepoint <- factor(x$timepoint)
x$marker <- factor(x$marker)
x$treatment_group <- factor(x$treatment_group)

# ensuring that 'timepoint' is a factor with the desired order
x$timepoint <- factor(x$timepoint, levels = c("DAY1", "DAY8", "DAY15", "DAY22", "DAY29"))

# reformatting the data by using reshape2 package :

x_wide <- reshape(
  x,
  idvar = c("subject", "marker", "treatment_group"),  # Variables to keep constant
  timevar = "timepoint",                              # Variable that will become columns
  direction = "wide",                                 # Convert from long to wide format
  v.names = "analyte_value"                           # Values to spread across the new columns
)

# Print the transformed data
colnames(x_wide)
head(x_wide, 2)
tail(x_wide, 2)



# 4. Please test for each marker whether Day 8 and Day 1 readouts are different at alpha = 0.05 under each treatment



# we select the relevant columns :
#                        subject, 
#                        marker,
#                        treatment_group,
#                        analyte_value.DAY1,
#                        analyte_value.DAY8


a <- x_wide %>% dplyr::select(subject, 
                        marker,
                        treatment_group,
                        analyte_value.DAY1,
                        analyte_value.DAY8)

print("the markers that are present :")
table(a$marker)

head(a, 2)
tail(a, 2)



# In order to determine the type of statistical test to use (T-test, Welch T-test, Wilcoxon signed rank test) 
# we check :
# NORMAL DISTRIBUTION of ANALYTE_VALUES
# HOMOSCEDASTICITY or HETEROSCEDASTICITY of ANALYTE_VALUES



# NORMAL DISTRIBUTION of ANALYTE_VALUES



# which test to use to assess normal distribution of the data ?

# Shapiro-Wilk : it is widely used and generally recommended for smaller sample sizes.

# Anderson-Darling Test : it is more robust in detecting deviations from normality in larger samples.

# !!! In our case, we can not apply AD test, because the sample size must be greater than 7.

a_ta <- a %>% filter(treatment_group == "TA")
# head(a_ta, 2)
# tail(a_ta, 2)

a_tc <- a %>% filter(treatment_group == "TC")
# head(a_tc, 2)
# tail(a_tc, 2)

a_tb <- a %>% filter(treatment_group == "TB")
print(a_tb)

sw_normality_results_a_ta <- a_ta %>%
  group_by(marker) %>%
  summarize(
    shapiro_day1_p_value = shapiro.test(analyte_value.DAY1)$p.value,
    shapiro_day8_p_value = shapiro.test(analyte_value.DAY8)$p.value,
    day1_normal = ifelse(shapiro_day1_p_value >= 0.05, "Normal distrib", "Not Normal distrib"),
    day8_normal = ifelse(shapiro_day8_p_value >= 0.05, "Normal distrib", "Not Normal distrib"),
    .groups = 'drop'
  )

# View the results
print(sw_normality_results_a_ta)

sw_normality_results_a_tb <- a_tb %>%
  group_by(marker) %>%
  summarize(
    shapiro_day1_p_value = shapiro.test(analyte_value.DAY1)$p.value,
    shapiro_day8_p_value = shapiro.test(analyte_value.DAY8)$p.value,
    day1_normal = ifelse(shapiro_day1_p_value >= 0.05, "Normal distrib", "Not Normal distrib"),
    day8_normal = ifelse(shapiro_day8_p_value >= 0.05, "Normal distrib", "Not Normal distrib"),
    .groups = 'drop'
  )

# View the results
print(sw_normality_results_a_tb)

sw_normality_results_a_tc <- a_tc %>%
  group_by(marker) %>%
  summarize(
    shapiro_day1_p_value = shapiro.test(analyte_value.DAY1)$p.value,
    shapiro_day8_p_value = shapiro.test(analyte_value.DAY8)$p.value,
    day1_normal = ifelse(shapiro_day1_p_value >= 0.05, "Normal distrib", "Not Normal distrib"),
    day8_normal = ifelse(shapiro_day8_p_value >= 0.05, "Normal distrib", "Not Normal distrib"),
    .groups = 'drop'
  )

# View the results
print(sw_normality_results_a_tc)



# CONCLUSIONS :
# with a few exceptions, these tests inform us that the analyte values data at Day 1 and Day 8 is NORMALLY DISTRIBUTED



# HOMOSCEDASTICITY or HETEROSCEDASTICITY of ANALYTE_VALUES



# Next, we check the equality between variances for each marker analyte_values at Day 1 and Day 8. 

# There are many statistical tests that we can use : 
# F-test (var.test)
# Levene test
# Bartlett's Test
# Fligner-Killeen Test

# I have noted that the results that we obtain with each of these tests are relatively similar.



# Treatment TA
# The p-value is greater than 0.05, it suggests no significant difference in variances for the group associated with each marker.

df = a_ta

df_long <- df %>%
  pivot_longer(cols = starts_with("analyte_value"), names_to = "day", values_to = "value") %>%
  mutate(day = ifelse(day == "analyte_value.DAY1", "Day1", "Day8"))

# Perform F-test for equal variances for each marker and check statistical significance
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = var.test(value[day == "Day1"], value[day == "Day8"])$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("F-test (var.test)")
equal_variance_results

# Levene's test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = leveneTest(value ~ day)$"Pr(>F)"[1],
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Levene's test")
equal_variance_results

# Bartlett's test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = bartlett.test(value ~ day)$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Bartlett's test")
equal_variance_results

# Fligner-Killeen test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = fligner.test(value ~ day)$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Fligner-Killeen")
equal_variance_results


# Treatment TB
# The p-value is greater than 0.05, it suggests no significant difference in variances for the group associated with each marker.

df = a_tb

df_long <- df %>%
  pivot_longer(cols = starts_with("analyte_value"), names_to = "day", values_to = "value") %>%
  mutate(day = ifelse(day == "analyte_value.DAY1", "Day1", "Day8"))

# Perform F-test for equal variances for each marker and check statistical significance
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = var.test(value[day == "Day1"], value[day == "Day8"])$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("F-test (var.test)")
equal_variance_results

# Levene's test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = leveneTest(value ~ day)$"Pr(>F)"[1],
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Levene's test")
equal_variance_results

# Bartlett's test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = bartlett.test(value ~ day)$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Bartlett's test")
equal_variance_results

# Fligner-Killeen test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = fligner.test(value ~ day)$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Fligner-Killeen")
equal_variance_results


# Treatment TC
# The p-value is greater than 0.05, it suggests no significant difference in variances for the group associated with each marker.

df = a_tc

df_long <- df %>%
  pivot_longer(cols = starts_with("analyte_value"), names_to = "day", values_to = "value") %>%
  mutate(day = ifelse(day == "analyte_value.DAY1", "Day1", "Day8"))

# Perform F-test for equal variances for each marker and check statistical significance
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = var.test(value[day == "Day1"], value[day == "Day8"])$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("F-test (var.test)")
equal_variance_results

# Levene's test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = leveneTest(value ~ day)$"Pr(>F)"[1],
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Levene's test")
equal_variance_results

# Bartlett's test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = bartlett.test(value ~ day)$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Bartlett's test")
equal_variance_results

# Fligner-Killeen test for equal variances for each marker
equal_variance_results <- df_long %>%
  group_by(marker) %>%
  summarize(
    p_value = fligner.test(value ~ day)$p.value,
    significance = ifelse(p_value <= 0.05, "Significant", "Not Significant")
  )

# Print the results
print("Equality in the variances between Markers between Day 1 and Day 8, under treatment TC")
print("Fligner-Killeen")
equal_variance_results




# CONCLUSIONS :
# with a few exceptions, these tests tell us that the VARIANCES of the distributions 
# of the analyte values are equal between Day 1 and Day 8



# Given the gaussian distribution and the homoscedasticity of the analyte values for each marker under the treatment TA, TB, TC, 
# we could use a standard t.test var.equal = TRUE. Due to several exceptions, we will use also Welch T-test. 

# treatment TA

df = a_ta

# use T.test with var.equal = TRUE

results <- df %>%
  group_by(marker) %>%
  summarise(
    t_test = list(t.test(analyte_value.DAY8, analyte_value.DAY1, var.equal=TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    p_value = sapply(t_test, function(x) x$p.value),
    significance_label = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
# Print the results
print("Differences in the Treatment TA :")
print("use T.test that assumes equal variances :")
print(results)
      
# use Welch T.test
      
results <- df %>%
  group_by(marker) %>%
  summarise(
    t_test = list(t.test(analyte_value.DAY8, analyte_value.DAY1)),
    .groups = 'drop'
  ) %>%
  mutate(
    p_value = sapply(t_test, function(x) x$p.value),
    significance_label = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
# Print the results
print("Differences in the Treatment TA :")
print("use Welsch T.test:")
print(results)

# treatment TB

df = a_tb 

# use T.test with var.equal = TRUE

results <- df %>%
  group_by(marker) %>%
  summarise(
    t_test = list(t.test(analyte_value.DAY8, analyte_value.DAY1, var.equal=TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    p_value = sapply(t_test, function(x) x$p.value),
    significance_label = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
# Print the results
print("Differences in the Treatment TB :")
print("use T.test that assumes equal variances :")
print(results)
      
# use Welch T.test
      
results <- df %>%
  group_by(marker) %>%
  summarise(
    t_test = list(t.test(analyte_value.DAY8, analyte_value.DAY1)),
    .groups = 'drop'
  ) %>%
  mutate(
    p_value = sapply(t_test, function(x) x$p.value),
    significance_label = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
# Print the results
print("Differences in the Treatment TB :")
print("use Welsch T.test:")
print(results)

# treatment TC

df = a_tc

# use T.test with var.equal = TRUE

results <- df %>%
  group_by(marker) %>%
  summarise(
    t_test = list(t.test(analyte_value.DAY8, analyte_value.DAY1, var.equal=TRUE)),
    .groups = 'drop'
  ) %>%
  mutate(
    p_value = sapply(t_test, function(x) x$p.value),
    significance_label = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
# Print the results
print("Differences in the Treatment TC :")
print("use T.test that assumes equal variances :")
print(results)
      
# use Welch T.test
      
results <- df %>%
  group_by(marker) %>%
  summarise(
    t_test = list(t.test(analyte_value.DAY8, analyte_value.DAY1)),
    .groups = 'drop'
  ) %>%
  mutate(
    p_value = sapply(t_test, function(x) x$p.value),
    significance_label = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )
# Print the results
print("Differences in the Treatment TC :")
print("use Welsch T.test:")
print(results)



# CONCLUSIONS : for each marker at Day 8 and Day 1 under each treatment, 
# we do not detect statistical significance difference of the analyte values between Day 1 and Day 8.


