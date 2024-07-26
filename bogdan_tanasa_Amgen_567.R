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

head(x,2)
tail(x,2)



# 5. Please fit an approriate statistical model for each marker with analyte_value as dependent variables, 
# and timepoint and treatment as independent variables (both are conisidered factors). 
# Assume subjects are randomly drawn from a population. 
# With each model, please report the significance of treatment effect and the contrast between Day 22 and Day 8.



# Given the statistical analysis shown in the previous part (part4), 
# focused on the gaussian distribution and  homescedasticity / heteroscedasticity of the data,
# I believe that we could use simple linear models. 
# Given a few exceptions in the gaussian distribution and homoscedasticity of the analyte values, 
# that we have observed, we may also use GLM.



# In our models, we do NOT consider INTERACTIONS between TIMEPOINT and TREATMENT.



# Separating the data frame per marker

x$timepoint = factor(x$timepoint, levels = c("DAY1", "DAY8", "DAY15", "DAY22", "DAY29"))
x$treatment_group = factor(x$treatment_group)

# Set reference levels
x$treatment_group <- relevel(x$treatment_group, ref = "TA")
x$timepoint <- relevel(x$timepoint, ref = "DAY1")

x_C4 = split(x, x$marker)$C4
x_C8 = split(x, x$marker)$C8
x_TG = split(x, x$marker)$TG



df = x_C4

# Fit Generalized Linear Model
# glm_model <- glm(analyte_value ~ timepoint + treatment_group, 
#                  family = gaussian(), # the model assumes normally distributed residuals.
#                  data = df)
# Get the model summary
# glm_summary <- summary(glm_model)
# print(glm_summary)

# Fit a Linear Model
lm_model <- lm(analyte_value ~ timepoint + treatment_group, data = df)

# Get the model summary
lm_summary <- summary(lm_model)

# Print the summary to examine significance of effects
print(lm_summary)

# Extracting the contrast of interest : 
df$timepoint <- factor(df$timepoint, levels = c("DAY1", "DAY8", "DAY15", "DAY22", "DAY29"))

# Compute estimated marginal means
emm <- emmeans(lm_model, ~ timepoint)

# Specify the contrast between Day 22 and Day 8
contrast_results <- contrast(emm, list("Day22 vs Day8" = c(0, -1, 0, 1, 0)))

# Print the results
print(contrast_results)

# INTERPRETATION :

# REFERENCE LEVELS : 
# When all predictors are at their reference levels (treatment "TA", timepoint "DAY1"), 
# the estimated average analyte_value is statistically significant, indicating a strong baseline effect.

# For the timepoint "DAY8" compared to the reference timepoint, the analyte_value is marginally significant 
# (p-value just above 0.05), suggesting a trend toward significance.

# Additionally, Multiple R-squared and Adjusted R-squared are very low, indicating that the model does not explain 
# much more variance than would be expected by chance. 
# There is a relatively weak relationship between the predictors and the outcome.

# Only the intercept is statistically significant.
# The F-statistic is close to 0.05, indicating that the model is marginally significant as a whole 
# but doesn’t strongly explain the variability in the response variable.

# Contrast DAY22 - DAY8 : the difference is not statistically significant.

df = x_C8

# Fit Generalized Linear Model
# glm_model <- glm(analyte_value ~ timepoint + treatment_group, 
#                  family = gaussian(), # the model assumes normally distributed residuals.
#                  data = df)
# Get the model summary
# glm_summary <- summary(glm_model)
# print(glm_summary)

# Fit a Linear Model
lm_model <- lm(analyte_value ~ timepoint + treatment_group, data = df)

# Get the model summary
lm_summary <- summary(lm_model)

# Print the summary to examine significance of effects
print(lm_summary)

# Extracting the contrast of interest : 
df$timepoint <- factor(df$timepoint, levels = c("DAY1", "DAY8", "DAY15", "DAY22", "DAY29"))

# Compute estimated marginal means
emm <- emmeans(lm_model, ~ timepoint)

# Specify the contrast between Day 22 and Day 8
contrast_results <- contrast(emm, list("Day22 vs Day8" = c(0, -1, 0, 1, 0)))

# Print the results
print(contrast_results)

# INTERPRETATION : 

# The estimated baseline analyte_value when timepoint and treatment_group are at their reference levels is marginally significant.
# The p-value is slightly above the conventional 0.05 threshold, 
# suggesting that the baseline value is not significantly different from zero, but there is a hint of a difference.

# TREATMENT TB : The analyte_value for the treatment group "TB" is estimated to be 1108 units higher compared 
# to the reference treatment group. 
# This effect is statistically significant (p-value < 0.01), suggesting a strong difference in analyte_value for the "TB" treatment group.

# Additionally, Multiple R-squared and Adjusted R-squared are very low, indicating that the model does not explain 
# much more variance than would be expected by chance. 
# There is a relatively weak relationship between the predictors and the outcome.

# Only treatment_groupTB shows a statistically significant effect on analyte_value,
# suggesting a notable impact of this treatment group.

# The F-statistic is significant (p-value < 0.05), indicating that the model as a whole is statistically significant 
# and that at least one of the predictors contributes to explaining the variability in analyte_value.

# Contrast DAY22 - DAY8 : the difference is not statistically significant.

df = x_TG

# Fit Generalized Linear Model
# glm_model <- glm(analyte_value ~ timepoint + treatment_group, 
#                  family = gaussian(), # the model assumes normally distributed residuals.
#                  data = df)
# Get the model summary
# glm_summary <- summary(glm_model)
# print(glm_summary)

# Fit a Linear Model
lm_model <- lm(analyte_value ~ timepoint + treatment_group, data = df)

# Get the model summary
lm_summary <- summary(lm_model)

# Print the summary to examine significance of effects
print(lm_summary)

# Extracting the contrast of interest : 
df$timepoint <- factor(df$timepoint, levels = c("DAY1", "DAY8", "DAY15", "DAY22", "DAY29"))

# Compute estimated marginal means
emm <- emmeans(lm_model, ~ timepoint)

# Specify the contrast between Day 22 and Day 8
contrast_results <- contrast(emm, list("Day22 vs Day8" = c(0, -1, 0, 1, 0)))

# Print the results
print(contrast_results)

# INTERPRETATION :

# DAY 8 : The analyte_value is significantly higher on DAY 8 compared to the reference timepoint, 
# and this effect is highly statistically significant.

# DAY 22: The positive estimate indicates a meaningful increase in analyte_value, 
# and the p-value below 0.05 confirms that this effect is statistically significant.

# Additionally, Multiple R-squared and Adjusted R-squared are very low, indicating that the model does not explain 
# much more variance than would be expected by chance. 
# There is a relatively weak relationship between the predictors and the outcome.

# DAY8 and DAY22 have a significant impact on analyte_value, with DAY8 showing the strongest effect. 
# DAY15 is marginally significant, while DAY29 does not significantly impact analyte_value.

# The p-value of the F-statistics indicates that the overall model is statistically significant. 

# Contrast DAY22 - DAY8 : the difference is not statistically significant.



# 6. If you had to analyze 1 million markers, how would you parallelize the tests from question 4 
# on a multi-processor machine? Please demonstrate this parallelization using just the markers 
# available in the spreadsheet.



# Let's consider the code that we have used before, when considering the treatment TB. 

x_wide <- reshape(
  x,
  idvar = c("subject", "marker", "treatment_group"),  # Variables to keep constant
  timevar = "timepoint",                              # Variable that will become columns
  direction = "wide",                                 # Convert from long to wide format
  v.names = "analyte_value"                           # Values to spread across the new columns
)

a <- x_wide %>% dplyr::select(subject, 
                        marker,
                        treatment_group,
                        analyte_value.DAY1,
                        analyte_value.DAY8)


a_tb <- a %>% filter(treatment_group == "TB")
head(a_tb, 2)
tail(a_tb, 2)

df = a_tb 

# Previously, when answering the question # 4, we have written the following piece of code 
# We used T.test with var.equal = TRUE and Welch T.test.
# We demonstrate the code by using Welch T.test segment of code.

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



# In order to parallelize, we use the library "parallel" :
library(parallel)

# Number of cores to use
num_cores <- detectCores() - 1
print("the number of cores is :") 
print(num_cores)


# Define the function to perform Welch T-test for each marker

compute_t_test <- function(marker_data) {
  
  t_test_result <- t.test(marker_data$analyte_value.DAY8, marker_data$analyte_value.DAY1)
  
  p_value <- t_test_result$p.value
  significance_label <- ifelse(p_value < 0.05, "Significant", "Not Significant")
  
  return(list(p_value = p_value, significance_label = significance_label))
}

# Split data by marker
df = a_tb 
marker_list <- split(df, df$marker)

# Initialize parallel computing, perform parallel computation, and stop the cluster
cl <- makeCluster(num_cores)
results_list <- parLapply(cl, marker_list, compute_t_test)
stopCluster(cl)


print("the results produced by the R cluster :")
results_list

# we place these results into a dataframe 
# results_parallel_processing <- data.frame(
#  marker = c(names(results_list[1]), names(results_list[2]), names(results_list[3])),
#  p_value = c(results_list[[1]]$p_value, results_list[[2]]$p_value, results_list[[3]]$p_value),
#  significance_label = c(results_list[[1]]$significance_label, results_list[[2]]$significance_label, results_list[[3]]$significance_label) 
# )

# print(results_parallel_processing)

# we generalize to any number of markers :

results_parallel_processing.df<- data.frame(
  marker = names(results_list),
  p_value = sapply(results_list, function(x) x$p_value),
  significance_label = sapply(results_list, function(x) x$significance_label)
)

print("Differences in the Treatment TB between DAY 1 and DAY 8:")
print("use Welsch T.test:")
print(results_parallel_processing.df)





# 7. Please automate the table view for different layouts, i.e., write a function that takes the input data 
# and another parameter indicating a categorical (nominal) variable so that the output of this function 
# will produce a new table in which each level of the indicated variable become a separate column filled 
# with corresponding analyte_value and others columns remain. 
# In the data provided for the quiz, except "analyte_value", all other variables are factors :)



# we use the manin data frame x :

head(x,2)
tail(x,2)



table_view <- function(data, categorical_variable) {
  
    # Reshape the data according to the instructions :
    reshaped_data <- data %>%
                   pivot_wider(names_from = categorical_variable, values_from = analyte_value)
  
    return(reshaped_data)

}

# Categorical variable : "marker" ; print the reshaped data

reshaped_df <- table_view(x, "marker")
head(as.data.frame(reshaped_df), 6)

# Categorical variable : "treatment_group" ; print the reshaped data

reshaped_df <- table_view(x, "treatment_group")
head(as.data.frame(reshaped_df), 6)

# Categorical variable : "timepoint" ; print the reshaped data

reshaped_df <- table_view(x, "timepoint")
head(as.data.frame(reshaped_df), 6)


