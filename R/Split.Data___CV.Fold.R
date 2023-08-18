Split.Data___CV.Fold = function(X, y, splie.var, n_fold=10, return.index=T){






}


Split.Data___Grouping.By___Continuous


# Assuming your data is in a dataframe called 'data'
# and your response variable is 'response' and age variable is 'age'
data = X
# Create age groups
data %>% names

X$Age %>% unique %>% sort
c(18,25,35,45,55,65,75),
data <- data %>%
  mutate(age_group = case_when(
    Age <= 20 ~ "0-18",
    Age <= 40 ~ "18-30",
    Age <= 60 ~ "30-40",
    Age <= 80 ~ "50-60",
    TRUE ~ "60+"
  ))

# Create a combined stratification variable
data <- data %>%
  unite("strata", response, age_group, sep = "_")

# Perform stratified sampling
sample_data <- data %>%
  group_by(strata) %>%
  sample_frac(0.2)

# This 'sample_data' dataframe now contains 20% of the original data
# with the proportions of the response variable and age groups maintained.





















