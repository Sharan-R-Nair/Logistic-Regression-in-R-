###############################################################
# ASSIGNMENT 2 – STATISTICS FOR BUSINESS
# LOGISTIC REGRESSION ANALYSIS
# Dataset: termsv.xlsx
# Target variable: subscribed (Yes / No)

###############################################################


###############################################################
# INSTALLING & LOADING REQUIRED PACKAGES
###############################################################


 #install.packages("tidyverse")
 #install.packages("readxl")
 #install.packages("ggplot2")
 #install.packages("caret")
 #install.packages("broom")
 #install.packages("skimr")
 #install.packages("scales")
 #in 
 #install.packages("modelsummary")
 #install.packages("pROC")

library(tidyverse)
library(readxl)
library(ggplot2)
library(caret)
library(broom)
library(skimr)
library(scales)
library(car)
library(modelsummary)
library(pROC)


###############################################################
# TO CHECK AND ADD THE SETTING DIRECTORY
###############################################################
 
 
getwd()
setwd("F:/Languages/R Language")


###############################################################
# LOAD AND INSPECT DATA
###############################################################

termsv <- read_excel("termsv.xlsx")

cat("Number of rows =", nrow(termsv),
    "| Number of columns =", ncol(termsv), "\n")

glimpse(termsv)
View(termsv)


###############################################################
# DEPENDENT VARIABLE (TARGET)
###############################################################
# subscribed = whether the customer subscribed (Yes / No)

termsv$subscribed <- tolower(as.character(termsv$subscribed))
termsv$subscribed <- ifelse(termsv$subscribed %in% c("yes","y","1","true"),
                            "Yes","No")
termsv$subscribed <- factor(termsv$subscribed, levels = c("No","Yes"))

# Class balance (important for logistic regression)
table(termsv$subscribed)
prop.table(table(termsv$subscribed))


###############################################################
# DATA QUALITY CHECKS 
###############################################################

# NUMBER OF observations were amended

rows_before <- nrow(termsv)
na_rows <- sum(!complete.cases(termsv))

cat("Rows with missing values BEFORE cleaning:", na_rows, "\n")

# Drop ID column (identifier only, no predictive value)
if ("ID" %in% names(termsv)) {
  termsv <- termsv %>% select(-ID)
}

# Remove rows with missing values (clean modelling dataset)
termsv_clean <- termsv %>% drop_na()

rows_after <- nrow(termsv_clean)

cat("Rows BEFORE cleaning:", rows_before, "\n")
cat("Rows AFTER cleaning :", rows_after, "\n")
cat("Rows REMOVED        :", rows_before - rows_after, "\n")


###############################################################
# VARIABLE TYPE CONVERSION
###############################################################

# Convert categorical and numeric variables explicitly

categorical_vars <- c(
  "gender","occupation","salary_level","marital_status",
  "education_level","credit_default",
  "has_mortgage","has_personal_loan",
  "has_car_insurance","has_life_insurance",
  "has_savings_account","has_current_account",
  "has_credit_card","contact_method","month","day",
  "prev_campaign_outcome"
)

numeric_vars <- c(
  "age","contact_duration","number_contacts","last_contacted",
  "prev_contacts","empl_var_rate","cons_price_index",
  "cons_conf_index","euribor_three_mth","nr_employed"
)

categorical_vars <- categorical_vars[categorical_vars %in% names(termsv_clean)]
numeric_vars <- numeric_vars[numeric_vars %in% names(termsv_clean)]

termsv_clean <- termsv_clean %>%
  mutate(across(all_of(categorical_vars), as.factor)) %>%
  mutate(across(all_of(numeric_vars), as.numeric))

glimpse(termsv_clean)

###############################################################
# FIX CATEGORY INCONSISTENCIES
###############################################################

termsv_clean$salary_level <- as.character(termsv_clean$salary_level)

termsv_clean$salary_level <- dplyr::recode(
  termsv_clean$salary_level,
  "med" = "medium"
)

termsv_clean$salary_level <- factor(termsv_clean$salary_level)


###############################################################
# DESCRIPTIVE STATISTICS (FULL TABLE)
###############################################################

desc_stats <- termsv_clean %>% skim()
print(desc_stats)


###############################################################
# UNIVARIATE LOGISTIC REGRESSION (HYPOTHESIS SCREENING)
###############################################################

# Used to identify the strongest predictors (top 5–10)

get_auc <- function(varname, data) {
  f <- as.formula(paste("subscribed ~", varname))
  m <- glm(f, data = data, family = binomial)
  p <- predict(m, type = "response")
  roc_obj <- roc(data$subscribed, p, levels = c("No","Yes"), quiet = TRUE)
  as.numeric(auc(roc_obj))
}

predictors <- setdiff(names(termsv_clean), "subscribed")

auc_table <- tibble(
  predictor = predictors,
  AUC = map_dbl(predictors, ~get_auc(.x, termsv_clean))
) %>% arrange(desc(AUC))

print(auc_table)

# Select top 5 predictors 

top5_vars <- auc_table %>% slice_head(n = 5) %>% pull(predictor)
cat("Top 5 predictors:\n")
print(top5_vars)


# Select top 10 predictors (for comparison model)

top10_vars <- auc_table %>% slice_head(n = 10) %>% pull(predictor)
cat("Selected predictors:\n")

print(top10_vars)

###############################################################
# HYPOTHESES 
###############################################################

# H1 : Contact duration positively affects subscription likelihood
# H2 : Employment rate affects subscription likelihood
# H3 : Interest rate (Euribor) affects subscription likelihood
# H4 : Employment variation rate affects subscription likelihood
# H5 : Month of contact affects subscription likelihood
# H6 : Salary level affects subscription likelihood
# H7 : Outcome of previous campaign affects subscription likelihood
# H8 : Number of previous contacts affects subscription likelihood
# H9 : Contact method affects subscription likelihood
# H10: Consumer price index affects subscription likelihood

###############################################################
# HYPOTHESIS VISUALISATIONS (BEFORE MODELLING)
###############################################################

# 1) Helper: Proportion bar chart for categorical predictors 

plot_prop_cat <- function(df, xvar, title_txt){
  df %>%
    dplyr::count(.data[[xvar]], subscribed) %>%
    ggplot(aes(x = .data[[xvar]], y = n, fill = subscribed)) +
    geom_bar(stat = "identity", position = "fill") +
    theme_minimal() +
    labs(title = title_txt, x = xvar, y = "Proportion", fill = "Subscribed") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# 2) Helper: Binned proportion plot for numeric predictors
# (shows how subscription rate changes as the numeric variable increases)

plot_prop_num_binned <- function(df, xvar, bins = 10, title_txt){
  
  df %>%
    mutate(
      bin = cut(.data[[xvar]], breaks = bins, include.lowest = TRUE),
      bin_mid = as.numeric(sub("\\((.+),(.+)\\]", "\\1", bin)) +
        (as.numeric(sub("\\((.+),(.+)\\]", "\\2", bin)) -
           as.numeric(sub("\\((.+),(.+)\\]", "\\1", bin))) / 2
    ) %>%
    group_by(bin_mid) %>%
    summarise(
      prop_yes = mean(subscribed == "Yes"),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = bin_mid, y = prop_yes)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(
      title = title_txt,
      x = xvar,
      y = "Proportion Subscribed (Yes)"
    )
}

# 3) Helper: Boxplot (alternative numeric visual)

plot_box_num <- function(data, xvar, title_txt){
  ggplot(data, aes(x = subscribed, y = .data[[xvar]])) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = title_txt, x = "Subscribed", y = xvar)
}

################################################################################
# H1 : Contact duration positively affects subscription likelihood
################################################################################

plot_prop_num_binned(termsv_clean, "contact_duration", bins = 10,
                     "H1: Subscription Rate vs Contact Duration (Binned)")


################################################################################
# H2 : Employment rate affects subscription likelihood
################################################################################

plot_prop_num_binned(termsv_clean, "nr_employed", bins = 10,
                     "H2: Subscription Rate vs Number Employed (Binned)")

################################################################################
# H3 : Interest rate (Euribor) affects subscription likelihood
################################################################################

plot_prop_num_binned(termsv_clean, "euribor_three_mth", bins = 8,
                     "H3: Subscription Rate vs Euribor 3M (Binned)")

################################################################################
# H4 : Employment variation rate affects subscription likelihood
################################################################################

plot_prop_num_binned(termsv_clean, "empl_var_rate", bins = 10,
                     "H4: Subscription Rate vs Employment Variation Rate (Binned)")

################################################################################
# H5 : Month of contact affects subscription likelihood
################################################################################

plot_prop_cat(termsv_clean, "month", "H5: Subscription Proportion by Month")

################################################################################
# H6 : Salary level affects subscription likelihood
################################################################################

plot_prop_cat(termsv_clean, "salary_level", "H6: Subscription Proportion by Salary Level")

################################################################################
# H7 : Outcome of previous campaign affects subscription likelihood
################################################################################

plot_prop_cat(termsv_clean, "prev_campaign_outcome", "H7: Subscription Proportion by Previous Campaign Outcome")

################################################################################
# H8 : Number of previous contacts affects subscription likelihood
################################################################################

plot_prop_num_binned(termsv_clean, "prev_contacts", bins = 10,
                     "H8: Subscription Rate vs Previous Contacts (Binned)")

################################################################################
# H9 : Contact method affects subscription likelihood
################################################################################

plot_prop_cat(termsv_clean, "contact_method", "H9: Subscription Proportion by Contact Method")

################################################################################
# H10: Consumer price index affects subscription likelihood
################################################################################

plot_prop_num_binned(termsv_clean, "cons_price_index", bins = 10,
                     "H10: Subscription Rate vs Consumer Price Index (Binned)")


###############################################################
# SETTING SEED 
###############################################################

set.seed(40490877)


###############################################################
# TRAIN / TEST SPLIT (STRATIFIED)
###############################################################

index <- createDataPartition(termsv_clean$subscribed, p = 0.80, list = FALSE)
train <- termsv_clean[index, ]
test  <- termsv_clean[-index, ]

###############################################################
# TOP 5 LOGISTIC REGRESSION
###############################################################

top5_formula <- as.formula(
  paste("subscribed ~", paste(top5_vars, collapse = " + "))
)

logit_model_top5 <- glm(top5_formula, data = train, family = binomial)
summary(logit_model_top5)

# Odds Ratios for Top 5 model
or_plot_top5 <- tidy(logit_model_top5, conf.int = TRUE) %>%
  mutate(
    Odds_Ratio = exp(estimate),
    OR_Lower = exp(conf.low),
    OR_Upper = exp(conf.high)
  ) %>%
  filter(term != "(Intercept)")

ggplot(or_plot_top5, aes(x = reorder(term, Odds_Ratio), y = Odds_Ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = OR_Lower, ymax = OR_Upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Odds Ratios from Top 5 Logistic Regression Model",
    x = "Predictor",
    y = "Odds Ratio (95% CI)"
  )

###############################################################
#  COMPARISON MODEL: MULTIPLE LOGISTIC REGRESSION MODEL 
###############################################################

final_formula <- as.formula(
  paste("subscribed ~", paste(top10_vars, collapse = " + "))
)

logit_model <- glm(final_formula, data = train, family = binomial)
summary(logit_model)


or_plot <- tidy(logit_model, conf.int = TRUE) %>%
  mutate(
    Odds_Ratio = exp(estimate),
    OR_Lower = exp(conf.low),
    OR_Upper = exp(conf.high)
  ) %>%
  filter(term != "(Intercept)")

ggplot(or_plot, aes(x = reorder(term, Odds_Ratio), y = Odds_Ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = OR_Lower, ymax = OR_Upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Odds Ratios from Final Logistic Regression Model",
    x = "Predictor",
    y = "Odds Ratio (95% CI)"
  )



###############################################################
# FULL REGRESSION OUTPUT (ODDS RATIOS + CI + P-VALUES)
###############################################################

coef_table <- tidy(logit_model, conf.int = TRUE) %>%
  mutate(
    Odds_Ratio = exp(estimate),
    OR_Lower = exp(conf.low),
    OR_Upper = exp(conf.high)
  ) %>%
  arrange(p.value)

print(coef_table)

  ###############################################################
# ASSUMPTION CHECK: MULTICOLLINEARITY (VIF)
###############################################################

vif_values_top5 <- vif(logit_model_top5)
print(vif_values_top5)
cat("Average VIF (Top 5):", mean(vif_values_top5), "\n")


vif_values <- vif(logit_model)
print(vif_values)
cat("Average VIF:", mean(vif_values), "\n")


###############################################################
# TOP 5 MODEL: PREDICTION & ACCURACY
###############################################################

test_prob_top5 <- predict(logit_model_top5, newdata = test, type = "response")

test_pred_top5 <- ifelse(test_prob_top5 >= 0.5, "Yes", "No")
test_pred_top5 <- factor(test_pred_top5, levels = c("No","Yes"))

conf_matrix_top5 <- confusionMatrix(test_pred_top5, test$subscribed, positive = "Yes")
print(conf_matrix_top5)



###############################################################
# MODEL Probability Distribution
###############################################################

test$predicted_prob_top5 <- test_prob_top5

ggplot(test, aes(x = predicted_prob_top5, fill = subscribed)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Predicted Probability Distribution (Top 5 Logistic Regression)",
    x = "Predicted Probability of Subscription",
    y = "Density",
    fill = "Subscribed"
  )


test_prob <- predict(logit_model, newdata = test, type = "response")

test$predicted_prob <- test_prob

ggplot(test, aes(x = predicted_prob, fill = subscribed)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Predicted Probability Distribution by Subscription Outcome",
    x = "Predicted Probability of Subscription",
    y = "Density",
    fill = "Subscribed"
  )


# Classification threshold = 0.5

test_pred <- ifelse(test_prob >= 0.5, "Yes", "No")
test_pred <- factor(test_pred, levels = c("No","Yes"))

conf_matrix <- confusionMatrix(test_pred, test$subscribed, positive = "Yes")
print(conf_matrix)


###############################################################
# ROC CURVE & AUC
###############################################################

roc_top5 <- roc(test$subscribed, test_prob_top5, levels = c("No","Yes"), quiet = TRUE)
auc_top5 <- auc(roc_top5)

cat("AUC (Top 5) =", as.numeric(auc_top5), "\n")
plot(roc_top5,
     main = paste("ROC Curve Top 5 Model (AUC =", round(as.numeric(auc_top5), 3), ")"))

roc_obj <- roc(test$subscribed, test_prob, levels = c("No","Yes"), quiet = TRUE)
auc_value <- auc(roc_obj)

cat("AUC =", as.numeric(auc_value), "\n")
plot(roc_obj,
     main = paste("ROC Curve (AUC =", round(as.numeric(auc_value), 3), ")"))

###############################################################
# VISUAL COMPARISON: ROC Curves (Top10 vs Top5)
###############################################################

plot(roc_top5, main = "ROC Comparison: Top 10 vs Top 5", legacy.axes = TRUE, col= "green")
plot(roc_obj, add = TRUE, col = "red")

legend(
  "bottomright",
  legend = c(
    paste("Top 10 (AUC =", round(as.numeric(auc_value), 3), ")"),
    paste("Top 5 (AUC =", round(as.numeric(auc_top5), 3), ")")
  ),
  col = c("red", "green"),
  lty = 2,
  bty = "n"
)



###############################################################
# MODEL ACCURACY SUMMARY TABLE
###############################################################

accuracy_table <- tibble(
  Model = "Logistic Regression (Top predictors)",
  Accuracy = unname(conf_matrix$overall["Accuracy"]),
  Kappa = unname(conf_matrix$overall["Kappa"]),
  Sensitivity = unname(conf_matrix$byClass["Sensitivity"]),
  Specificity = unname(conf_matrix$byClass["Specificity"]),
  Precision = unname(conf_matrix$byClass["Pos Pred Value"]),
  F1 = unname(conf_matrix$byClass["F1"]),
  AUC = as.numeric(auc_value)
)

print(accuracy_table)

###############################################################
# COMPARISON: TOP 5 VS TOP 10
###############################################################

accuracy_table_top5 <- tibble(
  Model = "Logistic Regression (Top 5 predictors)",
  Accuracy = unname(conf_matrix_top5$overall["Accuracy"]),
  Kappa = unname(conf_matrix_top5$overall["Kappa"]),
  Sensitivity = unname(conf_matrix_top5$byClass["Sensitivity"]),
  Specificity = unname(conf_matrix_top5$byClass["Specificity"]),
  Precision = unname(conf_matrix_top5$byClass["Pos Pred Value"]),
  F1 = unname(conf_matrix_top5$byClass["F1"]),
  AUC = as.numeric(auc_top5)
)

comparison_table <- bind_rows(accuracy_table, accuracy_table_top5)
print(comparison_table)

###############################################################
# VISUAL COMPARISON: TOP 10 vs TOP 5 (Key Metrics)
###############################################################

# Convert to long format for ggplot
comparison_long <- comparison_table %>%
  select(Model, Accuracy, Sensitivity, Specificity, Precision, F1, AUC) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

ggplot(comparison_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Performance Comparison: Top 10 vs Top 5 Logistic Regression",
    x = "Metric",
    y = "Score"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###############################################################
# REGRESSION TABLE OUTPUT 
###############################################################

models <- list(
  "Top 10 Logistic Regression" = logit_model,
  "Top 5 Logistic Regression"  = logit_model_top5
)
options(scipen = 999)

modelsummary(
  models,
  exponentiate = TRUE,
  statistic = NULL, 
  fmt = 3,
  ci_method = "wald"
)




