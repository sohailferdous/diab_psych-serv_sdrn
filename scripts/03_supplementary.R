# This script accompanies the study: Sociodemographic risk factors for psychiatric service use among newly-diagnosed 
# adults with type 2 diabetes in Scotland
#
# Data-analysis was conducted using SDRN-NDS data within the diabepi safe haven. File paths within the haven have been 
# redacted.
# 
# Script: 03_supplementary.R: This script performs sensitivity COX analysis, explores missingness by performing 
# hypothesis tests between complete and missing subsets, and checks proportional hazards assumption by generating 
# Schoenfeld residual plots.
#
# Please ensure you have compiled scripts 01_data-prep.R and 02_survival-analysis.R before running this script.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. SENSITIVITY ANALYSIS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 6.1 Sensitivity analysis - Cox PH models =============================================================================
# 
{                               # COMPILE HERE TO GENERATE TABLE WITH SENSITIVITY ANALYSIS MODEL ESTIMATES
# 
#** 6.1.1 Univariate (unadjusted) models -------------------------------------------------------------------------------
#
#*** 6.1.1.1 Sex ####
#
uni_sens_sex.ph_with_pre <- coxph(surv_object_sens_with_pre~sex, data=complete_sens_with_pre)
uni_sens_sex.ph_without_pre <- coxph(surv_object_sens_without_pre~sex, data=complete_sens_without_pre)

#*** 6.1.1.2 Age ####
#
uni_sens_age.ph_with_pre <- coxph(surv_object_sens_with_pre~age, data=complete_sens_with_pre)
uni_sens_age.ph_without_pre <- coxph(surv_object_sens_without_pre~age, data=complete_sens_without_pre)

#*** 6.1.1.3 Ethnicity ####
#
uni_sens_ethnicity.ph_with_pre <- coxph(surv_object_sens_with_pre~ethnicity, data=complete_sens_with_pre)
uni_sens_ethnicity.ph_without_pre <- coxph(surv_object_sens_without_pre~ethnicity, data=complete_sens_without_pre)

#*** 6.1.1.4 SIMD ####
#
uni_sens_simd.ph_with_pre <- coxph(surv_object_sens_with_pre~simd, data=complete_sens_with_pre)
uni_sens_simd.ph_without_pre <- coxph(surv_object_sens_without_pre~simd, data=complete_sens_without_pre)

#** 6.1.2 Multivariate (adjusted) models -------------------------------------------------------------------------------
#
multi_sens.ph_with_pre <- coxph(surv_object_sens_with_pre~sex+age+ethnicity+simd+diag_year, data=complete_sens_with_pre)
multi_sens.ph_without_pre <- coxph(surv_object_sens_without_pre~sex+age+ethnicity+simd+diag_year, data=complete_sens_without_pre)

#** 6.1.3 Compile model estimates and p-values into a table ------------------------------------------------------------
models <- list(
  "Sex (With Pre)" = uni_sens_sex.ph_with_pre,
  "Sex (Without Pre)" = uni_sens_sex.ph_without_pre,
  "Age (With Pre)" = uni_sens_age.ph_with_pre,
  "Age (Without Pre)" = uni_sens_age.ph_without_pre,
  "Ethnicity (With Pre)" = uni_sens_ethnicity.ph_with_pre,
  "Ethnicity (Without Pre)" = uni_sens_ethnicity.ph_without_pre,
  "SIMD (With Pre)" = uni_sens_simd.ph_with_pre,
  "SIMD (Without Pre)" = uni_sens_simd.ph_without_pre,
  "Multivariable (With Pre)" = multi_sens.ph_with_pre,
  "Multivariable (Without Pre)" = multi_sens.ph_without_pre)

# Creating a function to extract model estimates
extract_cox_results <- function(model, model_name) {
  tidy_model <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  tidy_model$model <- model_name
  tidy_model}

results_df <- bind_rows(
  lapply(names(models), function(name) extract_cox_results(models[[name]], name)))

sens_table <- results_df %>%
  mutate(
    HR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p.value = formatC(p.value, format = "f", digits = 3)
  ) %>%
  select(Model = model, Variable = term, `HR (95% CI)` = HR_CI, `P-value` = p.value)

print(sens_table, row.names = F, n=Inf)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. EXPLORING MISSINGNESS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#* 7.1 Making the missing subset  ======================================================================================
# 
missing_main <- subset_main %>% 
  filter(is.na(simd_dec) | is.na(ethnicity) | is.na(health_board))

#* 7.2 Contingency table and hypothesis testing ========================================================================
#
# Chi-sq tests were conducted for categorical variables and t-test was done for continuous age
# 
#** 7.2.1 sex ----------------------------------------------------------------------------------------------------------
{
combined_sex <- c(complete_main$sex, missing_main$sex)
group_labels <- c(rep("complete_main", nrow(complete_main)), rep("missing_main", nrow(missing_main)))
sex_table <- table(combined_sex, group_labels)
print(sex_table)
chisq_test_sex <- chisq.test(sex_table)
print(chisq_test_sex)
}

#** 7.2.2 Age (continuous) ---------------------------------------------------------------------------------------------
{
t_test_age <- t.test(complete_main$age, missing_main$age)
print(t_test_age)
}

#** 7.2.3 Age (categorical) --------------------------------------------------------------------------------------------
{
combined_age_cat <- c(complete_main$age_cat, missing_main$age_cat)
group_labels <- c(rep("complete_main", nrow(complete_main)), rep("missing_main", nrow(missing_main)))
age_table <- table(combined_age_cat, group_labels)
print(age_table)
chisq_test_age <- chisq.test(age_table)
print(chisq_test_age)
}

#** 7.2.4 SIMD ---------------------------------------------------------------------------------------------------------
{
combined_simd <- c(complete_main$simd, missing_main$simd)
group_labels <- c(rep("complete_main", nrow(complete_main)), rep("missing_main", nrow(missing_main)))
simd_table <- table(combined_simd, group_labels)
print(simd_table)
chisq_test_simd <- chisq.test(simd_table)
print(chisq_test_simd)
}

#** 7.2.5 Ethnicity ----------------------------------------------------------------------------------------------------
{
missing_main_eth <- missing_main %>%
  filter(ethnicity != "missing_main")

missing_main_eth$ethnicity <- droplevels(missing_main_eth$ethnicity)

table(missing_main_eth$ethnicity)

combined_ethnicity <- c(complete_main$ethnicity, missing_main_eth$ethnicity)
group_labels <- c(rep("complete_main", nrow(complete_main)), rep("missing_main", nrow(missing_main_eth)))
ethnicity_table <- table(combined_ethnicity, group_labels)
print(ethnicity_table)
chisq_test_ethnicity <- chisq.test(ethnicity_table)
print(chisq_test_ethnicity)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8. PROPORTIONAL HAZARDS ASSUMPTION TESTING ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
{                                 # COMPILE HERE TO PREPARE ALL PLOTS
# 
# 8.1 Univariable (unadjusted) models ==================================================================================
# 
# 8.1.1 No history of prior psychiatric service use --------------------------------------------------------------------
# 
# Age
sch_uni_without_age <- ggcoxzph(
  fit = cox.zph(uni_age.ph_without_pre),
  point.col = "red",
  point.size = 2,
  var = c("age"),
  ggtheme = theme_bw())

# Sex
sch_uni_without_sex <-ggcoxzph(
  fit = cox.zph(uni_sex.ph_without_pre),
  point.col = "red",
  point.size = 2,
  var = c("sex"),
  ggtheme = theme_bw())

# SIMD
sch_uni_without_simd <-ggcoxzph(
  fit = cox.zph(uni_simd.ph_without_pre),
  point.col = "red",
  point.size = 2,
  var = c("simd"),
  ggtheme = theme_bw())

# Ethnicity
sch_uni_without_ethnicity <- ggcoxzph(
  fit = cox.zph(uni_ethnicity.ph_without_pre),
  point.col = "red",
  point.size = 2,
  var = c("ethnicity"),
  ggtheme = theme_bw())

# 8.1.2 History of prior psychiatric service use -----------------------------------------------------------------------
# 
# Age
sch_uni_with_age <- ggcoxzph(
  fit = cox.zph(uni_age.ph_with_pre),
  point.col = "red",
  point.size = 2,
  var = c("age"),
  ggtheme = theme_bw())

# Sex
sch_uni_with_sex <-ggcoxzph(
  fit = cox.zph(uni_sex.ph_with_pre),
  point.col = "red",
  point.size = 2,
  var = c("sex"),
  ggtheme = theme_bw())

# SIMD
sch_uni_with_simd <-ggcoxzph(
  fit = cox.zph(uni_simd.ph_with_pre),
  point.col = "red",
  point.size = 2,
  var = c("simd"),
  ggtheme = theme_bw())

# Ethnicity
sch_uni_with_ethnicity <- ggcoxzph(
  fit = cox.zph(uni_ethnicity.ph_with_pre),
  point.col = "red",
  point.size = 2,
  var = c("ethnicity"),
  ggtheme = theme_bw())

# 8.2 Multivariable (adjusted) models ==================================================================================
# 
# 8.2.1 No history of prior psychiatric service use --------------------------------------------------------------------
#
sch_multi_without <- ggcoxzph(
  fit = cox.zph(multi.ph_without_pre),
  point.col = "red",
  point.size = 2,
  var = c("age", "sex", "simd", "ethnicity"),
  ggtheme = theme_bw())

# 8.2.2 History of prior psychiatric service use -----------------------------------------------------------------------
# 
sch_multi_with <- ggcoxzph(
  fit = cox.zph(multi.ph_with_pre),
  point.col = "red",
  point.size = 2,
  var = c("age", "sex", "simd", "ethnicity"),
  ggtheme = theme_bw())
}

# 8.3.3. View plots ----------------------------------------------------------------------------------------------------
print(sch_uni_without_age)
print(sch_uni_without_sex)
print(sch_uni_without_simd)
print(sch_uni_without_ethnicity)
print(sch_uni_with_age)
print(sch_uni_with_sex)
print(sch_uni_with_simd)
print(sch_uni_with_ethnicity)
print(sch_multi_without)
print(sch_multi_with)

# Please check script 04_export.R for export options.
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~