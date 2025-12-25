# This script accompanies the study: Sociodemographic risk factors for psychiatric service use among newly-diagnosed 
# adults with type 2 diabetes in Scotland
#
# Data-analysis was conducted using SDRN-NDS data within the diabepi safe haven. File paths within the haven have been 
# redacted.
# 
# Script: 02_survival-analysis.R: This script performs the main survival analysis, and generates Kaplan-Meier (KM)
# plots, and forest plots depicting Cox model estimates for main analysis.
# 
# Please ensure you have compiled the 01_data-prep.R script before running this script.
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. SURVIVAL ANALYSIS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
{                               # COMPILE HERE TO FINISH SURVIVAL ANALYSIS (STEP 4)
#
#* 4.1 Load packages and import data ===================================================================================
#
library(tidyverse)
library(survival)
library(survminer)

#* 4.2 Kaplan-Meier plots ==============================================================================================
#
#** 4.2.1 Sex ----------------------------------------------------------------------------------------------------------
# 
sex.km <-survfit(surv_object_main~sex,data=complete_main)

survplot_sex <- ggsurvplot(
  sex.km, 
  data = complete_main, 
  size = 1,                 
  conf.int = FALSE, 
  pval = FALSE, 
  risk.table = TRUE, 
  risk.table.col = "strata",
  legend.labs = c("Males", "Females"),
  legend.title = "",
  risk.table.height = 0.3, 
  ggtheme = theme_bw(),
  ylim = c(0.75, 1),
  xlim = c(0, 19.5),
  break.time.by = 3,
  xlab = "Follow-up time (years)",
  ylab = "Proportion of cohort without psychiatric service use",
  censor = FALSE)

survplot_sex$plot <- survplot_sex$plot + ggtitle("KM PLOT FOR SEX")

#** 4.2.2 Age groups (categorical age) ---------------------------------------------------------------------------------
#
age_cat.km <-survfit(surv_object_main~age_cat,data=complete_main)

survplot_age_cat <-ggsurvplot(
  age_cat.km, 
  data = complete_main, 
  size = 1,                 
  conf.int = FALSE, 
  pval = FALSE, 
  risk.table = TRUE, 
  risk.table.col = "strata",
  legend.labs = c("18-40", "40-50", "50-60", "60-70", "70+"),
  legend.title = "",
  risk.table.height = 0.3, 
  ggtheme = theme_bw(),
  ylim = c(0.75, 1),
  xlim = c(0, 19.5),
  break.time.by = 3,
  xlab = "Follow-up time (years)",
  ylab = "Proportion of cohort without psychiatric service use",
  censor = FALSE)

survplot_age_cat$plot <- survplot_age_cat$plot + ggtitle("KM PLOT FOR AGE-GROUPS")

#** 4.2.3 SIMD quintiles -----------------------------------------------------------------------------------------------
#
simd.km <-survfit(surv_object_main~simd_km_plot,data=complete_main)

survplot_simd <-ggsurvplot(
  simd.km, 
  data = complete_main, 
  size = 1,                 
  conf.int = FALSE, 
  pval = FALSE, 
  risk.table = TRUE, 
  risk.table.col = "strata",
  legend.labs = c("1st quintile (most deprived)", "2nd quintile", "3rd quintile", "4th quintile", "5th quintile"),
  legend.title = "",
  risk.table.height = 0.3, 
  ggtheme = theme_bw(),
  ylim = c(0.75, 1),
  xlim = c(0, 19.5),
  break.time.by = 3,
  xlab = "Follow-up time (years)",
  ylab = "Proportion of cohort without psychiatric service use",
  censor = FALSE)

survplot_simd$plot <- survplot_simd$plot + ggtitle("KM PLOT FOR SIMD QUINTILES")

#** 4.2.4 Ethnicity ----------------------------------------------------------------------------------------------------
#
ethnicity.km <-survfit(surv_object_main~ethnicity_km_plot,data=complete_main)
survplot_ethnicity <-ggsurvplot(
  ethnicity.km, 
  data = complete_main, 
  size = 1,                 
  conf.int = FALSE, 
  pval = FALSE, 
  risk.table = TRUE, 
  risk.table.col = "strata",
  legend.labs = c("White", "Black", "South Asian", "Mixed", "Other ethnic group"),
  legend.title = "",
  risk.table.height = 0.3, 
  ggtheme = theme_bw(),
  ylim = c(0.75, 1),
  xlim = c(0, 19.5),
  break.time.by = 3,
  xlab = "Follow-up time (years)",
  ylab = "Proportion of cohort without psychiatric service use",
  censor = FALSE)

survplot_ethnicity$plot <- survplot_ethnicity$plot + ggtitle("KM PLOT FOR ETHNICITY")

#** 4.2.5 View the plots ----------------------------------------------------------------------------------------------
# 
# print(survplot_sex)
# print(survplot_age_cat)
# print(survplot_simd)
# print(survplot_ethnicity)

#* 4.3 Cox PH Modelling ================================================================================================
#
# Models are stratified according to history of psychiatric service prior diabetes diagnosis (with_pre and without_pre)
#
#** 4.3.1 Univariate (unadjusted) models -------------------------------------------------------------------------------
#
#*** 4.3.1.1 Sex ####
#
uni_sex.ph_with_pre <- coxph(surv_object_main_with_pre~sex, data=complete_main_with_pre)
uni_sex.ph_without_pre <- coxph(surv_object_main_without_pre~sex, data=complete_main_without_pre)

#*** 4.3.1.2 Age ####
#
uni_age.ph_with_pre <- coxph(surv_object_main_with_pre~age, data=complete_main_with_pre)
uni_age.ph_without_pre <- coxph(surv_object_main_without_pre~age, data=complete_main_without_pre)

#*** 4.3.1.3 Ethnicity ####
#
uni_ethnicity.ph_with_pre <- coxph(surv_object_main_with_pre~ethnicity, data=complete_main_with_pre)
uni_ethnicity.ph_without_pre <- coxph(surv_object_main_without_pre~ethnicity, data=complete_main_without_pre)

#*** 4.3.1.4 SIMD ####
#
uni_simd.ph_with_pre <- coxph(surv_object_main_with_pre~simd, data=complete_main_with_pre)
uni_simd.ph_without_pre <- coxph(surv_object_main_without_pre~simd, data=complete_main_without_pre)

#** 4.3.2 Multivariate (adjusted) models -------------------------------------------------------------------------------
#
multi.ph_with_pre <- coxph(surv_object_main_with_pre~sex+age+ethnicity+simd+diag_year, data=complete_main_with_pre)
multi.ph_without_pre <- coxph(surv_object_main_without_pre~sex+age+ethnicity+simd+diag_year, data=complete_main_without_pre)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. FOREST PLOT FOR COX MODEL ESTIMATES (Figure 2) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# IMPORTANT - Code for both forest plot designs are added here. Only the selected design code will be retained in the
# final script.
#
# 5.1 Data preparation for forest plot =================================================================================
# 
{                                           # COMPILE HERE TO FINISH PLOT DATA PREPARATION
# 
# 5.1.1 Creating function to extract Cox model estimates ---------------------------------------------------------------
# 
extract_cox <- function(model) {
  s <- summary(model)
  data.frame(
    hr    = s$coef[, "exp(coef)"],
    lower = s$conf.int[, "lower .95"],
    upper = s$conf.int[, "upper .95"],
    row.names = rownames(s$coef))
}

# 5.1.2 Extracting model estimates -------------------------------------------------------------------------------------
# 
sex_with_pre        <- extract_cox(uni_sex.ph_with_pre)
sex_without_pre     <- extract_cox(uni_sex.ph_without_pre)

age_with_pre        <- extract_cox(uni_age.ph_with_pre)
age_without_pre     <- extract_cox(uni_age.ph_without_pre)

simd_with_pre       <- extract_cox(uni_simd.ph_with_pre)
simd_without_pre    <- extract_cox(uni_simd.ph_without_pre)

ethnicity_with_pre  <- extract_cox(uni_ethnicity.ph_with_pre)
ethnicity_without_pre <- extract_cox(uni_ethnicity.ph_without_pre)

multi_with_pre      <- extract_cox(multi.ph_with_pre)
multi_without_pre   <- extract_cox(multi.ph_without_pre)

# 5.1.3 Labelling and reordering ethnicity variable --------------------------------------------------------------------
#
# This is being done so that the plot label looks like "Ethnicity - Black", instead of "ethnicityBlack". This is not
# a problem for other variables.
#
ethnicity_order_model <- c(
  "ethnicityBlack",
  "ethnicitySouth Asian",
  "ethnicityMixed",
  "ethnicityOther ethnic group")

ethnicity_labels <- c(
  "Ethnicity - Black",
  "Ethnicity - South Asian",
  "Ethnicity - Mixed",
  "Ethnicity - Other Ethnic Group")

# 5.1.4 Building individual data frames for each model and combining them ----------------------------------------------
#
# Previous psychiatric service use, univariable (unadjusted)
# 
df_uni_with_pre <- data.frame(
  variable = c(
    "Age",
    "Female",
    paste("SIMD", c("1st Quintile", "2nd Quintile", "3rd Quintile", "4th Quintile")),
    ethnicity_labels
  ),
  hr = c(
    age_with_pre[1, "hr"],
    sex_with_pre[1, "hr"],
    simd_with_pre[1:4, "hr"],
    ethnicity_with_pre[ethnicity_order_model, "hr"]
  ),
  lower = c(
    age_with_pre[1, "lower"],
    sex_with_pre[1, "lower"],
    simd_with_pre[1:4, "lower"],
    ethnicity_with_pre[ethnicity_order_model, "lower"]
  ),
  upper = c(
    age_with_pre[1, "upper"],
    sex_with_pre[1, "upper"],
    simd_with_pre[1:4, "upper"],
    ethnicity_with_pre[ethnicity_order_model, "upper"]
  ),
  model_type = "Univariate",
  group = "With Pre")

# No previous psychiatric service use, univariable (unadjusted)
# 
df_uni_without_pre <- df_uni_with_pre %>%
  mutate(
    hr = c(
      age_without_pre[1, "hr"],
      sex_without_pre[1, "hr"],
      simd_without_pre[1:4, "hr"],
      ethnicity_without_pre[ethnicity_order_model, "hr"]
    ),
    lower = c(
      age_without_pre[1, "lower"],
      sex_without_pre[1, "lower"],
      simd_without_pre[1:4, "lower"],
      ethnicity_without_pre[ethnicity_order_model, "lower"]
    ),
    upper = c(
      age_without_pre[1, "upper"],
      sex_without_pre[1, "upper"],
      simd_without_pre[1:4, "upper"],
      ethnicity_without_pre[ethnicity_order_model, "upper"]
    ),
    group = "Without Pre")

# Previous psychiatric service use, multivariable (adjusted)
# 
df_multi_with_pre <- df_uni_with_pre %>%
  mutate(
    hr = c(
      multi_with_pre["age", "hr"],
      multi_with_pre["sexFemale", "hr"],
      multi_with_pre[paste0("simd", c("1st", "2nd", "3rd", "4th")), "hr"],
      multi_with_pre[ethnicity_order_model, "hr"]
    ),
    lower = c(
      multi_with_pre["age", "lower"],
      multi_with_pre["sexFemale", "lower"],
      multi_with_pre[paste0("simd", c("1st", "2nd", "3rd", "4th")), "lower"],
      multi_with_pre[ethnicity_order_model, "lower"]
    ),
    upper = c(
      multi_with_pre["age", "upper"],
      multi_with_pre["sexFemale", "upper"],
      multi_with_pre[paste0("simd", c("1st", "2nd", "3rd", "4th")), "upper"],
      multi_with_pre[ethnicity_order_model, "upper"]
    ),
    model_type = "Multivariate")

# No previous psychiatric service use, multivariable (adjusted)
# 
df_multi_without_pre <- df_multi_with_pre %>%
  mutate(
    hr = c(
      multi_without_pre["age", "hr"],
      multi_without_pre["sexFemale", "hr"],
      multi_without_pre[paste0("simd", c("1st", "2nd", "3rd", "4th")), "hr"],
      multi_without_pre[ethnicity_order_model, "hr"]
    ),
    lower = c(
      multi_without_pre["age", "lower"],
      multi_without_pre["sexFemale", "lower"],
      multi_without_pre[paste0("simd", c("1st", "2nd", "3rd", "4th")), "lower"],
      multi_without_pre[ethnicity_order_model, "lower"]
    ),
    upper = c(
      multi_without_pre["age", "upper"],
      multi_without_pre["sexFemale", "upper"],
      multi_without_pre[paste0("simd", c("1st", "2nd", "3rd", "4th")), "upper"],
      multi_without_pre[ethnicity_order_model, "upper"]
    ),
    group = "Without Pre")

# Combining
# 
df_all <- bind_rows(
  df_uni_with_pre,
  df_uni_without_pre,
  df_multi_with_pre,
  df_multi_without_pre)

# 5.1.5 Creating labels and setting the variable order in the plot -----------------------------------------------------
#
df_all$label_txt <- sprintf("%.2f (%.2f - %.2f)", df_all$hr, df_all$lower, df_all$upper)

df_all$variable <- factor(
  df_all$variable,
  levels = c(
    "Age",
    "Female",
    "SIMD 1st Quintile",
    "SIMD 2nd Quintile",
    "SIMD 3rd Quintile",
    "SIMD 4th Quintile",
    ethnicity_labels))

}

# 5.1.6 DESIGN 1 - Faceted 2x2 forest plot -----------------------------------------------------------------------------
#
{                            # COMPILE HERE TO GENERATE AND VIEW PLOT
#
# 5.1.6.1 Ordering and defining some forest plot elements before plotting ####
# 
df_plot <- df_all %>%
  mutate(
    panel = case_when(
      group == "Without Pre" & model_type == "Univariate"   ~ "No previous psychiatric service use\n(unadjusted)",
      group == "Without Pre" & model_type == "Multivariate" ~ "No previous psychiatric service use\n(adjusted)",
      group == "With Pre"    & model_type == "Univariate"   ~ "Previous psychiatric service use\n(unadjusted)",
      group == "With Pre"    & model_type == "Multivariate" ~ "Previous psychiatric service use\n(adjusted)"),
    panel = factor(
      panel,
      levels = c(
        "No previous psychiatric service use\n(unadjusted)",
        "No previous psychiatric service use\n(adjusted)",
        "Previous psychiatric service use\n(unadjusted)",
        "Previous psychiatric service use\n(adjusted)")),
    
    # Specifying groups so that they can be coloured separately
    var_group = case_when(
      variable == "Age"                  ~ "Age",
      variable == "Female"               ~ "Sex",
      str_detect(variable, "^SIMD")      ~ "SIMD",
      str_detect(variable, "^Ethnicity") ~ "Ethnicity"),
    
    # Y-axis labels (with reference groups in new line)
    variable_label = case_when(
      variable == "Female" ~ "Sex - Female\n(ref: Male)",
      variable == "SIMD 1st Quintile" ~
        "SIMD 1st Quintile (most deprived)\n(ref: 5th Quintile)",
      variable == "Ethnicity - Black" ~
        "Ethnicity - Black\n(ref: White)",
      TRUE ~ variable))

# Lock y-axis order to original variable order
y_levels <- df_plot %>%
  distinct(variable, variable_label) %>%
  arrange(factor(variable, levels = levels(df_all$variable))) %>%
  pull(variable_label)

# 5.1.6.2 Plot figure 2 - faceted forest design ####
# 
figure_2_faceted <- ggplot(df_plot, aes(x = hr, y = variable_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  
  geom_errorbarh(
    aes(xmin = lower, xmax = upper, colour = var_group),
    height = 0.2) +
  
  geom_point(aes(colour = var_group), size = 2.6) +
  
  geom_text(
    aes(x = pmin(upper + 0.08, 2.6), label = label_txt, colour = var_group),        # A bit of margin so that estimates do not overlap with plots
    hjust = 0,
    size = 3.3,
    show.legend = FALSE) +
  
  facet_wrap(~ panel, ncol = 2) +
  
  scale_colour_manual(
    values = c(
      "Age"       = "#22007c",
      "Sex"       = "#f17105",
      "SIMD"      = "#243e36",
      "Ethnicity" = "#f8333c")) +
  
  scale_y_discrete(limits = rev(y_levels)) +
  
  scale_x_continuous(
    "Hazard ratio (95% CI)",
    limits = c(0.35, 2.8),
    expand = expansion(mult = c(0.02, 0.20))) +
  
  theme_bw(base_size = 13) +
  theme(
    panel.grid       = element_blank(),
    axis.title.y     = element_blank(),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    strip.background = element_rect(fill = "white", colour = "black"))

print(figure_2_faceted)
}

# 5.1.7 DESIGN 2 - Basic forest plot -----------------------------------------------------------------------------
#
{                            # COMPILE HERE TO GENERATE PLOT
#
# 5.1.7.1 Preparation for design 2 - creating a single combined factor for all models
# 
df_all$interaction_group_model <- interaction(
  df_all$group,
  df_all$model_type)

# 5.1.7.2 Plot figure 2 - basic forest design ####
# 
figure_2_basic <- ggplot(df_all, aes(x = hr, y = variable)) +
  
  geom_point(aes(shape = interaction_group_model,
                 fill  = interaction_group_model,
                 color = interaction_group_model),
             size = 3,
             stroke = 0,
             position = position_dodge(width = 0.5)) +
  
  geom_errorbar(aes(xmin = lower, xmax = upper,
                    color = interaction_group_model),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  
  geom_text(aes(x = upper, label = label_txt,
                color = interaction_group_model),
            position = position_dodge(width = 0.5),
            hjust = -0.1, size = 4) +
  
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  scale_fill_manual(
    values = c(
      "Without Pre.Univariate"   = "#22007c",
      "Without Pre.Multivariate" = "#f17105",
      "With Pre.Univariate"      = "#243e36",
      "With Pre.Multivariate"    = "#f8333c"),
    
    breaks = c(                                                     # to control legend order
      "Without Pre.Univariate",
      "Without Pre.Multivariate",
      "With Pre.Univariate",
      "With Pre.Multivariate"),
    
    labels = c(
      "Without Pre.Univariate"   = "No previous psychiatric service use, unadjusted",
      "Without Pre.Multivariate" = "No previous psychiatric service use, adjusted",
      "With Pre.Univariate"      = "Previous psychiatric service use, unadjusted",
      "With Pre.Multivariate"    = "Previous psychiatric service use, adjusted"),
    
    guide = guide_legend(ncol = 1, override.aes = list(
      shape = rep(23, 4),  # all diamonds
      fill  = c("#22007c", "#f17105", "#243e36", "#f8333c"),
      color = c("#22007c", "#f17105", "#243e36", "#f8333c"),
      stroke = 0,
      size = 3))) +
  
  scale_color_manual(
    values = c(
      "Without Pre.Univariate"   = "#22007c",
      "Without Pre.Multivariate" = "#f17105",
      "With Pre.Univariate"      = "#243e36",
      "With Pre.Multivariate"    = "#f8333c"),
    guide = "none") +
  
  scale_shape_manual(
    values = c(
      "Without Pre.Univariate"   = 23,
      "Without Pre.Multivariate" = 23,
      "With Pre.Univariate"      = 23,
      "With Pre.Multivariate"    = 23),
    guide = "none") +
  
  scale_y_discrete(limits = rev(levels(df_all$variable))) +
  scale_x_continuous("HR (95% CI)",
                     limits = c(0.3, 2.6),
                     breaks = seq(0.4, 2.4, by = 0.2)) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1, 1),
    legend.background   = element_blank(),
    legend.title        = element_blank(),
    legend.text         = element_text(size = 10),
    panel.grid.major    = element_blank(),
    panel.grid.minor    = element_blank(),
    axis.ticks.y        = element_blank(),
    axis.text.y         = element_text(),
    axis.title.y        = element_blank(),
    axis.title.x        = element_text(size = 10, margin = margin(t = 10)))

print(figure_2_basic)
}

# Please check script 04_export.R for export options.
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~