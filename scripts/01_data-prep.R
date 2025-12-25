# This script accompanies the study: Sociodemographic risk factors for psychiatric service use among newly-diagnosed 
# adults with type 2 diabetes in Scotland
#
# Data-analysis was conducted using SDRN-NDS data within the diabepi safe haven. File names and paths within the haven 
# have been redacted.
# 
# Script: 01_data-prep.R: This script prepares data for survival analysis (main and sensitivity analysis), and 
# generates descriptive population characteristics including baseline table, median ages for study population, median 
# follow-up times, and total event counts.
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. DATA IMPORT AND PREPARATION ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
{                               # COMPILE HERE TO FINISH DATA PREPARATION (STEPS 1 AND 2)
#
#* 1.1 Load packages and import data ===================================================================================
#
library(tidyverse)
library(survival)
library(magrittr)
library(glue)

cohort <- readRDS(file = file.path("filepath", "filename.rds"))
  
#* 1.2 Removing age<18, modifying variables, and making a subset of variables required =================================
#
#** 1.2.1 Main analysis ------------------------------------------------------------------------------------------------
# 
data_main <- cohort %>% 
  filter(age_at_diag>=18) %>% 
  mutate(age = age_at_diag,
         sex = factor(gender_cat),
         simd_dec = factor(simd.decile),
         ethnicity = factor(ethnic_5),
         health_board = factor(hba_cat),
         inpat_pre = factor(smr04_pre),
         inpat_post = factor(smr04_post),
         outpat_pre = factor(smr00_pre),
         outpat_post = factor(smr00_post),
         inpat_pre_cond = factor(smr04_pre_mhc),
         inpat_post_cond = factor(smr04_post_mhc),
         inpat_post_date = as.Date(smr04_post_date),
         outpat_post_date = as.Date(smr00_post_date),
         start_date = as.Date(earliest.mention),
         diag_year = year(start_date),
         death_date = as.Date(date.of.death),
         study_end = as.Date("2022-06-30"))


subset_main <- subset(data_main, select = c(serialno, age, sex, simd_dec, health_board, inpat_pre, inpat_post, 
                                            outpat_pre, outpat_post, inpat_pre_cond, inpat_post_cond, inpat_post_date, 
                                            outpat_post_date, start_date, death_date, study_end, ethnicity, diag_year))

#** 1.2.2 Sensitivity analysis -----------------------------------------------------------------------------------------
#
# Sensitivity analysis included participants aged >=30 years to minimise risk of diabetes misdiagnosis
#
data_sens <- cohort %>% 
  filter(age_at_diag>=30) %>%
  mutate(age = age_at_diag,
         sex = factor(gender_cat),
         simd_dec = factor(simd.decile),
         ethnicity = factor(ethnic_5),
         health_board = factor(hba_cat),
         inpat_pre = factor(smr04_pre),
         inpat_post = factor(smr04_post),
         outpat_pre = factor(smr00_pre),
         outpat_post = factor(smr00_post),
         inpat_pre_cond = factor(smr04_pre_mhc),
         inpat_post_cond = factor(smr04_post_mhc),
         inpat_post_date = as.Date(smr04_post_date),
         outpat_post_date = as.Date(smr00_post_date),
         start_date = as.Date(earliest.mention),
         diag_year = year(start_date),
         death_date = as.Date(date.of.death),
         study_end = as.Date("2022-06-30"))


subset_sens <- subset(data_sens, select = c(serialno, age, sex, simd_dec, health_board, inpat_pre, inpat_post, 
                                            outpat_pre, outpat_post, inpat_pre_cond, inpat_post_cond, inpat_post_date, 
                                            outpat_post_date, start_date, death_date, study_end, ethnicity, diag_year))

#* 1.3 Categorising age (for KM plots) =================================================================================
#
# Only for main analysis
#
subset_main$age_cat <- cut(subset_main$age,
                      breaks = c(18,40,50,60,70,Inf),
                      labels = c("18-39", "40-49", "50-59", "60-69", "70+"),
                      right = FALSE)

#* 1.4 Transforming SIMD deciles to quintiles ==========================================================================
#
#** 1.4.1 Main analysis ------------------------------------------------------------------------------------------------
# 
subset_main$simd_dec <- as.numeric(as.character(subset_main$simd_dec))

subset_main$simd <- cut(subset_main$simd_dec,
                   breaks = c(0,2,4,6,8,10),
                   labels = c("1st", "2nd", "3rd", "4th", "5th"),
                   right = TRUE)

subset_main <- subset_main %>% 
  mutate(simd = factor(simd))

#** 1.4.2 Sensitivity analysis -----------------------------------------------------------------------------------------
#
subset_sens$simd_dec <- as.numeric(as.character(subset_sens$simd_dec))

subset_sens$simd <- cut(subset_sens$simd_dec,
                        breaks = c(0,2,4,6,8,10),
                        labels = c("1st", "2nd", "3rd", "4th", "5th"),
                        right = TRUE)

subset_sens <- subset_sens %>% 
  mutate(simd = factor(simd))

#* 1.5 Removing entries for deaths or outpatient visits after June 30, 2022 and creating yes/no categories for them ====
#
#** 1.5.1 Main analysis ------------------------------------------------------------------------------------------------
#
subset_main <- subset_main %>%
  mutate(death_date=replace(death_date, death_date > "2022-06-30", NA))

subset_main$died = as.factor(case_when(is.na(subset_main$date.of.death)==TRUE~"FALSE", 
                                  TRUE~"TRUE"))

subset_main <- subset_main %>%
  mutate(outpat_post_date=replace(outpat_post_date, outpat_post_date > "2022-06-30", NA))

subset_main$outpat_post = as.factor(case_when(is.na(subset_main$outpat_post_date)==TRUE~"FALSE",
                      TRUE~"TRUE"))

#** 1.5.2 Sensitivity analysis -----------------------------------------------------------------------------------------
#
subset_sens <- subset_sens %>%
  mutate(death_date=replace(death_date, death_date > "2022-06-30", NA))

subset_sens$died = as.factor(case_when(is.na(subset_sens$date.of.death)==TRUE~"FALSE", 
                                       TRUE~"TRUE"))

subset_sens <- subset_sens %>%
  mutate(outpat_post_date=replace(outpat_post_date, outpat_post_date > "2022-06-30", NA))

subset_sens$outpat_post = as.factor(case_when(is.na(subset_sens$outpat_post_date)==TRUE~"FALSE",
                                              TRUE~"TRUE"))

#* 1.6 Making aggregated columns for inpat only, outpat only, both, and none ===========================================
#
#** 1.6.1 Main analysis ------------------------------------------------------------------------------------------------
#
subset_main <- subset_main %>%
  mutate(
    inpat_pre = as.logical(as.character(inpat_pre)),
    inpat_post = as.logical(as.character(inpat_post)),
    outpat_pre = as.logical(as.character(outpat_pre)),
    outpat_post = as.logical(as.character(outpat_post))
  ) %>%
  mutate(
    inpat_only = (inpat_pre | inpat_post) & !(outpat_pre | outpat_post),
    outpat_only = (outpat_pre | outpat_post) & !(inpat_pre | inpat_post),
    inpat_pre_only = inpat_pre & !outpat_pre,
    inpat_post_only = inpat_post & !outpat_post,
    outpat_pre_only = outpat_pre & !inpat_pre,
    outpat_post_only = outpat_post & !inpat_post,
    both = (inpat_pre | inpat_post) & (outpat_pre | outpat_post),
    none = !(inpat_pre | inpat_post | outpat_pre | outpat_post),
    both_pre = inpat_pre & outpat_pre,
    both_post = inpat_post & outpat_post,
    none_pre = !(inpat_pre | outpat_pre),
    none_post = !(inpat_post | outpat_post),
    post_only = (inpat_post_only | outpat_post_only | (inpat_post_only & outpat_post_only))
  ) %>%
  mutate(
    inpat_only = factor(inpat_only),
    outpat_only = factor(outpat_only),
    inpat_pre_only = factor(inpat_pre_only),
    inpat_post_only = factor(inpat_post_only),
    outpat_pre_only = factor(outpat_pre_only),
    outpat_post_only = factor(outpat_post_only),
    both = factor(both),
    none = factor(none),
    both_pre = factor(both_pre),
    both_post = factor(both_post),
    none_pre = factor(none_pre),
    none_post = factor(none_post),
    post_only = factor(post_only)
  )

#** 1.6.2 Sensitivity analysis -----------------------------------------------------------------------------------------
#
subset_sens <- subset_sens %>%
  mutate(
    inpat_pre = as.logical(as.character(inpat_pre)),
    inpat_post = as.logical(as.character(inpat_post)),
    outpat_pre = as.logical(as.character(outpat_pre)),
    outpat_post = as.logical(as.character(outpat_post))
  ) %>%
  mutate(
    inpat_only = (inpat_pre | inpat_post) & !(outpat_pre | outpat_post),
    outpat_only = (outpat_pre | outpat_post) & !(inpat_pre | inpat_post),
    inpat_pre_only = inpat_pre & !outpat_pre,
    inpat_post_only = inpat_post & !outpat_post,
    outpat_pre_only = outpat_pre & !inpat_pre,
    outpat_post_only = outpat_post & !inpat_post,
    both = (inpat_pre | inpat_post) & (outpat_pre | outpat_post),
    none = !(inpat_pre | inpat_post | outpat_pre | outpat_post),
    both_pre = inpat_pre & outpat_pre,
    both_post = inpat_post & outpat_post,
    none_pre = !(inpat_pre | outpat_pre),
    none_post = !(inpat_post | outpat_post),
    post_only = (inpat_post_only | outpat_post_only | (inpat_post_only & outpat_post_only))
  ) %>%
  mutate(
    inpat_only = factor(inpat_only),
    outpat_only = factor(outpat_only),
    inpat_pre_only = factor(inpat_pre_only),
    inpat_post_only = factor(inpat_post_only),
    outpat_pre_only = factor(outpat_pre_only),
    outpat_post_only = factor(outpat_post_only),
    both = factor(both),
    none = factor(none),
    both_pre = factor(both_pre),
    both_post = factor(both_post),
    none_pre = factor(none_pre),
    none_post = factor(none_post),
    post_only = factor(post_only)
  )

#* 1.7 Making the complete data subset by removing missing values, setting reference groups and re-leveling ============
#
#** 1.7.1 Main analysis ------------------------------------------------------------------------------------------------
#
complete_main <- subset_main %>% 
  drop_na(simd_dec, health_board, ethnicity)

complete_main <- complete_main %>% 
  mutate(simd = relevel(simd, ref="5th"),
         simd_km_plot = factor(simd, levels = c("1st", "2nd", "3rd", "4th", "5th")),     # Creating a new variable for km plot because cannot relevel for modelling
         sex=relevel(sex, ref="Male"),
         ethnicity=relevel(ethnicity, ref="White"),
         ethnicity_km_plot = factor(ethnicity, levels = c("White", "Black", "South Asian", "Mixed", "Other ethnic group"
                                                          )))

#** 1.7.2 Sensitivity analysis -----------------------------------------------------------------------------------------
#
complete_sens <- subset_sens %>% 
  drop_na(simd_dec, health_board, ethnicity)

complete_sens <- complete_sens %>% 
  mutate(simd = relevel(simd, ref="5th"),
         simd_km_plot = factor(simd, levels = c("1st", "2nd", "3rd", "4th", "5th")),
         sex=relevel(sex, ref="Male"),
         ethnicity=relevel(ethnicity, ref="White"),
         ethnicity_km_plot = factor(ethnicity, levels = c("White", "Black", "South Asian", "Mixed", "Other ethnic group"
                                                          )))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. SURVIVAL ANALYSIS PREPARATION ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Preparation for main and sensitivity analysis are done in parallel, they have not been divided into sub-sections.
# 
#* 2.1 Define a variable "end_point" ===================================================================================
#
# end_point is assigned the first among death_date, inpat_post_date, outpat_post_date, or study_end dates
# 
complete_main$end_point <- apply(complete_main[, c("inpat_post_date", 
                                         "outpat_post_date", 
                                         "study_end")], 
                            1, function(x) min(x, na.rm = TRUE))

complete_sens$end_point <- apply(complete_sens[, c("inpat_post_date", 
                                                   "outpat_post_date", 
                                                   "study_end")], 
                                 1, function(x) min(x, na.rm = TRUE))

#* 2.2 Censoring participants at death_date if it occurs before end_point ==============================================
# 
complete_main <- complete_main %>% 
  mutate(end_point = ifelse(!is.na(death_date) & death_date < end_point, 
                            as.Date(death_date), 
                            as.Date(end_point)))

complete_main <- complete_main %>%
  mutate(end_point = as.Date(end_point, origin = "1970-01-01"))

complete_sens <- complete_sens %>% 
  mutate(end_point = ifelse(!is.na(death_date) & death_date < end_point, 
                            as.Date(death_date), 
                            as.Date(end_point)))

complete_sens <- complete_sens %>%
  mutate(end_point = as.Date(end_point, origin = "1970-01-01"))

#* 2.3 Calculating time to event in days and converting it to time to years to make KM plots more intuitive =============
#
complete_main$time_to_event_days <- as.numeric(complete_main$end_point - complete_main$start_date)

complete_main$time_years <- complete_main$time_to_event_days/365

complete_sens$time_to_event_days <- as.numeric(complete_sens$end_point - complete_sens$start_date)

complete_sens$time_years <- complete_sens$time_to_event_days/365

#* 2.4 Creating status indicator =======================================================================================
#
#* If the end_point is before study_end and it's not due to death, it's an event, otherwise it's censored.
# 
complete_main$status <- ifelse(complete_main$end_point < complete_main$study_end & 
                            (is.na(complete_main$death_date) | complete_main$end_point != as.Date(complete_main$death_date)), 
                          "event", "censored")

complete_sens$status <- ifelse(complete_sens$end_point < complete_sens$study_end & 
                                 (is.na(complete_sens$death_date) | complete_sens$end_point != as.Date(complete_sens$death_date)), 
                               "event", "censored")

#* 2.5 Making subsets for psychiatric service use prior to T2DM (for stratified analysis) ==============================
#
complete_main_with_pre <- complete_main %>% 
  filter(inpat_pre == "TRUE" | outpat_pre == "TRUE" | both_pre == "TRUE")

complete_main_without_pre <- complete_main %>% 
  filter(none_pre == "TRUE")

complete_sens_with_pre <- complete_sens %>% 
  filter(inpat_pre == "TRUE" | outpat_pre == "TRUE" | both_pre == "TRUE")

complete_sens_without_pre <- complete_sens %>% 
  filter(none_pre == "TRUE")

#* 2.7 Creating survival objects =======================================================================================
#
surv_object_main <- complete_main %$% 
  Surv(time_years,status=="event")

surv_object_main_with_pre <- complete_main_with_pre %$% 
  Surv(time_years,status=="event")

surv_object_main_without_pre <- complete_main_without_pre %$% 
  Surv(time_years,status=="event")

surv_object_sens <- complete_sens %$% 
  Surv(time_years,status=="event")

surv_object_sens_with_pre <- complete_sens_with_pre %$% 
  Surv(time_years,status=="event")

surv_object_sens_without_pre <- complete_sens_without_pre %$% 
  Surv(time_years,status=="event")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. OUTPUT DISPLAY ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This section generates the baseline characteristics table, median ages for all groups, median follow-up time and 
# number of events after diabetes diagnosis.
#
#* 3.1 Baseline characteristics (Table 1) ==============================================================================
#
{                            # COMPILE HERE TO GENERATE BASELINE CHARACTERISTICS TABLE (TABLE 1)
#
#** 3.1.1 Splitting subset_main according to history of psychiatric service use prior to diabetes diagnosis ------------
#
# subset_main is being used instead of complete_main so that missing proportions could also be included in table 1.
#
subset_main_with_pre <- subset_main %>% 
  filter(inpat_pre == "TRUE" | outpat_pre == "TRUE" | both_pre == "TRUE")

subset_main_without_pre <- subset_main %>% 
  filter(none_pre == "TRUE")

#** 3.1.2 Creating a function to calculate counts and percentages, including for missing values ------------------------
#
get_n_percent <- function(data, var, missing_label = "Missing") {
  
  data <- data %>%
    mutate(temp_var = fct_explicit_na({{ var }}, na_level = missing_label))
  
  data %>%
    count(temp_var, .drop = FALSE) %>%
    mutate(
      percent = n / sum(n) * 100,
      formatted = glue("{n} ({round(percent, 1)})")
    ) %>%
    select(Level = temp_var, formatted)
}

#** 3.1.3 Create a function to make an easily replicable baseline table ------------------------------------------------
#
make_summary_table <- function(df) {
  age_tbl <- get_n_percent(df, age_cat)                   # There are no missing values for age and sex
  sex_tbl <- get_n_percent(df, sex)
  simd_tbl <- get_n_percent(df, simd) %>%
    mutate(Level = if_else(Level == "Missing", "Missing_simd", Level))
  eth_tbl <- get_n_percent(df, ethnicity) %>%
    mutate(Level = if_else(Level == "Missing", "Missing_eth", Level))
  
  make_section <- function(title, tbl) {
    bind_rows(tibble(Level = title, formatted = NA_character_), tbl)
  }
  
  bind_rows(
    make_section("Age", age_tbl),
    make_section("Sex", sex_tbl),
    make_section("SIMD quintile", simd_tbl),
    make_section("Ethnicity", eth_tbl)
  )
}

#** 3.1.4 Creating tables for each dataset (total, with_pre, without_pre) ----------------------------------------------
#
tbl_subset_main <- make_summary_table(subset_main) %>%
  distinct(Level, .keep_all = TRUE) %>%
  rename(Subset = formatted)

tbl_with_pre <- make_summary_table(subset_main_with_pre) %>%
  distinct(Level, .keep_all = TRUE) %>%
  rename(With_Pre = formatted)

tbl_without_pre <- make_summary_table(subset_main_without_pre) %>%
  distinct(Level, .keep_all = TRUE) %>%
  rename(Without_Pre = formatted)

#** 3.1.5 Defining ordering of variables in table ----------------------------------------------------------------------
#
age_levels <- c("18-39", "40-49", "50-59", "60-69", "70+")
sex_levels <- c("Male", "Female")
simd_levels <- c("1st", "2nd", "3rd", "4th", "5th", "Missing_simd")
ethnicity_levels <- c("White", "Black", "South Asian", "Mixed", "Other ethnic group", "Missing_eth")

#** 3.1.6 Combining all tables and printing the final table ------------------------------------------------------------
#
baseline_table <- full_join(tbl_subset_main, tbl_with_pre, by = "Level") %>%
  full_join(tbl_without_pre, by = "Level") %>%
  mutate(Level = as.character(Level)) %>%
  arrange(match(Level, c(
    "Age", age_levels,
    "Sex", sex_levels,
    "SIMD quintile", simd_levels,
    "Ethnicity", ethnicity_levels
  )))

baseline_table <- baseline_table %>%
  select(Level, Subset, Without_Pre, With_Pre)

print(baseline_table, n=Inf)
}

#* 3.2 Median ages for total, and groups with/without prior psychiatric service use history ============================
#
{                           # COMPILE HERE TO PRINT TABLE
#
# Defining variable "include" so that minimum/maximum ages are excluded. This is done to prevent possible identification
# of individuals with maximum/minimum ages.
# 
include <- c("1st Qu.", "Median", "Mean", "3rd Qu.")

s1 <- round(summary(complete_main$age)[include], 2)
s2 <- round(summary(complete_main_without_pre$age)[include], 2)
s3 <- round(summary(complete_main_with_pre$age)[include], 2)

summary <- rbind(
  All = as.numeric(s1),
  Without_Pre = as.numeric(s2),
  With_Pre = as.numeric(s3))

colnames(summary) <- include
age_table <- as.data.frame(summary)
print(age_table)

rm(include,s1,s2,s3,summary)
}

#* 3.3 Median follow-up time in years ==================================================================================
#
{                           # COMPILE HERE TO PRINT SUMMARY
# 
summary_years <- summary(round(complete_main$time_years, 1))
print(summary_years)
}

#* 3.4 Total event counts after diabetes diagnosis =====================================================================
#
{                           # COMPILE HERE
# 
s1 <- summary(complete_main$inpat_post_only)
s2 <- summary(complete_main$outpat_post_only)
s3 <- summary(complete_main$both_post)
s4 <- summary(complete_main$none_post)

summary <- rbind(
  Inpatient_post_only = as.numeric(s1),
  Outpatient_post_only = as.numeric(s2),
  Both_post = as.numeric(s3),
  Non_post = as.numeric(s4)
  )

colnames(summary) <- names(s1)
events_table <- as.data.frame(summary)
print(events_table)

rm(s1,s2,s3,s4,summary)
}

# Please check script 04_export.R for export options.
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~