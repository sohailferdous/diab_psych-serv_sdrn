# This script accompanies the study: Sociodemographic risk factors for psychiatric service use among newly-diagnosed 
# adults with type 2 diabetes in Scotland
#
# Data-analysis was conducted using SDRN-NDS data within the diabepi safe haven. File paths within the haven have been 
# redacted.
# 
# Script: 04_export.R: This script exports results from the other three scripts.
#
# Please ensure you have compiled scripts 01_data-prep.R, 02_survival-analysis.R and 03_supplementary before running 
# this script.
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01_data-prep.R ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# This section exports table 1 (baseline characteristics), median ages for study population, median follow-up times, and
# total number of events occurred.
# 
{                                 # COMPILE HERE TO EXPORT ONE GO
#
sink("filepath/filename.txt")

cat("\nMain table\n\n")
print(baseline_table, n=Inf)

cat("\n=================================\n")
cat("\nAge statistics by prior history\n\n")
print(age_table)

cat("\n=================================\n")
cat("\nFollow-up time in years\n\n")
print(summary_years)

cat("\n=================================\n")
cat("\nNumber of events during study period\n\n")
print(events_table)

sink (file = NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02_survival_analysis.R ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# Figure 1A-1D KM plots (PDF AND PNG ) =================================================================================
#
# This section exports each KM plot separately in PDF and PNG format, followed by combining them and exporting the
# combined plot.
# 
{                     # Compile here to generate all KM plots in png and pdf formats in one go
#
p1A_plot <- survplot_sex$plot
p1A_table <- survplot_sex$table
p1B_plot <- survplot_age_cat$plot
p1B_table <- survplot_age_cat$table
p1C_plot <- survplot_simd$plot
p1C_table <- survplot_simd$table
p1D_plot <- survplot_ethnicity$plot
p1D_table <- survplot_ethnicity$table

#** 1A - sex -----------------------------------------------------------------------------------------------------------
#
combined <- ggarrange(p1A_plot, p1A_table,
                      ncol = 1,
                      heights = c(2, 1))

ggsave("filepath/filename.png", plot = combined,
       width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf", plot = combined,
       width = 12, height = 9, dpi = 600)

rm(combined)

#** 1B - age_cat -------------------------------------------------------------------------------------------------------
#
combined <- ggarrange(p1B_plot, p1B_table,
                      ncol = 1,
                      heights = c(2, 1))

ggsave("filepath/filename.png", plot = combined,
       width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf", plot = combined,
       width = 12, height = 9, dpi = 600)

rm(combined)

#** 1C - simd ----------------------------------------------------------------------------------------------------------
#
combined <- ggarrange(p1C_plot, p1C_table,
                      ncol = 1,
                      heights = c(2, 1))

ggsave("filepath/filename.png", plot = combined,
       width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf", plot = combined,
       width = 12, height = 9, dpi = 600)

rm(combined)

#** 1D - ethnicity -----------------------------------------------------------------------------------------------------
#
combined <- ggarrange(p1D_plot, p1D_table,
                      ncol = 1,
                      heights = c(2, 1))

ggsave("filepath/filename.png", plot = combined,
       width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf", plot = combined,
       width = 12, height = 9, dpi = 600)

rm(combined)

#** Combined 1A-1D plots in a 2x2 layout -------------------------------------------------------------------------------
#
combined <- ggarrange(p1A_plot, p1B_plot, p1C_plot, p1D_plot,
                          ncol = 2, nrow = 2,
                          labels = c("A", "B", "C", "D"))

ggsave("filepath/filename.png", plot = combined,
       width = 12, height = 11, dpi = 600)

ggsave("filepath/filename.pdf", plot = combined,
       width = 12, height = 11, dpi = 600)

rm(combined, p1A_plot, p1A_table, p1B_plot, p1B_table, p1C_plot, p1C_table, p1D_plot, p1D_table)
}

# Figure 2 - Cox model forest plot =====================================================================================
# 
{
#
# Design 1 -------------------------------------------------------------------------------------------------------------
# 
ggsave("filepath/filename.png", plot=figure_2_faceted,
       width = 10, height = 11, dpi = 600)

ggsave("filepath/filename.pdf", plot=figure_2_faceted,
       width = 10, height = 11, dpi = 600)

# Design 2
# 
ggsave("filepath/filename", plot=figure_2_basic,
       width = 10, height = 17, dpi = 600)

ggsave("filepath/filename", plot=figure_2_basic,
       width = 10, height = 17, dpi = 600)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03_supplementary.R ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Sensitivity analysis estimates =======================================================================================
# 
{                                       # COMPILE HERE
#
sink("filepath/filename.txt")

cat("\nSensitivity analysis results\n\n")
print(sens_table, row.names = F, n=Inf)

sink (file = NULL)
}

# Missingness data =====================================================================================================
# 
{                                       # COMPILE HERE
#
sink("filepath/filename.txt")

cat("\nHypothesis tests to check randomness of missing values\n\n")
cat("\n=================================\n")

cat("\nt-test for continuous age\n\n")
print(t_test_age)

cat("\n=================================\n")
cat("\nChi-squared test for categorical age\n\n")
print(age_table)
print(chisq_test_age)

cat("\n=================================\n")
cat("\nChi-squared test for sex\n\n")
print(sex_table)
print(chisq_test_sex)

cat("\n=================================\n")
cat("\nChi-squared test for simd quintiles\n\n")
print(simd_table)
print(chisq_test_simd)

cat("\n=================================\n")
cat("\nChi-squared test for ethnicity variable\n\n")
print(ethnicity_table)
print(chisq_test_ethnicity)

sink (file = NULL)
}

# Proportional hazard assumption testing ===============================================================================
# 
# This section exports Schoenfeld residual plots for all main analysis models. For univariable models, plots are
# exported individually, as well as combined. All exports are in PNG and PDF formats.
# 
# [[1]] since the plot is the first element in the ggcoxzph object
# 
{                                       # COMPILE HERE TO EXPORT ALL PLOTS
#
# Univariable models - No history of prior psychiatric service use -----------------------------------------------------
# 
# Age
# 
ggsave("filepath/filename.png", 
       plot = sch_uni_without_age[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_without_age[[1]], width = 12, height = 9, dpi = 600)


# Sex
# 
ggsave("filepath/filename.png", 
       plot = sch_uni_without_sex[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf", 
       plot = sch_uni_without_sex[[1]], width = 12, height = 9, dpi = 600)

# SIMD
# 
ggsave("filepath/filename.png",
       plot = sch_uni_without_simd[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_without_simd[[1]], width = 12, height = 9, dpi = 600)

# Ethnicity
# 
ggsave("filepath/filename.png",
       plot = sch_uni_without_ethnicity[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_without_ethnicity[[1]], width = 12, height = 9, dpi = 600)

# Combining
# 
sch_uni_without_combined <- ggarrange(sch_uni_without_age[[1]], sch_uni_without_sex[[1]], 
                                      sch_uni_without_simd[[1]], sch_uni_without_ethnicity[[1]], 
                                      ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

sch_uni_without_combined <- annotate_figure(sch_uni_without_combined,
                                         top = text_grob("Schoenfeld residuals - univariable models (without prior psychiatric service use)",
                                                         face = "bold", size = 15))

ggsave("filepath/filename.png",
       plot = sch_uni_without_combined, width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_without_combined, width = 12, height = 9, dpi = 600)

# Univariable models - History of prior psychiatric service use --------------------------------------------------------
# 
# Age
# 
ggsave("filepath/filename.png", 
       plot = sch_uni_with_age[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_with_age[[1]], width = 12, height = 9, dpi = 600)

# Sex
# 
ggsave("filepath/filename.png", 
       plot = sch_uni_with_sex[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf", 
       plot = sch_uni_with_sex[[1]], width = 12, height = 9, dpi = 600)

# SIMD
# 
ggsave("filepath/filename.png",
       plot = sch_uni_with_simd[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_with_simd[[1]], width = 12, height = 9, dpi = 600)

# Ethnicity
# 
ggsave("filepath/filename.png",
       plot = sch_uni_with_ethnicity[[1]], width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_with_ethnicity[[1]], width = 12, height = 9, dpi = 600)

# Combining
# 
sch_uni_with_combined <- ggarrange(sch_uni_with_age[[1]], sch_uni_with_sex[[1]], 
                                      sch_uni_with_simd[[1]], sch_uni_with_ethnicity[[1]], 
                                      ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

sch_uni_with_combined <- annotate_figure(sch_uni_with_combined,
                                           top = text_grob("Schoenfeld residuals - univariable models (with prior psychiatric service use)",
                                                           face = "bold", size = 15))

ggsave("filepath/filename.png",
       plot = sch_uni_with_combined, width = 12, height = 9, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_uni_with_combined, width = 12, height = 9, dpi = 600)

# Multivariable models - No history of prior psychiatric service use ---------------------------------------------------
# 
sch_multi_without_combined <- ggarrange(sch_multi_without[[1]], sch_multi_without[[2]], 
                                     sch_multi_without[[3]], sch_multi_without[[4]], 
                                     ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

sch_multi_without_combined <- annotate_figure(sch_multi_without_combined,
                                           top = text_grob("Schoenfeld residuals - multivariable model (without prior psychiatric service use)",
                                                           face = "bold", size = 15))

ggsave("filepath/filename.png",
       plot = sch_multi_without_combined, width = 12, height = 11, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_multi_without_combined, width = 12, height = 11, dpi = 600)

# Multivariable models - History of prior psychiatric service use ------------------------------------------------------
# 
sch_multi_with_combined <- ggarrange(sch_multi_with[[1]], sch_multi_with[[2]], 
                                     sch_multi_with[[3]], sch_multi_with[[4]], 
                                   ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

sch_multi_with_combined <- annotate_figure(sch_multi_with_combined,
                                           top = text_grob("Schoenfeld residuals - multivariable model (with prior psychiatric service use)",
                                                           face = "bold", size = 15))

ggsave("filepath/filename.png",
       plot = sch_multi_with_combined, width = 12, height = 11, dpi = 600)

ggsave("filepath/filename.pdf",
       plot = sch_multi_with_combined, width = 12, height = 11, dpi = 600)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~