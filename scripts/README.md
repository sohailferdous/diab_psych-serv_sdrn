<p>This folder contains all R scripts associated with the study. All analyses were conducted using Scottish Diabetes Research Network - National Diabetes Dataset (SDRN-NDS) dataset, within the diabepi safe haven. All filepaths and filenames within the safe haven have been redacted.</p>

<p>Cohort data from the SDRN-NDS dataset was linked and prepared by K.J.F., and the study analysis was performed by S.F.</p>

### Script descriptions
<p><b>01_data-prep.R:</b> This script prepares data for survival analysis (main and sensitivity analysis), and generates descriptive population characterisics including baseline table, median ages for study population, median follow-up times, and total event counts.</p>
<p><b>02_survival-analysis.R:</b> This script performs the main survival analysis, and generates Kaplan-Meier (KM) plots, and forest plots depicting Cox model estimates for main analysis.</p>
<p><b>03_supplementary.R:</b> This script performs sensitivity COX analysis, explores missingness by performing hypothesis tests between complete and missing subsets, and checks proportional hazards assumption by generating Schoenfeld residual plots.</p>
<p><b>04_export.R:</b> This script exports results from the other three scripts.</p>
