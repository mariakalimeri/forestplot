#' NMR-quantified Biomarker Names and Abbreviations
#'
#' A dataframe containing the abbreviations and spelled-out names of 228 NMR-quantified biomarkers.
#'
#' The main difference between forest_plot_disp_name1 and forest_plot_disp_name2 is how the names of the lipoprotein subclasses are displayed. For example, option 2 will display XXL-VLDL-TG % for the ratio of triglycerides in XXL VLDL particles, whereas option 1 assumes that plotting will be done according to lipid type, e.g. all triglycerides plotted in the same subgroup, therefore it would only display "Extremely large VLDL" (under the category "Triglycerides in lipoproteins").
#'
#' @format A data frame (tibble) with 228 rows and 4 columns:
#' \describe{
#'   \item{abbrev}{Biomarker abbreviation}
#'   \item{fullname}{Biomarker full name}
#'   \item{forest_plot_disp_name1}{Biomarker display name option 1 (for forest plot purposes)}
#'   \item{forest_plot_categories}{Suggestive biomarker category (for forest plot purposes)}
#'   \item{forest_plot_disp_name2}{Biomarker display name option 2 (for forest plot purposes)}
#' }
"biomarkers"


#' A Palette of 5 Colors
#'
#' A named vector containing the hex numbers of 5 colors.
"fpcolors"

#' Linear Associations of NMR-quantified Biomarkers to BMI
#'
#' A dataframe containing linear associations of NMR-quantified biomarkers to BMI as estimated using simulated data.
#'
#' @format A data frame (tibble) with 228 rows and 2 columns:
#' \describe{
#'   \item{abbrev}{Biomarker abbreviation}
#'   \item{cohort1}{Association values for simulated cohort 1}
#'   \item{cohort2}{Association values for simulated cohort 2}
#' }
"demo_beta"

#' Standard Error Values of Linear Associations of NMR-quantified Biomarkers to BMI
#'
#' A dataframe containing standard error values for linear associations of NMR-quantified biomarkers to BMI as estimated using simulated data. Std. error values correspond to the demo_beta dataframe values
#'
#' @format A data frame (tibble) with 228 rows and 2 columns:
#' \describe{
#'   \item{abbrev}{Biomarker abbreviation}
#'   \item{cohort1}{Std. error values for simulated cohort 1}
#'   \item{cohort2}{Std. error values for simulated cohort 2}
#' }
"demo_se"

#' P-values of Linear Associations of NMR-quantified Biomarkers to BMI
#'
#' A dataframe containing p-values for linear associations of NMR-quantified biomarkers to BMI as estimated using simulated data. p-values correspond to the demo_beta and demo_se dataframe values
#'
#' @format A data frame (tibble) with 228 rows and 2 columns:
#' \describe{
#'   \item{abbrev}{Biomarker abbreviation}
#'   \item{cohort1}{p-values for simulated cohort 1}
#'   \item{cohort2}{p-values for simulated cohort 2}
#' }
"demo_pval"
