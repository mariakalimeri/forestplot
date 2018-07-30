# Add Biomarker Category Rows in a beta, se or pval Dataframe
#
# This function reads three data_frames, a biomarker association data_frame, its corresponding standard error data_frame and its corresponding p-value data_frame and groups them to predefined catgories by sorting and adding rows above each category.
#
#
# @param beta A data frame (either tibble or not) containing named columns in the following way: the first column must be the exact abbreviations of the NMR biomarkers. This column must be named "abbrev". The rest of the columns, the study columns, must contain associations with whatever the name of the study is, e.g. if the column contains the univariate associations of the biomarkers to BMI, the column can be named BMI. Use more than one study columns if you want to plot more than one studies in the same file. Avoid plotting more than 5 or 6 studies cause the result is not pretty and/or may be hard to read. It is important that the columns have all either linear associations or odds/hazard ratios. The reason is that odds/hazard ratios are plotted on a log axis, whereas linear associations not.
# @param se A data frame (either \code{\link{tibble}} or not) in the same format as beta parameter. Keep the same order of columns and preferably the same order of rows (although the latter is not necessary).
# @param pval A data frame (either \code{\link{tibble}} or not) in the same format as beta and se parameters. Keep the same order of columns and preferably the same order of rows (although the latter is not necessary).
# @param biomarker_groups_as_list A named list of character vactors containing the groups of biomarkers to plot (see examples). The category names, i.e. the names of the components of the list, can be anything. The actual character vectors must contain the exact biomarker abbreviations. See Results.tsv or Results.xlsx files or the built-in dataset biomarkers$abbrev.
# @author Maria Kalimeri


add_bmr_group_names <- function(beta, se, pval, biomarker_groups_as_list){

  # Check that all beta, se and pval have a column named abbrev
  if (names(beta)[1]!="abbrev"){
    stop("The first column/variable of beta must be named 'abbrev' and must
          contain the exact abbreviations of the NMR biomarkers (see built-in
          biomarkers dataset).")
  }
  if (names(se)[1]!="abbrev"){
    stop("The first column/variable of se must be named 'abbrev' and must
          contain the exact abbreviations of the NMR biomarkers (see built-in
         biomarkers dataset).")
  }
  if (names(pval)[1]!="abbrev"){
    stop("The first column/variable of pval must be named 'abbrev' and must
          contain the exact abbreviations of the NMR biomarkers (see built-in
         biomarkers dataset).")
  }

  # Checks that beta, se and pval dataframes have same structure/dims
  # rows
  if (nrow(beta) != nrow(se) | nrow(beta) != nrow(pval) | nrow(se) != nrow(pval)){
    stop("beta, se, and pval must have same number of rows and the same 'abbrev' column.")
  }
  if (outersect(beta$abbrev, se$abbrev) %>% length() != 0){
    stop("beta, se, and pval must have same number of rows and same rownames")
  }
  if (outersect(beta$abbrev, pval$abbrev) %>% length() != 0){
    stop("beta, se, and pval must have same number of rows and same rownames")
  }
  if (outersect(se$abbrev, pval$abbrev) %>% length() != 0){
    stop("beta, se, and pval must have same number of rows and same rownames")
  }
  # cols
  if (ncol(beta) != ncol(se) | ncol(beta) != ncol(pval) | ncol(se) != ncol(pval)){
    stop("beta, se, and pval must have same number of columns and same colnames")
  }
  if (outersect(colnames(beta), colnames(se)) %>% length() != 0){
    stop("beta, se, and pval must have same number of columns and same colnames")
  }
  if (outersect(colnames(beta), colnames(pval)) %>% length() != 0){
    stop("beta, se, and pval must have same number of columns and same colnames")
  }
  if (outersect(colnames(se), colnames(pval)) %>% length() != 0){
    stop("beta, se, and pval must have same number of columns and same colnames")
  }
  # Extra check that the abbreviations given are in the standard format
  # # Use built-in biomarkers dataset for the check
  if (setdiff(beta$abbrev, biomarkers$abbrev) %>% length() != 0){
    stop(paste("All abbreviations must be a subset of biomarkers$abbrev! Currently the abbreviations in the beta dataframe contain the following non standard names:",
               paste(setdiff(beta$abbrev, biomarkers$abbrev), collapse = ", ")))
  }
  if (setdiff(se$abbrev, biomarkers$abbrev) %>% length() != 0){
    stop(paste("All abbreviations must be a subset of biomarkers$abbrev! Currently the abbreviations in the se dataframe contain the following non standard names:",
               setdiff(se$abbrev, biomarkers$abbrev),
               "Please correct, so that all abbreviations are a subset of biomarkers$abbrev"))
  }
  if (setdiff(pval$abbrev, biomarkers$abbrev) %>% length() != 0){
    stop(paste("All abbreviations must be a subset of biomarkers$abbrev! Currently the abbreviations in the pval dataframe contain the following non standard names:",
               setdiff(pval$abbrev, biomarkers$abbrev),
               "Please correct, so that all abbreviations are a subset of biomarkers$abbrev"))
  }

  # Initialize the beta, se and pval grouped objects to be built
  beta_grouped <-
    beta[0,]
  se_grouped <-
    se[0,]
  pval_grouped <-
    pval[0,]

  # A row that will be added above every group of biomarkers
  title_row_of_na <- beta[0,]
  title_row_of_na[1,] <- rep(NA, ncol(beta))

  # Loop over all biomarker groups and create the new orgered and grouped dataframes
  for (i in 1:length(biomarker_groups_as_list)){
    beta_grouped <-
      beta_grouped %>%
      rbind(., title_row_of_na) %>%
      rbind(., beta[match(biomarker_groups_as_list[[i]], beta$abbrev),])
    se_grouped <-
      se_grouped %>%
      rbind(., title_row_of_na) %>%
      rbind(., se[match(biomarker_groups_as_list[[i]], se$abbrev),])
    pval_grouped <-
      pval_grouped %>%
      rbind(., title_row_of_na) %>%
      rbind(., pval[match(biomarker_groups_as_list[[i]], pval$abbrev),])
  }

  # Add the names of the groups in the rows that have NAs. Caution: In case of
  # subsequent NA rows, keep only the last row as the title one.
  rows_for_group_names <- NULL
  for (rowno in 1:nrow(beta_grouped)){
    # This is a titlename row only if the subsequent row's abbrev is not NA
    if (is.na(beta_grouped$abbrev[rowno]) &&
        !is.na(beta_grouped$abbrev[rowno+1])){
        rows_for_group_names <- c(rows_for_group_names, rowno)
    }
  }

  beta_grouped$abbrev[rows_for_group_names] <-
    se_grouped$abbrev[rows_for_group_names] <-
    pval_grouped$abbrev[rows_for_group_names] <-
    names(biomarker_groups_as_list)

  return(list(beta_grouped=beta_grouped,
              se_grouped=se_grouped,
              pval_grouped=pval_grouped))
}

