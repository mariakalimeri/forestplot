#' Generate Ordered, Grouped Subsets of Selected Biomarkers
#'
#' This function generates ordered, grouped subsets of selected biomarkers in
#' the form of named lists.
#'
#' The function currently generates 3 different groupings. The need for these
#' specific groupings and orderings came from the need to generate different
#' versions of forest plots. The categories "serum_all" and "edta_plasma_all"
#' are expected to be the most useful as they group all the blood biomarkers
#' for either serum (228 biomarkers) or plasma (225 biomarkers) samples. The
#' other category "example_short" is meant as an example case.
#'
#' @param bmr_grouping_choice A string. Must be one of the following 3:
#' "serum_all", "edta_plasma_all", "example_short".
#' @return A named list of selected biomarkers in a predefined order
#' @keywords biomarker categories
#' @author Maria Kalimeri
#' @export
#' @examples
#' # Get the grouped biomarkers
#' biomarker_groups <- bmr_selected_grouping("edta_plasma_all")

bmr_selected_grouping <- function(bmr_grouping_choice = c("serum_all",
                                                          "edta_plasma_all",
                                                          "example_short")) {
  if (!bmr_grouping_choice %in% c("serum_all",
                                  "edta_plasma_all",
                                  "example_short")) {
    stop(
      "bmr_grouping_choice variable must be one of 'serum_all', 'edta_plasma_all', 'example_short'"
    )
  }

  val <- switch(
    bmr_grouping_choice,
    serum_all = list(
      "Branched-chain amino acids" = c("Ile", "Leu", "Val"),
      "Aromatic amino acids" = c("Phe", "Tyr", "His"),
      "Other amino acids" = c("Ala", "Gln", "Gly"),
      "Ketone bodies" = c("AcAce", "bOHBut", NA),
      "Miscellaneous" = c("Crea", "Alb", "Ace", "Cit", NA),
      "Inflammation" = c("Gp", NA),
      "Fatty acids" = c(
        "TotFA",
        "UnSat",
        "SFA",
        "MUFA",
        "PUFA",
        "FAw6",
        "LA",
        "FAw3",
        "DHA",
        NA
      ),
      "Total cholesterol in lipoproteins" = c(
        "XXL-VLDL-C",
        "XL-VLDL-C",
        "L-VLDL-C",
        "M-VLDL-C",
        "S-VLDL-C",
        "XS-VLDL-C",
        "IDL-C",
        "L-LDL-C",
        "M-LDL-C",
        "S-LDL-C",
        "XL-HDL-C",
        "L-HDL-C",
        "M-HDL-C",
        "S-HDL-C"
      ),
      "Triglycerides in lipoproteins" = c(
        "XXL-VLDL-TG",
        "XL-VLDL-TG",
        "L-VLDL-TG",
        "M-VLDL-TG",
        "S-VLDL-TG",
        "XS-VLDL-TG",
        "IDL-TG",
        "L-LDL-TG",
        "M-LDL-TG",
        "S-LDL-TG",
        "XL-HDL-TG",
        "L-HDL-TG",
        "M-HDL-TG",
        "S-HDL-TG"
      ),
      "Phospholipids" = c("SM", "TotCho", "PC", "TotPG", "TG/PG", NA),

      "Composite lipid measures" = c(
        "Serum-C",
        "VLDL-C",
        "LDL-C",
        "HDL-C",
        "HDL2-C",
        "HDL3-C",
        "EstC",
        "FreeC",
        "Remnant-C",
        "Serum-TG",
        "VLDL-TG",
        "LDL-TG",
        "HDL-TG",
        NA
      ),
      "Glycolysis-related metabolites" = c("Lac", "Glc", "Pyr", "Glol"),
      "Fatty acid ratios" = c(
        "SFA/FA",
        "MUFA/FA",
        "PUFA/FA",
        "FAw6/FA",
        "LA/FA",
        "FAw3/FA",
        "DHA/FA",
        NA
      ),
      "Total cholesterol in lipoproteins (%)" =
        c(
          "XXL-VLDL-C_%",
          "XL-VLDL-C_%",
          "L-VLDL-C_%",
          "M-VLDL-C_%",
          "S-VLDL-C_%",
          "XS-VLDL-C_%",
          "IDL-C_%",
          "L-LDL-C_%",
          "M-LDL-C_%",
          "S-LDL-C_%",
          "XL-HDL-C_%",
          "L-HDL-C_%",
          "M-HDL-C_%",
          "S-HDL-C_%"
        ),
      "Triglycerides in lipoproteins (%)" = c(
        "XXL-VLDL-TG_%",
        "XL-VLDL-TG_%",
        "L-VLDL-TG_%",
        "M-VLDL-TG_%",
        "S-VLDL-TG_%",
        "XS-VLDL-TG_%",
        "IDL-TG_%",
        "L-LDL-TG_%",
        "M-LDL-TG_%",
        "S-LDL-TG_%",
        "XL-HDL-TG_%",
        "L-HDL-TG_%",
        "M-HDL-TG_%",
        "S-HDL-TG_%"
      ),
      "Free cholesterol in lipoproteins" = c(
        "XXL-VLDL-FC",
        "XL-VLDL-FC",
        "L-VLDL-FC",
        "M-VLDL-FC",
        "S-VLDL-FC",
        "XS-VLDL-FC",
        "IDL-FC",
        "L-LDL-FC",
        "M-LDL-FC",
        "S-LDL-FC",
        "XL-HDL-FC",
        "L-HDL-FC",
        "M-HDL-FC",
        "S-HDL-FC"
      ),
      "Cholesterol esters in lipoproteins" =
        c(
          "XXL-VLDL-CE",
          "XL-VLDL-CE",
          "L-VLDL-CE",
          "M-VLDL-CE",
          "S-VLDL-CE",
          "XS-VLDL-CE",
          "IDL-CE",
          "L-LDL-CE",
          "M-LDL-CE",
          "S-LDL-CE",
          "XL-HDL-CE",
          "L-HDL-CE",
          "M-HDL-CE",
          "S-HDL-CE"
        ),
      "Phospholipids in lipoproteins" = c(
        "XXL-VLDL-PL",
        "XL-VLDL-PL",
        "L-VLDL-PL",
        "M-VLDL-PL",
        "S-VLDL-PL",
        "XS-VLDL-PL",
        "IDL-PL",
        "L-LDL-PL",
        "M-LDL-PL",
        "S-LDL-PL",
        "XL-HDL-PL",
        "L-HDL-PL",
        "M-HDL-PL",
        "S-HDL-PL"
      ),
      "Total lipids in lipoproteins" = c(
        "XXL-VLDL-L",
        "XL-VLDL-L",
        "L-VLDL-L",
        "M-VLDL-L",
        "S-VLDL-L",
        "XS-VLDL-L",
        "IDL-L",
        "L-LDL-L",
        "M-LDL-L",
        "S-LDL-L",
        "XL-HDL-L",
        "L-HDL-L",
        "M-HDL-L",
        "S-HDL-L"
      ),
      "Apolipoproteins" = c("ApoB", "ApoA1", "ApoB/ApoA1"),
      "Free cholesterol in lipoproteins (%)" =
        c(
          "XXL-VLDL-FC_%",
          "XL-VLDL-FC_%",
          "L-VLDL-FC_%",
          "M-VLDL-FC_%",
          "S-VLDL-FC_%",
          "XS-VLDL-FC_%",
          "IDL-FC_%",
          "L-LDL-FC_%",
          "M-LDL-FC_%",
          "S-LDL-FC_%",
          "XL-HDL-FC_%",
          "L-HDL-FC_%",
          "M-HDL-FC_%",
          "S-HDL-FC_%"
        ),
      "Cholesterol esters in lipoproteins (%)" =
        c(
          "XXL-VLDL-CE_%",
          "XL-VLDL-CE_%",
          "L-VLDL-CE_%",
          "M-VLDL-CE_%",
          "S-VLDL-CE_%",
          "XS-VLDL-CE_%",
          "IDL-CE_%",
          "L-LDL-CE_%",
          "M-LDL-CE_%",
          "S-LDL-CE_%",
          "XL-HDL-CE_%",
          "L-HDL-CE_%",
          "M-HDL-CE_%",
          "S-HDL-CE_%"
        ),
      "Phospholipids in lipoproteins (%)" = c(
        "XXL-VLDL-PL_%",
        "XL-VLDL-PL_%",
        "L-VLDL-PL_%",
        "M-VLDL-PL_%",
        "S-VLDL-PL_%",
        "XS-VLDL-PL_%",
        "IDL-PL_%",
        "L-LDL-PL_%",
        "M-LDL-PL_%",
        "S-LDL-PL_%",
        "XL-HDL-PL_%",
        "L-HDL-PL_%",
        "M-HDL-PL_%",
        "S-HDL-PL_%"
      ),
      "Concentration of lipoproteins" = c(
        "XXL-VLDL-P",
        "XL-VLDL-P",
        "L-VLDL-P",
        "M-VLDL-P",
        "S-VLDL-P",
        "XS-VLDL-P",
        "IDL-P",
        "L-LDL-P",
        "M-LDL-P",
        "S-LDL-P",
        "XL-HDL-P",
        "L-HDL-P",
        "M-HDL-P",
        "S-HDL-P"
      ),
      "Lipoprotein particle size" = c("VLDL-D", "LDL-D", "HDL-D")
    ),
    edta_plasma_all =  list(
      "Branched-chain amino acids" = c("Ile", "Leu", "Val"),
      "Aromatic amino acids" = c("Phe", "Tyr", "His"),
      "Other amino acids" = c("Ala", "Gln", NA),
      "Ketone bodies" = c("AcAce", "bOHBut", NA),
      "Miscellaneous" = c("Crea", "Alb", "Ace", "Cit", NA),
      "Inflammation" = c("Gp", NA),
      "Fatty acids" = c(
        "TotFA",
        "UnSat",
        "SFA",
        "MUFA",
        "PUFA",
        "FAw6",
        "LA",
        "FAw3",
        "DHA",
        NA
      ),
      "Total cholesterol in lipoproteins" =
        c(
          "XXL-VLDL-C",
          "XL-VLDL-C",
          "L-VLDL-C",
          "M-VLDL-C",
          "S-VLDL-C",
          "XS-VLDL-C",
          "IDL-C",
          "L-LDL-C",
          "M-LDL-C",
          "S-LDL-C",
          "XL-HDL-C",
          "L-HDL-C",
          "M-HDL-C",
          "S-HDL-C"
        ),
      "Triglycerides in lipoproteins" =
        c(
          "XXL-VLDL-TG",
          "XL-VLDL-TG",
          "L-VLDL-TG",
          "M-VLDL-TG",
          "S-VLDL-TG",
          "XS-VLDL-TG",
          "IDL-TG",
          "L-LDL-TG",
          "M-LDL-TG",
          "S-LDL-TG",
          "XL-HDL-TG",
          "L-HDL-TG",
          "M-HDL-TG",
          "S-HDL-TG"
        ),
      "Phospholipids" = c("SM", "TotCho", "PC", "TotPG", "TG/PG", NA),

      "Composite lipid measures" = c(
        "Serum-C",
        "VLDL-C",
        "LDL-C",
        "HDL-C",
        "HDL2-C",
        "HDL3-C",
        "EstC",
        "FreeC",
        "Remnant-C",
        "Serum-TG",
        "VLDL-TG",
        "LDL-TG",
        "HDL-TG",
        NA
      ),
      "Glycolysis-related metabolites" =
        c("Lac", "Glc", NA, NA),
      "Fatty acid ratios" = c(
        "SFA/FA",
        "MUFA/FA",
        "PUFA/FA",
        "FAw6/FA",
        "LA/FA",
        "FAw3/FA",
        "DHA/FA",
        NA
      ),
      "Total cholesterol in lipoproteins (%)" =
        c(
          "XXL-VLDL-C_%",
          "XL-VLDL-C_%",
          "L-VLDL-C_%",
          "M-VLDL-C_%",
          "S-VLDL-C_%",
          "XS-VLDL-C_%",
          "IDL-C_%",
          "L-LDL-C_%",
          "M-LDL-C_%",
          "S-LDL-C_%",
          "XL-HDL-C_%",
          "L-HDL-C_%",
          "M-HDL-C_%",
          "S-HDL-C_%"
        ),
      "Triglycerides in lipoproteins (%)" =
        c(
          "XXL-VLDL-TG_%",
          "XL-VLDL-TG_%",
          "L-VLDL-TG_%",
          "M-VLDL-TG_%",
          "S-VLDL-TG_%",
          "XS-VLDL-TG_%",
          "IDL-TG_%",
          "L-LDL-TG_%",
          "M-LDL-TG_%",
          "S-LDL-TG_%",
          "XL-HDL-TG_%",
          "L-HDL-TG_%",
          "M-HDL-TG_%",
          "S-HDL-TG_%"
        ),
      "Free cholesterol in lipoproteins" =
        c(
          "XXL-VLDL-FC",
          "XL-VLDL-FC",
          "L-VLDL-FC",
          "M-VLDL-FC",
          "S-VLDL-FC",
          "XS-VLDL-FC",
          "IDL-FC",
          "L-LDL-FC",
          "M-LDL-FC",
          "S-LDL-FC",
          "XL-HDL-FC",
          "L-HDL-FC",
          "M-HDL-FC",
          "S-HDL-FC"
        ),
      "Cholesterol esters in lipoproteins" =
        c(
          "XXL-VLDL-CE",
          "XL-VLDL-CE",
          "L-VLDL-CE",
          "M-VLDL-CE",
          "S-VLDL-CE",
          "XS-VLDL-CE",
          "IDL-CE",
          "L-LDL-CE",
          "M-LDL-CE",
          "S-LDL-CE",
          "XL-HDL-CE",
          "L-HDL-CE",
          "M-HDL-CE",
          "S-HDL-CE"
        ),
      "Phospholipids in lipoproteins" =
        c(
          "XXL-VLDL-PL",
          "XL-VLDL-PL",
          "L-VLDL-PL",
          "M-VLDL-PL",
          "S-VLDL-PL",
          "XS-VLDL-PL",
          "IDL-PL",
          "L-LDL-PL",
          "M-LDL-PL",
          "S-LDL-PL",
          "XL-HDL-PL",
          "L-HDL-PL",
          "M-HDL-PL",
          "S-HDL-PL"
        ),
      "Total lipids in lipoproteins" =
        c(
          "XXL-VLDL-L",
          "XL-VLDL-L",
          "L-VLDL-L",
          "M-VLDL-L",
          "S-VLDL-L",
          "XS-VLDL-L",
          "IDL-L",
          "L-LDL-L",
          "M-LDL-L",
          "S-LDL-L",
          "XL-HDL-L",
          "L-HDL-L",
          "M-HDL-L",
          "S-HDL-L"
        ),
      "Apolipoproteins" = c("ApoB", "ApoA1", "ApoB/ApoA1"),
      "Free cholesterol in lipoproteins (%)" =
        c(
          "XXL-VLDL-FC_%",
          "XL-VLDL-FC_%",
          "L-VLDL-FC_%",
          "M-VLDL-FC_%",
          "S-VLDL-FC_%",
          "XS-VLDL-FC_%",
          "IDL-FC_%",
          "L-LDL-FC_%",
          "M-LDL-FC_%",
          "S-LDL-FC_%",
          "XL-HDL-FC_%",
          "L-HDL-FC_%",
          "M-HDL-FC_%",
          "S-HDL-FC_%"
        ),
      "Cholesterol esters in lipoproteins (%)" =
        c(
          "XXL-VLDL-CE_%",
          "XL-VLDL-CE_%",
          "L-VLDL-CE_%",
          "M-VLDL-CE_%",
          "S-VLDL-CE_%",
          "XS-VLDL-CE_%",
          "IDL-CE_%",
          "L-LDL-CE_%",
          "M-LDL-CE_%",
          "S-LDL-CE_%",
          "XL-HDL-CE_%",
          "L-HDL-CE_%",
          "M-HDL-CE_%",
          "S-HDL-CE_%"
        ),
      "Phospholipids in lipoproteins (%)" =
        c(
          "XXL-VLDL-PL_%",
          "XL-VLDL-PL_%",
          "L-VLDL-PL_%",
          "M-VLDL-PL_%",
          "S-VLDL-PL_%",
          "XS-VLDL-PL_%",
          "IDL-PL_%",
          "L-LDL-PL_%",
          "M-LDL-PL_%",
          "S-LDL-PL_%",
          "XL-HDL-PL_%",
          "L-HDL-PL_%",
          "M-HDL-PL_%",
          "S-HDL-PL_%"
        ),
      "Concentration of lipoproteins" =
        c(
          "XXL-VLDL-P",
          "XL-VLDL-P",
          "L-VLDL-P",
          "M-VLDL-P",
          "S-VLDL-P",
          "XS-VLDL-P",
          "IDL-P",
          "L-LDL-P",
          "M-LDL-P",
          "S-LDL-P",
          "XL-HDL-P",
          "L-HDL-P",
          "M-HDL-P",
          "S-HDL-P"
        ),
      "Lipoprotein particle size" = c("VLDL-D", "LDL-D", "HDL-D")
    ),
    example_short = list(
      "Lipids and triglycerides" = c("Serum-C",
                                     "LDL-C",
                                     "HDL-C",
                                     "VLDL-C",
                                     "Serum-TG"),
      "Apolipoproteins" = c("ApoB",
                            "ApoA1",
                            "ApoB/ApoA1"),
      "Glycolysis and other" = c("Glc",
                                 "Lac",
                                 "Pyr",
                                 "Glol",
                                 "Crea",
                                 "Gp"),
      "Amino acids" = c("Ile",
                        "Leu",
                        "Val",
                        "Phe",
                        "Tyr",
                        "His",
                        "Ala",
                        "Gly",
                        "Gln"),
      "Fatty acid ratios" = c("SFA/FA",
                              "MUFA/FA",
                              "FAw6/FA",
                              "LA/FA",
                              "FAw3/FA",
                              "DHA/FA")
    )
  )
  return(val)
}
