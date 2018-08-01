context("forestplot_nmr")


test_that("plots tests case 1", {
  bmr_all_grouped <- bmr_selected_grouping(bmr_grouping_choice = "serum_all")

  forestplot_nmr(beta=demo_beta,
                 se=demo_se,
                 pval=demo_pval,
                 biomarker_groups_as_list=bmr_all_grouped,
                 filename='TEMP1_plot_pkgtesting.pdf',
                 plot_title="Linear associations to BMI",
                 is_log_odds_ratio=FALSE,
                 xlabel="SD difference (95% CI)",
                 signif_cutoff=0.05,
                 legend_vars=names(demo_beta)[2:3],
                 paper="a4",
                 height = 12,
                 width = 9)
  expect_true(
    "TEMP1_plot_pkgtesting.pdf" %in% list.files()
  )
  file.remove("TEMP1_plot_pkgtesting.pdf")
})


test_that("plots tests case 2", {
  abbrev <- c("LDL-C", "HDL-C", "Serum-C")
  b <- data.frame(
    abbrev = abbrev,
    rs12 = c(1, -0.5, 0.8),
    stringsAsFactors = F
  )
  s <- data.frame(
    abbrev = abbrev,
    rs12 = c(0.2, 0.3, 0.1),
    stringsAsFactors = F
  )
  p <- data.frame(
    abbrev = abbrev,
    rs12 = c(0.0001, 0.2, 0.3),
    stringsAsFactors = F
  )
  bm_groups <- list("Clinical" = abbrev, "CRASH" = c("NOT_IN_DATA", "LDL-C"))

  forestplot_nmr(beta = b,
                 se = s,
                 pval = p,
                 biomarker_groups_as_list = bm_groups,
                 filename = "TEMP2_plot_pkgtesting.pdf")

  expect_true(
    "TEMP2_plot_pkgtesting.pdf" %in% list.files()
  )
  file.remove("TEMP2_plot_pkgtesting.pdf")
})
