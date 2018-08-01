context("add_bmr_group_names")

serum_all_grouping <- bmr_selected_grouping("serum_all")
edta_plasma_grouping <- bmr_selected_grouping("edta_plasma_all")
example_short_grouping <- bmr_selected_grouping("example_short")


test_that("the beta grouped dataframe with serum_all grouping names has the expected length", {
  expect_equal(
    add_bmr_group_names(demo_beta, demo_se, demo_pval, serum_all_grouping) %>%
      .$beta_grouped %>%
      nrow,
    228 +
      serum_all_grouping %>% unlist %>% is.na %>% sum +
      serum_all_grouping %>% length
  )
})

test_that("the se grouped dataframe with edta_plasma_all grouping has the expected length", {
  expect_equal(
    add_bmr_group_names(demo_beta, demo_se, demo_pval, edta_plasma_grouping) %>%
      .$se_grouped %>%
      nrow,
    225 +
      edta_plasma_grouping %>% unlist %>% is.na %>% sum +
      edta_plasma_grouping %>% length
  )
})

test_that("the pval grouped dataframe with example_short grouping has the expected length", {
  expect_equal(
    add_bmr_group_names(demo_beta, demo_se, demo_pval, example_short_grouping) %>%
      .$pval_grouped %>%
      nrow,
    29 +
      example_short_grouping %>% unlist %>% is.na %>% sum +
      example_short_grouping %>% length
  )
})
