## Test Setup
kri_workflows <- MakeWorkflowList(c("kri0005", "cou0005"), strPackage = "gsm.kri")
kri_custom <- MakeWorkflowList(c("kri0005_custom", "cou0005_custom"), yaml_path_custom_metrics, strPackage = "gsm.qc")

## Test Code
testthat::test_that("Labs Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.", {
  ## regular -----------------------------------------
  test <- map(kri_workflows, ~ robust_runworkflow(.x, mapped_data, steps = 1:6))

  # grouping col in yaml file is interpreted correctly in dfInput GroupID
  iwalk(test, ~ expect_identical(
    sort(unique(.x$Analysis_Input$GroupID)),
    sort(unique(.x$Mapped_SUBJ[[kri_workflows[[.y]]$steps[[which(map_chr(kri_workflows[[.y]]$steps, ~ .x$name) == "gsm.core::Input_Rate")]]$params$strGroupCol]])) # No guarantee the Input_Rate mapping is done step 2, need better index
  ))

  # data is properly transformed by correct group in dfTransformed
  expect_equal(
    n_distinct(test$cou0005$Mapped_SUBJ[[kri_workflows[[1]]$steps[[which(map_chr(kri_workflows[[1]]$steps, ~ .x$name) == "gsm.core::Input_Rate")]]$params$strGroupCol]]),
    nrow(test$cou0005$Analysis_Transformed)
  )
  expect_equal(
    n_distinct(test$kri0005$Mapped_SUBJ[[kri_workflows[[2]]$steps[[which(map_chr(kri_workflows[[2]]$steps, ~ .x$name) == "gsm.core::Input_Rate")]]$params$strGroupCol]])-8,
    nrow(test$kri0005$Analysis_Transformed)
  )

  ## custom -------------------------------------------
  test_custom <- map(kri_custom, ~ robust_runworkflow(.x, mapped_data, steps = 1:5))

  # grouping col in custom yaml file is interpreted correctly in dfInput GroupID
  iwalk(test_custom, ~ expect_identical(
    sort(unique(.x$Analysis_Input$GroupID)),
    sort(unique(.x$Mapped_SUBJ[[tolower(kri_custom[[.y]]$meta$GroupLevel)]]))
  ))

  # data is properly transformed by correct group in dfTransformed
  iwalk(test_custom, ~ expect_equal(
    n_distinct(.x$Mapped_SUBJ[[tolower(kri_custom[[.y]]$meta$GroupLevel)]]),
    nrow(.x$Analysis_Transformed)
  ))
})
