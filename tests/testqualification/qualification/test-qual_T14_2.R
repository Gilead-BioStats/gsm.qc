## Test Setup
kri_workflows <- MakeWorkflowList(c(sprintf("kri%04d", 13), sprintf("cou%04d", 13)), strPackage = "gsm.kri")
kri_custom <- MakeWorkflowList(c(sprintf("kri%04d_custom", 13), sprintf("cou%04d_custom", 13)), yaml_path_custom_metrics, strPackage = "gsm.qc")

## Test Code
testthat::test_that("PK Compliance Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.", {
  ## regular -----------------------------------------
  test <- map(kri_workflows, ~ robust_runworkflow(.x, mapped_data, steps = 1:5) %>% suppressWarnings())
  a <- map(kri_workflows, ~ robust_runworkflow(.x, mapped_data)) %>% expect_warning(., "value of 0 removed.")
  # grouping col in yaml file is interpreted correctly in dfInput GroupID
  iwalk(test, ~ expect_identical(
    sort(unique(.x$Analysis_Input$GroupID)),
    sort(unique(test$cou0013$Mapped_SUBJ[[kri_workflows[[.y]]$steps[[which(map_chr(kri_workflows[[.y]]$steps, ~ .x$name) == "gsm.core::Input_Rate")]]$params$strGroupCol]])) # No guarantee the Input_Rate mapping is done step 2, need better index
  ))

  # data is properly transformed by correct group in dfTransformed
  iwalk(test, ~ expect_true(
   all(.x$Analysis_Transformed$GroupID %in%
     unique(.x$Mapped_SUBJ[[kri_workflows[[.y]]$steps[[which(map_chr(kri_workflows[[.y]]$steps, ~ .x$name) == "gsm.core::Input_Rate")]]$params$strGroupCol]])
   )
  ))

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
