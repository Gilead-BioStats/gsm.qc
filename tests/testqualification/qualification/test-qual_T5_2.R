## Test Setup
kri_workflows <- c(
  MakeWorkflowList(c("kri0001", "cou0001"), strPackage = "gsm.kri"),
  MakeWorkflowList("kri0001_custom", yaml_path_custom_metrics, strPackage = "gsm.qc")
)

partial_mapped_workflows <- map(kri_workflows, ~ robust_runworkflow(.x, mapped_data, steps = 1:5))

## Test Code
testthat::test_that("Given appropriate raw participant-level data, flag values are correctly assigned as NA for sites with low enrollment.", {
  # define custom min denominator
  test_nMinDenominator <- c(500, 1000, 1500)

  # test output row size
  test_output <- map(test_nMinDenominator, function(test) {
    a <- map(partial_mapped_workflows, ~ cli::cli_fmt(Flag(.x$Analysis_Analyzed, strAccrualMetric = "Denominator", nAccrualThreshold = test))[1])
    map(a, ~ as.numeric(str_extract(., "(\\d+)(?=\\s+Group)")))
  })

  hardcode_output <- map(test_nMinDenominator, function(test) {
    original <- map(partial_mapped_workflows, ~ nrow(.x$Analysis_Flagged))

    filt <- map(partial_mapped_workflows, function(df) {
      df$Analysis_Flagged %>%
        filter(Denominator >= test) %>%
        nrow()
    })

    imap(original, ~ .x - filt[[.y]])
  })

  expect_equal(test_output, hardcode_output)

  # test for identical output
  yaml_test <- map(test_nMinDenominator, function(test) {
    for (workflow in names(kri_workflows)) {
      kri_workflows[[workflow]]$meta$AccrualThreshold <- test
    }
    imap(partial_mapped_workflows, ~ robust_runworkflow(kri_workflows[[.y]], .x, steps = 5, bKeepInputData = F)[["Analysis_Flagged"]])
  })

  function_test <- map(test_nMinDenominator, function(test) {
    map(partial_mapped_workflows, ~ Flag(.x$Analysis_Analyzed, strAccrualMetric = "Denominator", nAccrualThreshold = test,
                                         vThreshold = .x$vThreshold,
                                         vFlag = c(-2,-1,0,1,2)))
  })

  expect_identical(yaml_test, function_test)
})
