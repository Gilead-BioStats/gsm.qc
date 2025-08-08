## Test Setup
kri_workflows <- MakeWorkflowList(strNames = "kri", strPath = "workflow/2_metrics", strPackage = "gsm.kri")
analyzed <- RunWorkflows(kri_workflows[-13], mapped_data) # Exclude pk/pd since thats not counting to SRS

reporting_workflows <- MakeWorkflowList(strPackage = "gsm.reporting")
outputs <- map(reporting_workflows, \(x) x$steps[[length(x$steps)]]$output)

## Test Code
testthat::test_that("Given summarized analytics data, all appropriate aspects of site risk score are available to calculate it correctly", {
  # Check all kri workflows have 1:1 mapped flags and respective weights
  expect_equal(
    map(kri_workflows[-13], function(x) length(strsplit(x$meta$Flag, ",")[[1]])) ,
    map(kri_workflows[-13], function(x) length(strsplit(x$meta$RiskScoreWeight, ",")[[1]]))
  )

  # Check that all Analysis_Flagged data frames contain columns for Weight and WeightMax
  expect_true(all(unlist(map(analyzed, function(x) all(c("Weight", "WeightMax") %in% names(x$Analysis_Flagged))))))


})
