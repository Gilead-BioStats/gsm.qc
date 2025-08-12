## Test Setup
kri_workflows <- MakeWorkflowList(strNames = "kri", strPath = "workflow/2_metrics", strPackage = "gsm.kri")
analyzed <- RunWorkflows(kri_workflows[-13], mapped_data) # Exclude pk/pd since thats not counting to SRS

reporting_workflows <- MakeWorkflowList(strPackage = "gsm.reporting")
historical_reporting_results <- gsm.core::reportingResults %>% dplyr::filter(SnapshotDate < max(.data$SnapshotDate))
reporting <- RunWorkflows(reporting_workflows, lData = c(mapped_data, list(lAnalyzed = analyzed,
                                                                        lWorkflows = kri_workflows[-13])))

## Test Code
testthat::test_that("Given summarized analytics data, all appropriate aspects of site risk score are available to calculate it correctly", {
  # Check all kri workflows have 1:1 mapped flags and respective weights
  expect_equal(
    map(kri_workflows[-13], function(x) length(strsplit(x$meta$Flag, ",")[[1]])) ,
    map(kri_workflows[-13], function(x) length(strsplit(x$meta$RiskScoreWeight, ",")[[1]]))
  )

  # Check that all Analysis_Flagged data frames contain columns for Weight and WeightMax
  expect_true(all(unlist(map(analyzed, function(x) all(c("Weight", "WeightMax") %in% names(x$Analysis_Flagged))))))

  # Check Site Risk Score matches by hand vs using gsm.kri functions
  SRS_by_hand <- map2(
    analyzed,
    names(analyzed),
    function(x, y) {
      x$Analysis_Summary %>%
        mutate(MetricID = y) %>%
        inner_join(., gsm.kri::metricWeights, by = c("MetricID", "Flag"))
    }
  ) %>%
    bind_rows() %>%
    group_by(GroupID) %>%
    summarize(Numerator = sum(Weight, na.rm = TRUE),
              Denominator = sum(WeightMax, na.rm = TRUE),
              Metric = Numerator/Denominator * 100,
              Score = Metric)

  SRS_auto <- analyzed %>% CalculateRiskScore()

  expect_equal(SRS_by_hand, SRS_auto)
})
