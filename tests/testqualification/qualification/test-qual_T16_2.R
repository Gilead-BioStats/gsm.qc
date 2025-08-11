## Test Setup
kri_workflows <- MakeWorkflowList(strNames = c("kri", "srs"), strPath = "workflow/2_metrics", strPackage = "gsm.kri")
analyzed <- RunWorkflows(kri_workflows[-13], mapped_data) # Exclude pk/pd since thats not counting to SRS

reporting_workflows <- MakeWorkflowList(strPackage = "gsm.reporting")
historical_reporting_results <- gsm.core::reportingResults %>% dplyr::filter(SnapshotDate < max(.data$SnapshotDate))
reporting <- RunWorkflows(reporting_workflows, lData = c(mapped_data, list(lAnalyzed = analyzed,
                                                                        lWorkflows = kri_workflows[-13])))

outputs <- map(reporting_workflows, \(x) x$steps[[length(x$steps)]]$output)

## Test Code
testthat::test_that("Given summarized analytics data, all appropriate aspects of site risk score are available to calculate it correctly", {
  # Check Site Risk Score matches by hand vs using gsm.kri functions
  transposed_by_hand <- map2(
    analyzed[-13],
    names(analyzed[-13]),
    function(x, y) {
      x$Analysis_Summary %>%
        mutate(MetricID = y) %>%
        inner_join(., gsm.kri::metricWeights, by = c("MetricID", "Flag"))
    }
  ) %>%
    bind_rows() %>%
    left_join(., select(reporting$Reporting_Metrics, MetricID, Abbreviation), by = "MetricID") %>%
    mutate(FlagIcon = Report_FormatFlag(Flag),
           Label = paste0(FlagIcon, ' <sup>', Weight, '</sup>')) %>%
    tidyr::pivot_wider(., id_cols= c("GroupID", "GroupLevel"), names_from = "Abbreviation", values_from = "Label", names_prefix = "Label_") %>%
    arrange(GroupID)

  transposed_auto <- reporting$Reporting_Results %>%
    RiskScore() %>%
    TransposeRiskScore(., dfMetrics = reporting$Reporting_Metrics) %>%
    select(GroupID, GroupLevel, contains("Label")) %>%
    arrange(GroupID)

  expect_equal(transposed_by_hand, transposed_auto)
})
