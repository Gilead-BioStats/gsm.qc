## Test Setup
kri_workflows <- MakeWorkflowList(strPath = "workflow/2_metrics", strPackage = "gsm.kri")
reporting_workflows <- MakeWorkflowList(strPackage = "gsm.reporting")
analyzed <- RunWorkflows(kri_workflows, mapped_data)
outputs <- map(reporting_workflows, \(x) x$steps[[length(x$steps)]]$output)
historical_reporting_results <- gsm.core::reportingResults %>% dplyr::filter(SnapshotDate < max(.data$SnapshotDate))
## Test Code
testthat::test_that("Given summarized analytics data and historical reporting results data, a properly specified reporting workflow creates cross-sectional results data set including changes from previous snapshot with one record per metric per group.", {
  test <- RunWorkflows(reporting_workflows, lData = c(mapped_data, list(lAnalyzed = analyzed,
                                                                        lWorkflows = kri_workflows,
                                                                        Reporting_Results_Longitudinal = historical_reporting_results)))

  # test output stucture
  expect_true(all(map_lgl(test, \(x) is.data.frame(x))))
  expect_equal(nrow(test$Reporting_Metrics), length(analyzed))
  expect_equal(nrow(test$Reporting_Results), analyzed %>% map(\(x) {nrow(x$Analysis_Summary)}) %>% do.call(sum, .))

  # test output content basics
  expect_true(all(outputs %in% names(test)))
  expect_true(all(unique(test$Reporting_Results$GroupID) %in% (analyzed %>% map(\(x) {x$Analysis_Summary$GroupID}) %>% do.call(c, .) %>% unique())))
  expect_true(all(apply(expand.grid(c("Numerator", "Denominator", "Metric", "Score", "Flag"), c("Previous", "Change", "PercentChange")), 1, paste, collapse="_")
                  %in% names(test$Reporting_Results)))
  ## construct metrics df with kri_workflows meta and compare to output
  metrics_df <- kri_workflows %>% purrr::map(function(wf) {
    return(tibble::as_tibble(wf$meta))
  }) %>% purrr::list_rbind() %>%
    mutate(MetricID = paste0(.data$Type,
                             "_", .data$ID))
  expect_equal(test$Reporting_Metrics, metrics_df)
  expect_equal(test$Reporting_Groups, bind_rows(mapped_data$Mapped_STUDY, mapped_data$Mapped_SITE, mapped_data$Mapped_COUNTRY))

  # manually construct delta output and compare with test output
  dfResults_combined <- dplyr::bind_rows(historical_reporting_results, test$Reporting_Results) %>%
    select(StudyID, GroupLevel, GroupID, MetricID, SnapshotDate, Numerator, Denominator, Metric, Score, Flag)

  dfChanges <- dfResults_combined %>%
    tidyr::pivot_longer(
    cols = all_of( c("Numerator", "Denominator", "Metric", "Score", "Flag")),
    names_to = "Param",
    values_to = "Value"
  ) %>%
    dplyr::mutate(Param = factor(.data$Param, levels = c("Numerator", "Denominator", "Metric", "Score", "Flag"))) %>%
    dplyr::group_by(across(all_of(c(
    "StudyID",
    "GroupLevel",
    "GroupID",
    "MetricID",
    "Param"
  )))) %>%
    dplyr::arrange("SnapshotDate", .by_group = TRUE) %>%
    dplyr::mutate(
    Previous = dplyr::lag(.data$Value),
    Change = Value - dplyr::lag(.data$Value),
    PercentChange = .data$Change / dplyr::lag(.data$Value) *
      100,
    SnapshotDate_Previous = dplyr::lag(.data$SnapshotDate)
  ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(across(all_of(
      c(
        "StudyID",
        "GroupLevel",
        "GroupID",
        "MetricID",
        "SnapshotDate_Previous",
        "Param",
        "SnapshotDate"
      )
    ))) %>%
    tidyr::pivot_wider(
      names_from = Param,
      values_from = c(Value, Previous, Change, PercentChange),
      names_glue = "{Param}_{.value}",
    ) %>%
    dplyr::rename_with( ~ gsub("_Value", "", .x), tidyr::ends_with("_Value"))

  dfChanges <- gsm.kri::FilterByLatestSnapshotDate(dfChanges)

  expect_equal(test$Reporting_Results, dfChanges)
  })


