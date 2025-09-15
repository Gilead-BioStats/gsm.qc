#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import gsm.core
#' @import gsm.mapping
#' @import gsm.kri
#' @import gsm.reporting
## usethis namespace: end

# Include these "quiet" functions "in" the package so that it will produce a
# NAMESPACE file.

quiet_RunWorkflows <- function(...) {
  suppressMessages({
    RunWorkflows(...)
  })
}

quiet_RunWorkflow <- function(...) {
  suppressMessages({
    RunWorkflow(...)
  })
}

quiet_Analyze_NormalApprox <- function(...) {
  suppressMessages({
    Analyze_NormalApprox(...)
  })
}

quiet_Analyze_NormalApprox_PredictBounds <- function(
  ...,
  msg_classes = c("default_nStep", "default_vThreshold")
) {
  suppressMessages(
    {
      Analyze_NormalApprox_PredictBounds(...)
    },
    classes = glue::glue("gsm_msg-{msg_classes}")
  )
}
