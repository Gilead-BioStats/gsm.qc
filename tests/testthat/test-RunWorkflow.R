suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(cli)
  library(glue)
})
set.seed(123)
core_mappings <- c(
  "AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB",
  "PD", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ"
)

wf_mapping <- MakeWorkflowList(
  strPath = "workflow/1_mappings",
  strNames = core_mappings,
  strPackage = "gsm.mapping"
)
workflows <- MakeWorkflowList(strNames = paste0("kri", sprintf("%04d", 1:2)), strPackage = "gsm.kri")

lSource <- gsm.core::lSource

lRaw <- list(
  Raw_SUBJ = lSource$Raw_SUBJ,
  Raw_AE = lSource$Raw_AE,
  Raw_PD = lSource$Raw_PD %>%
    rename(subjid = subjectenrollmentnumber),
  Raw_LB = lSource$Raw_LB,
  Raw_STUDCOMP = lSource$Raw_STUDCOMP %>%
    select(subjid, compyn),
  Raw_SDRGCOMP = lSource$Raw_SDRGCOMP,
  Raw_DATACHG = lSource$Raw_DATACHG %>%
    rename(subject_nsv = subjectname),
  Raw_DATAENT = lSource$Raw_DATAENT %>%
    rename(subject_nsv = subjectname),
  Raw_QUERY = lSource$Raw_QUERY %>%
    rename(subject_nsv = subjectname),
  Raw_ENROLL = lSource$Raw_ENROLL,
  Raw_SITE = lSource$Raw_SITE %>%
    rename(studyid = protocol) %>%
    rename(invid = pi_number) %>%
    rename(InvestigatorFirstName = pi_first_name) %>%
    rename(InvestigatorLastName = pi_last_name) %>%
    rename(City = city) %>%
    rename(State = state) %>%
    rename(Country = country) %>%
    rename(Status = site_status),
  Raw_STUDY = lSource$Raw_STUDY %>%
    rename(studyid = protocol_number) %>%
    rename(Status = status)
)

# Create Mapped Data
gsm.core::SetLogger(log4r::logger(threshold = "FATAL"))
lMapped <- quiet_RunWorkflows(lWorkflows = wf_mapping, lData = lRaw)

# Run Metrics
results <- map(
  workflows,
  ~ quiet_RunWorkflow(lWorkflow = .x, lData = lMapped, bReturnResult = FALSE, bKeepInputData = FALSE)
)

yaml_outputs <- map(
  map(workflows, ~ map_vec(.x$steps, ~ .x$output)),
  ~ .x[!grepl("lCharts", .x)]
)
gsm.core::SetLogger(log4r::logger(threshold = "WARN"))

test_that("RunWorkflow preserves all steps when bReturnResult = FALSE", {
  expect_no_error({
    purrr::iwalk(
      workflows,
      function(this_workflow, this_name) {
        expect_identical(
          this_workflow, results[[this_name]][names(this_workflow)]
        )
      }
    )
  })
})

test_that("RunWorkflow contains all outputs from yaml steps", {
  expect_no_error({
    purrr::iwalk(
      results,
      function(this_result, this_name) {
        expect_setequal(yaml_outputs[[this_name]], names(this_result$lData))
      }
    )
  })
})

test_that("RunWorkflow contains all outputs from yaml steps with populated fields (contains rows of data)", {
  expect_no_error({
    purrr::iwalk(
      yaml_outputs,
      function(this_output_set, this_name) {
        expect_true(
          all(map_int(results[[this_name]]$lData[this_output_set], NROW) > 0)
        )
      }
    )
  })
})

# ----
# Test [ lConfig ] parameter of [ RunWorkflow ].

lConfig <- list(
  LoadData = function(lWorkflow, lConfig, lData) {
    lData <- lData
    purrr::imap(
      lWorkflow$spec,
      ~ {
        input <- lConfig$Domains[[.y]]

        if (is.data.frame(input)) {
          data <- input
        } else if (is.function(input)) {
          data <- input()
        } else if (is.character(input)) {
          data <- read.csv(input)
        } else {
          cli::cli_abort("Invalid data source: {input}.")
        }

        lData[[.y]] <<- (ApplySpec(data, .x))
      }
    )
    return(lData)
  },
  SaveData = function(lWorkflow, lConfig) {
    domain <- paste0(lWorkflow$meta$Type, "_", lWorkflow$meta$ID)
    if (domain %in% names(lConfig$Domains)) {
      assign(
        domain,
        lWorkflow$lResult,
        envir = .GlobalEnv
      )
    }
  },
  Domains = c(
    Raw_AE = function() {
      lSource$Raw_AE
    },
    Mapped_AE = function() {
      Mapped_AE
    }
  )
)

test_that("RunWorkflow loads/saves data with configuration object.", {
  expect_no_error({
    RunWorkflow(
      lWorkflow = wf_mapping$AE,
      lConfig = lConfig
    )
  })

  expect_true(exists("Mapped_AE"))
})

test_that("RunWorkflow passes existing lData objects through with configuration object.", {
  lData <- list(lWorkflows = wf_mapping)

  expect_no_error({
    output <- RunWorkflow(
      lWorkflow = wf_mapping$AE,
      lConfig = lConfig,
      lData = lData,
      bReturnResult = F
    )
  })

  expect_true(!is.null(output$lData$lWorkflows))
})

# Trick gsm.core into throwing errors as errors, not as logs. I think this
# happens "naturally" on GHA but tests failed for me locally without this.
gsm.core::SetLogger(log4r::logger(
  "WARN",
  function(level, ...) {
    msg <- log4r::bare_log_layout()(level, ...)
    if (toupper(level) == "WARN") {
      warning(msg)
    } else {
      stop(msg)
    }
  }
))

test_that("RunWorkflow errors out if [ lConfig ] does not have a method to load data.", {
  lBadConfig <- lConfig
  lBadConfig$LoadData <- NULL

  expect_error(
    {
      RunWorkflow(
        lWorkflow = wf_mapping$AE,
        lConfig = lBadConfig
      )
    },
    "must include a function named .LoadData."
  )
})

test_that("RunWorkflow errors out if the data load method does not have expected parameters.", {
  lBadConfig <- lConfig
  lBadConfig$LoadData <- function(lWorkflow) {}

  expect_error(
    {
      RunWorkflow(
        lWorkflow = wf_mapping$AE,
        lConfig = lBadConfig
      )
    },
    "must include a function named .LoadData."
  )

  lBadConfig <- lConfig
  lBadConfig$LoadData <- function(lConfig) {}

  expect_error(
    {
      RunWorkflow(
        lWorkflow = wf_mapping$AE,
        lConfig = lBadConfig
      )
    },
    "must include a function named .LoadData."
  )
})

test_that("RunWorkflow errors out if [ lConfig ] does not have a method to save data.", {
  lBadConfig <- lConfig
  lBadConfig$SaveData <- NULL

  expect_error(
    {
      RunWorkflow(
        lWorkflow = wf_mapping$AE,
        lConfig = lBadConfig
      )
    },
    "must include a function named .SaveData."
  )
})

test_that("RunWorkflow errors out if the data save method does not have expected parameters.", {
  lBadConfig <- lConfig
  lBadConfig$SaveData <- function(lWorkflow) {}

  expect_error(
    {
      RunWorkflow(
        lWorkflow = wf_mapping$AE,
        lConfig = lBadConfig
      )
    },
    "must include a function named .SaveData."
  )

  lBadConfig <- lConfig
  lBadConfig$SaveData <- function(lConfig) {}

  expect_error(
    {
      RunWorkflow(
        lWorkflow = wf_mapping$AE,
        lConfig = lBadConfig
      )
    },
    "must include a function named .SaveData."
  )
})
