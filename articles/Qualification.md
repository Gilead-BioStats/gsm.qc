# Qualification Report for the {gsm} R Packages

## Introduction

Risk-Based Quality Monitoring (RBQM) is a proactive approach to clinical
trial monitoring that focuses on identifying and addressing the most
critical risks to the integrity of study data and patient safety. This
approach aims to ensure that study data are accurate, reliable, and
credible while optimizing the use of resources and minimizing the burden
on study sites.

The `{gsm}` suite of R packages supports RBQM by performing risk
assessments primarily focused on detecting differences in quality at the
site level. This approach is intended to detect potential issues related
to critical data or process(es) across the major risk categories of
safety, efficacy, disposition, treatment, and general quality. Each
category consists of one or more risk assessment(s). Each risk
assessment analyzes the data to flag sites with potential outliers and
provides a visualization to help the user understand the issue.

## Scope

Qualification testing ensures that core functions execute as expected on
a system-wide scale. Qualification includes executing various
functional, performance, and usability testing. Qualification tests are
designed to provide developers with a repeatable process that is easy to
update and document. This document summarizes the qualification testing
performed on `gsm` functions essential to the analysis workflow.

## Process Overview

Each essential `gsm` workflow function is independently qualified using
specifications and test cases compiled in this report. Details are
provided below.

### Specifications

Specifications capture the most critical use cases for a given function.
Each function must have at least one (1) specification, and each
specification must have at least one (1) associated test case. Multiple
specifications may exist for a function, and multiple test cases may
exist for a specification.

Each specification includes the following components:

- **Description:** outlines the use case for the specification

- **Risk Assessment**

- **Risk Level:** assigned a value of “Low”, “Medium”, or “High”,
  corresponding to the risk associated with the specification failing

- **Risk Impact:** assigned a value of “Low”, “Medium”, or “High”,
  corresponding to the severity of the impact associated with the
  specification failing

- **Test Cases:** lists measurable test cases associated with the
  specification

### Test Cases

Test cases translate specifications into testable scripts to confirm
that the package functions meet the established requirements. Test cases
represent how a user may utilize the function to help identify code gaps
and support testing automation.

Test cases for all `gsm` packages are written using the standard
`testthat` workflow. A single test script is saved for each test case
and is named following the convention `test_qual_{TestID}.R`, where
`TestID` is the test case number. Test code within these scripts is
written clearly and concisely to facilitate quick execution and
interpretability. Note that a single test case may be associated with
multiple specifications.

## Test Results

### One Row Per Specification

| Spec ID | Spec Description                                                                                                                                                                                                                       | Risk | Impact | Associated Test IDs |
|---------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|--------|---------------------|
| S1_1    | Given raw participant-level data, all necessary data.frame transformations are made to create input data for all workflows                                                                                                             | High | High   | T1_1                |
| S2_1    | Given raw participant-level data, a properly specified Workflow for a KRI creates summarized and flagged data                                                                                                                          | High | High   | T2_1                |
| S2_2    | Given raw participant-level data with missingness, a properly specified Workflow for a KRI creates summarized and flagged data                                                                                                         | High | High   | T2_2                |
| S3_1    | Given pre-processed input data, a properly specified Workflow for a KRI creates summarized and flagged data                                                                                                                            | High | High   | T3_1                |
| S4_1    | Given appropriate metadata (i.e. vThresholds), flagged observations are properly marked in summary data                                                                                                                                | High | High   | T4_1                |
| S4_2    | Given appropriate metadata (i.e. vThresholds), data.frame of bounds can be created                                                                                                                                                     | High | High   | T4_2                |
| S5_1    | Given appropriate raw participant-level data, flag values can be correctly assigned to records that meet flagging criteria, including custom thresholding.                                                                             | High | High   | T5_1                |
| S5_2    | Given appropriate raw participant-level data, flag values are correctly assigned as NA for sites with low enrollment.                                                                                                                  | High | High   | T5_2                |
| S6_1    | Given appropriate raw participant-level data, an Adverse Event Assessment can be done using the Normal Approximation method.                                                                                                           | High | High   | T6_1                |
| S6_2    | Adverse Event Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.                                                                                      | High | High   | T6_2                |
| S7_1    | Given appropriate raw participant-level data, a Protocol Deviation Assessment can be done using the Normal Approximation method.                                                                                                       | High | High   | T7_1                |
| S7_2    | Protocol Deviation Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.                                                                                 | High | High   | T7_2                |
| S8_1    | Given appropriate raw participant-level data, a Dispositon Assessment can be done using the Normal Approximation method.                                                                                                               | High | High   | T8_1                |
| S8_2    | Disposition Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.                                                                                        | High | High   | T8_2                |
| S9_1    | Given appropriate raw participant-level data, a Labs Assessment can be done using the Normal Approximation method.                                                                                                                     | High | High   | T9_1                |
| S9_2    | Labs Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.                                                                                               | High | High   | T9_2                |
| S10_1   | Given appropriate raw participant-level data, a Data Change Rate Assessment can be done using the Normal Approximation method.                                                                                                         | High | High   | T10_1               |
| S10_2   | Data Change Rate Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                                                                                        | High | High   | T10_2               |
| S11_1   | Given appropriate raw participant-level data, a Data Entry Lag Assessment can be done using the Normal Approximation method.                                                                                                           | High | High   | T11_1               |
| S11_2   | Data Entry Lag Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                                                                                          | High | High   | T11_2               |
| S12_1   | Given appropriate raw participant-level data, a Query Age Assessment can be done using the Normal Approximation method.                                                                                                                | High | High   | T12_1               |
| S12_2   | Query Age Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                                                                                               | High | High   | T12_2               |
| S13_1   | Given appropriate raw participant-level data, a Query Rate Assessment can be done using the Normal Approximation method.                                                                                                               | High | High   | T13_1               |
| S13_2   | Query Rate Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                                                                                              | High | High   | T13_2               |
| S14_1   | Given appropriate raw participant-level data, a PK Compliance Rate Assessment can be done using the Identity method.                                                                                                                   | High | High   | T14_1               |
| S14_2   | PK Compliance Rate Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                                                                                      | High | High   | T14_2               |
| S15_1   | Given summarized analytics data, a properly specified reporting workflow creates cross-sectional results data set with one record per metric per group.                                                                                | High | High   | T15_1               |
| S15_2   | Given summarized analytics data and historical reporting results data, a properly specified reporting workflow creates cross-sectional results data set including changes from previous snapshot with one record per metric per group. | High | High   | T15_2               |
| S16_1   | Given analytics data and weights defined in each metric's workflow yaml, ensure the site risk score is correctly calculated using the preexisting functions in gsm.kri                                                                 |      | High   | T16_1               |

### One Row Per Test

| Function                           | Spec ID | Test ID | Test Description                                                                                                                                           | Test Result |
|------------------------------------|---------|---------|------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------|
| Mapping workflow                   | S1_1    | T1_1    | mappings now done by individual domain, test that inputs and outputs of priority 1 mappings are completed as expected                                      | Pass        |
| Mapping workflow                   | S1_1    | T1_1    | mappings now done by individual domain, test that inputs and outputs of priority 2 mappings are completed as expected                                      | Pass        |
| Mapping workflow                   | S1_1    | T1_1    | mappings now done by individual domain, test that inputs and outputs of priority 3 mappings are completed as expected                                      | Pass        |
| Analysis workflow                  | S2_1    | T2_1    | Given raw participant-level data, a properly specified Workflow for a KRI creates summarized and flagged data                                              | Pass        |
| Analysis workflow                  | S2_2    | T2_2    | Given raw participant-level data with missingness, a properly specified Workflow for a KRI creates summarized and flagged data                             | Pass        |
| Analysis workflow                  | S3_1    | T3_1    | Given pre-processed input data, a properly specified Workflow for a KRI creates summarized and flagged data                                                | Pass        |
| Flag_NormalApprox                  | S4_1    | T4_1    | Given appropriate metadata (i.e. vThresholds), flagged observations are properly marked in summary data                                                    | Pass        |
| Analyze_NormalApprox_PredictBounds | S4_2    | T4_2    | Given appropriate metadata (i.e. vThresholds), bounds are properly applied to generate flags                                                               | Pass        |
| Summarize                          | S5_1    | T5_1    | Given appropriate raw participant-level data, flag values can be correctly assigned to records that meet flagging criteria, including custom thresholding. | Pass        |
| Summarize                          | S5_2    | T5_2    | Given appropriate raw participant-level data, flag values are correctly assigned as NA for sites with low enrollment.                                      | Pass        |
| Adverse Event Assessment           | S6_1    | T6_1    | Given appropriate raw participant-level data, an Adverse Event Assessment can be done using the Normal Approximation method.                               | Pass        |
| Adverse Event Assessment           | S6_2    | T6_2    | Adverse Event Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.          | Pass        |
| Protocol Deviation Assessment      | S7_1    | T7_1    | Given appropriate raw participant-level data, a Protocol Deviation Assessment can be done using the Normal Approximation method.                           | Pass        |
| Protocol Deviation Assessment      | S7_2    | T7_2    | Protocol Deviation Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.     | Pass        |
| Disposition Assessment             | S8_1    | T8_1    | Given appropriate raw participant-level data, a Dispositon Assessment can be done using the Normal Approximation method.                                   | Pass        |
| Disposition Assessment             | S8_2    | T8_2    | Disposition Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.            | Pass        |
| Labs Assessment                    | S9_1    | T9_1    | Given appropriate raw participant-level data, a Labs Assessment can be done using the Normal Approximation method.                                         | Pass        |
| Labs Assessment                    | S9_2    | T9_2    | Labs Assessments can be done correctly using a grouping variable, such as Site or Country for KRIs, and Study for QTLs, when applicable.                   | Pass        |
| Data Change Rate Assessment        | S10_1   | T10_1   | Given appropriate raw participant-level data, a Data Change Rate Assessment can be done using the Normal Approximation method.                             | Pass        |
| Data Change Rate Assessment        | S10_2   | T10_2   | Data Change Rate Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                            | Pass        |
| Data Entry Lag Assessment          | S11_1   | T11_1   | Given appropriate raw participant-level data, a Data Entry Lag Assessment can be done using the Normal Approximation method.                               | Pass        |
| Data Entry Lag Assessment          | S11_2   | T11_2   | Data Entry Lag Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                              | Pass        |
| Query Age Assessment               | S12_1   | T12_1   | Given appropriate raw participant-level data, a Query Age Assessment can be done using the Normal Approximation method.                                    | Pass        |
| Query Age Assessment               | S12_2   | T12_2   | Query Age Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                   | Pass        |
| Query Rate Assessment              | S13_1   | T13_1   | Given appropriate raw participant-level data, a Query Rate Assessment can be done using the Normal Approximation method.                                   | Pass        |
| Query Rate Assessment              | S13_2   | T13_2   | Query Rate Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                                  | Pass        |
| PK Compliance Rate Assessment      | S14_1   | T14_1   | Given appropriate raw participant-level data, a PK Compliance Assessment can be done using the Identity method.                                            | Pass        |
| PK Compliance Rate Assessment      | S14_2   | T14_2   | PK Compliance Assessments can be done correctly using a grouping variable, such as Site, Country, or Study, when applicable.                               | Pass        |
| Reporting Workflow                 | S15_1   | T15_1   | NA                                                                                                                                                         | Pass        |
| Reporting Workflow                 | S15_2   | T15_2   | NA                                                                                                                                                         | Pass        |
| Site Risk Score Calculation        | S16_1   | T16_1   | NA                                                                                                                                                         | Pass        |

## Qualification Testing Environment

### Session Information

**R version 4.5.2 (2025-10-31)**

**Platform:** x86_64-pc-linux-gnu

**locale:** *LC_CTYPE=C.UTF-8*, *LC_NUMERIC=C*, *LC_TIME=C.UTF-8*,
*LC_COLLATE=C.UTF-8*, *LC_MONETARY=C.UTF-8*, *LC_MESSAGES=C.UTF-8*,
*LC_PAPER=C.UTF-8*, *LC_NAME=C*, *LC_ADDRESS=C*, *LC_TELEPHONE=C*,
*LC_MEASUREMENT=C.UTF-8* and *LC_IDENTIFICATION=C*

**attached base packages:** *tcltk*, *stats*, *graphics*, *grDevices*,
*utils*, *datasets*, *methods* and *base*

**other attached packages:** *yaml(v.2.3.12)*, *glue(v.1.8.0)*,
*cli(v.3.6.5)*, *tidyr(v.1.3.2)*, *gsm.qc(v.1.1.1)*,
*testthat(v.3.3.2)*, *riskmetric(v.0.2.6)*, *stringr(v.1.6.0)*,
*gh(v.1.5.0)*, *pander(v.0.6.6)*, *purrr(v.1.2.1)*, *dplyr(v.1.2.0)*,
*knitr(v.1.51)*, *gt(v.1.3.0)*, *gsm.reporting(v.1.1.3)*,
*gsm.kri(v.1.4.1)*, *gsm.mapping(v.1.1.2)* and *gsm.core(v.1.1.8)*

**loaded via a namespace (and not attached):** *gtable(v.0.3.6)*,
*xfun(v.0.56)*, *bslib(v.0.10.0)*, *ggplot2(v.4.0.2)*,
*remotes(v.2.5.0)*, *htmlwidgets(v.1.6.4)*, *devtools(v.2.4.6)*,
*vctrs(v.0.7.1)*, *tools(v.4.5.2)*, *generics(v.0.1.4)*,
*curl(v.7.0.0)*, *tibble(v.3.3.1)*, *pkgconfig(v.2.0.3)*,
*dbplyr(v.2.5.2)*, *RColorBrewer(v.1.1-3)*, *S7(v.0.2.1)*,
*desc(v.1.4.3)*, *lifecycle(v.1.0.5)*, *compiler(v.4.5.2)*,
*farver(v.2.1.2)*, *brio(v.1.1.5)*, *textshaping(v.1.0.5)*,
*usethis(v.3.2.1)*, *htmltools(v.0.5.9)*, *sass(v.0.4.10)*,
*lazyeval(v.0.2.2)*, *pillar(v.1.11.1)*, *pkgdown(v.2.2.0)*,
*jquerylib(v.0.1.4)*, *ellipsis(v.0.3.2)*, *cranlogs(v.2.1.1)*,
*DT(v.0.34.0)*, *cachem(v.1.1.0)*, *sessioninfo(v.1.2.3)*,
*tidyselect(v.1.2.1)*, *digest(v.0.6.39)*, *stringi(v.1.8.7)*,
*duckdb(v.1.4.4)*, *rprojroot(v.2.1.1)*, *fastmap(v.1.2.0)*,
*grid(v.4.5.2)*, *here(v.1.0.2)*, *magrittr(v.2.0.4)*,
*triebeard(v.0.4.1)*, *pkgbuild(v.1.4.8)*, *withr(v.3.0.2)*,
*waldo(v.0.6.2)*, *scales(v.1.4.0)*, *backports(v.1.5.0)*,
*rmarkdown(v.2.30)*, *httr(v.1.4.8)*, *otel(v.0.2.0)*, *ragg(v.1.5.1)*,
*memoise(v.2.0.1)*, *evaluate(v.1.0.5)*, *log4r(v.0.4.4)*,
*covr(v.3.6.5)*, *rex(v.1.2.1)*, *urltools(v.1.7.3.1)*,
*rlang(v.1.1.7)*, *Rcpp(v.1.1.1)*, *DBI(v.1.3.0)*,
*BiocManager(v.1.30.27)*, *xml2(v.1.5.2)*, *pkgload(v.1.5.0)*,
*rstudioapi(v.0.18.0)*, *jsonlite(v.2.0.0)*, *R6(v.2.6.1)*,
*systemfonts(v.1.3.2)* and *fs(v.1.6.7)*

## Pull Request History

The GitHub Pull Request (PR) process begins with the creation of one or
more issues that clearly define the proposed additions or revisions to
the package. Each issue should be assigned to a product developer (PD)
who then creates a `fix` branch named according to the related issue(s),
and implements the necessary code updates. Once the work is complete,
the PD opens a Pull Request to merge the `fix` branch into the target
branch (typically the `dev` branch). They must assign the PR to
themselves, request one or more reviewers, and link the PR to the
associated issue(s). Before the fix branch can be merged, the PR must be
approved by the designated reviewers and pass all required GitHub
qualification checks. Once these conditions are met, the `fix` branch is
merged into the target branch. This process is fully documented in the
[Contributor
Guidelines](https://gilead-biostats.github.io/gsm.core/articles/ContributorGuidelines.html#development-process)

Below, the most recent 10 PRs into gsm.qc are displayed. [See all Pull
Requests here.](https://github.com/gilead-biostats/gsm.qc/pulls)

#### Pull Request 45: Oops run this in R.

Merging use-r-shell into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/45>

| Requester  |   Date Requested    |  Reviewers  | Review Status |
|:----------:|:-------------------:|:-----------:|:-------------:|
| jonthegeek | 2025-09-26 21:03:54 | nandriychuk |   APPROVED    |

#### Pull Request 44: More workflow protection

Merging fix-qualification-emptyish into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/44>

| Requester  |   Date Requested    |  Reviewers  | Review Status |
|:----------:|:-------------------:|:-----------:|:-------------:|
| jonthegeek | 2025-09-26 20:39:53 | nandriychuk |   APPROVED    |

#### Pull Request 43: Add missing `)`

Merging fix-qualification-dispatch-paren into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/43>

| Requester  |   Date Requested    |  Reviewers  | Review Status |
|:----------:|:-------------------:|:-----------:|:-------------:|
| jonthegeek | 2025-09-26 20:03:45 | nandriychuk |   APPROVED    |

#### Pull Request 42: Properly build repo name and install

Merging fix-qualification-dispatch into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/42>

| Requester  |   Date Requested    |  Reviewers  | Review Status |
|:----------:|:-------------------:|:-----------:|:-------------:|
| jonthegeek | 2025-09-26 18:09:04 | nandriychuk |   APPROVED    |

#### Pull Request 41: Install gsm.qc as part of setup-r-dependencies step

Merging fix-40-working_workflow into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/41>

| Requester  |   Date Requested    |                    Reviewers                     | Review Status |
|:----------:|:-------------------:|:------------------------------------------------:|:-------------:|
| jonthegeek | 2025-09-26 14:53:34 | copilot-pull-request-reviewer\[bot\] nandriychuk |   COMMENTED   |

#### Pull Request 39: catch dev up to main

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/39>

| Requester |   Date Requested    | Reviewers  | Review Status |
|:---------:|:-------------------:|:----------:|:-------------:|
|  zdz2101  | 2025-09-18 19:52:18 | jonthegeek |   APPROVED    |

#### Pull Request 38: Gsm.qc v1.1.1 Release Candidate

Merging gsm.qc-v1.1.1 into main

<https://github.com/Gilead-BioStats/gsm.qc/pull/38>

| Requester |   Date Requested    |      Reviewers       | Review Status |
|:---------:|:-------------------:|:--------------------:|:-------------:|
|  zdz2101  | 2025-09-15 15:58:56 | jonthegeek jwildfire |   APPROVED    |

#### Pull Request 37: Solve merge conflict

Merging solve-merge-conflict into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/37>

| Requester |   Date Requested    | Reviewers  | Review Status |
|:---------:|:-------------------:|:----------:|:-------------:|
|  zdz2101  | 2025-09-12 19:35:40 | jonthegeek |   APPROVED    |

#### Pull Request 36: Main -\> Dev

Merging main into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/36>

|  Requester   |   Date Requested    | Reviewers | Review Status |
|:------------:|:-------------------:|:---------:|:-------------:|
| lauramaxwell | 2025-09-12 19:08:36 |           |               |

#### Pull Request 35: Update .github folder templates using github API

Merging update-github-folder into dev

<https://github.com/Gilead-BioStats/gsm.qc/pull/35>

|  Requester   |   Date Requested    | Reviewers | Review Status |
|:------------:|:-------------------:|:---------:|:-------------:|
| lauramaxwell | 2025-09-11 15:40:10 |  zdz2101  |   APPROVED    |
