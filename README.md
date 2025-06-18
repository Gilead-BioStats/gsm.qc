# gsm.qc

Quality Control Tests and documentation for the {gsm} suite of R packages.

This package creates a qualification report that is triggered by the release of one
of the gsm core packages (`gsm.core`, `gsm.mapping`, `gsm.kri`, `gsm.reporting`).
This qualification report runs a series of qualification tests, specified in the
`qualification/qualification_specs.csv` table and reports their status. 

Additional information about these qualification tests can be found in the [Qualification Workflow Vignette](https://gilead-biostats.github.io/gsm.qc/articles/QualificationWorkflow.html).
