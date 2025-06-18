# gsm.qc v1.1.0

This minor release of `gsm.qc` version 1.1.0 includes several enhancements:

- Two new qualification tests were added to the suite, testing the reporting data model, including the new `gsm.reporting::CalculateChanges()` function.
- Generalizes the modifications needed to test the analytics pipeline when NAs are removed
  from the data, ensuring that the tests are robust against changes to the sample data.

# gsm.qc v1.0.0

We are excited to announce the first major release of the `gsm.qc` package, which contains the testing suite 
that incorporates all gsm ecosystem packages. This is also where the qualification tests will reside, alongside 
where the qualification report is derived.

## Notable Changes:
**Migration of qualification tests/vignettes:**
Qualification tests, related vignettes and report were migrated out of formerly `gsm`, now `gsm.core`,
to its own package, `gsm.qc`.

**Replacing `clindata` with `gsm.datasim`:**
Replaced `clindata` references with `gsm.datasim`, improving the data simulation workflow.
PR [#41](https://github.com/Gilead-BioStats/gsm.qc/pull/14) 

# gsm.qc v0.0.1

This package is used to test all entire gsm ecosystem packages in one place.
