# Corrected COVER-I rerun checklist

Goal: rerun only the I/Critical outcome branch using the corrected COVER-I outcome cohort, without overwriting the current H/F manuscript results or mixing legacy outcome `13` with the corrected outcome.

## Current decision

- Corrected COVER-I outcome ID is centralized in `R/outcomeIds.R` as `Critical = 27`.
- Legacy outcome `13` is retained only as `RespiratoryFailureLegacy` for old-result identification.
- Before running, add the corrected COVER-I cohort JSON and `inst/Cohorts.csv` row for cohort `27`.

## 1. Add corrected outcome definition

1. Add the corrected cohort JSON:
   - `inst/cohorts/27_[O] Pandemic Prediction intensive services or death.json`
2. Add this row to `inst/Cohorts.csv`:
   - `27,27,[O] Pandemic Prediction intensive services or death,NA,TRUE`
3. Confirm the cohort definition reflects the original COVER-I endpoint: hospitalization with pneumonia requiring intensive services and/or death, as agreed with the SEEK-COVER first author.

## 2. Regenerate Strategus specifications

Run from repo root after confirming `R/outcomeIds.R` has `Critical = 27`:

```bash
Rscript extras/codeToCreateDevelopmentJsons.R
Rscript extras/codeToCreateValidation.R
Rscript extras/codeToCreateValidationCovidModels.R
Rscript extras/createRecalibration.R
```

After fixed recalibrated proxy models have been promoted, regenerate recalibrated-model validation specs:

```bash
Rscript extras/createRecalibratedValidation.R
```

If rolling recalibration is retained, regenerate its validation specs after rebuilding rolling recalibrated models.

Then create COVER-I-only Strategus JSONs so the data environment does not run H/F tasks:

```bash
scripts/make_coverI_only_jsons.sh 27
```

This writes filtered JSONs to `inst/study_execution_jsons/coverI_only/`. Transfer and run those filtered JSONs, not the full JSONs.

## 3. Rerun model development and validation

Use only the filtered JSONs in `inst/study_execution_jsons/coverI_only/`; these keep only tasks where `outcomeId == 27`. Do not run the unfiltered JSONs unless you intentionally want to rerun H/F.

Use separate output roots if possible, for example:

- `results/runs_coverI_v2/development/`
- `results/runs_coverI_v2/validation/`
- `results/runs_coverI_v2/new_models_validation/`
- `results/runs_coverI_v2/recalibration/`
- `results/runs_coverI_v2/recalibrated_models_validation/`
- `results/runs_coverI_v2/rolling_recalibrated_validation/`

Rerun for the corrected I/Critical outcome branch:

1. COVID-specific development for both feature sets:
   - `Critical_Parsimonious`
   - `Critical_Full`
2. COVID development windows:
   - first 3 months
   - first 6 months
   - first 9 months
   - first 12 months sampled
   - first 12 months full
3. Proxy model evaluations:
   - frozen parsimonious proxy: `models/cato`
   - frozen full proxy: `models/dataDrivenI`
4. Fixed recalibration of both proxy models using corrected outcome `27`.
5. Fixed recalibrated proxy validation.
6. Rolling recalibration sensitivity, if retained.

## 4. Promote corrected model artifacts

Promote the corrected I/Critical COVID-specific models to a separate model directory first, rather than overwriting `inst/newModels` immediately. Recommended scratch structure:

- `inst/newModels_coverI_v2/`
- `inst/recalibratedModels_coverI_v2/`
- `inst/rollingRecalibratedModels_coverI_v2/`

Only replace manuscript-facing model folders after QC passes.

## 5. QC before post-analysis

Run these checks before aggregating results:

1. Confirm every corrected I/Critical JSON uses `outcomeId = 27`, not `13`.
2. Confirm cohort generation includes cohort `27` and the event counts are nonzero by quarter.
3. Compare legacy `13` vs corrected `27` quarterly incidence to understand endpoint change.
4. Confirm all COVID model development windows have expected N/events.
5. Confirm whether 3m corrected COVER-I models are estimable.
6. Confirm no aggregated result file combines outcome `13` and outcome `27` under the same stratum.
7. Confirm model folders and result filenames distinguish corrected I/Critical results from legacy respiratory-failure results.

Suggested quick checks:

```bash
rg -n '"outcomeId": 13|outcomeId = 13' inst/study_execution_jsons
rg -n '"outcomeId": 27|outcomeId = 27' inst/study_execution_jsons
```

## 6. Rebuild post-analysis results

After corrected I/Critical results are extracted:

1. Rebuild quarterwise result CSVs for corrected I/Critical.
2. Combine H/F existing rows with corrected I/Critical rows only after checking no legacy `13` rows remain in manuscript-facing outputs.
3. Regenerate:
   - Table 1
   - Figure 1
   - Figure 2
   - Figure 3
   - Supplement Table S1
   - Supplement Figures S1/S2/S3
   - rolling recalibration supplement
   - cohort definitions appendix

## 7. Manuscript wording update

Do not call the corrected endpoint `Respiratory failure` unless the final cohort truly only captures respiratory failure. Preferred labels to consider:

- `Intensive services or death`
- `COVER-I`
- `Severe COVID outcome`

Whichever label is chosen must match Table 1, figures, captions, Methods, and supplement cohort definitions.
