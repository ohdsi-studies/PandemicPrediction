#!/usr/bin/env bash
set -euo pipefail

# Run from repository root after adding cohort 27 and regenerating base JSONs.
out_dir="inst/study_execution_jsons/coverI_only"
outcome_id="${1:-27}"
mkdir -p "$out_dir"

for src in \
  inst/study_execution_jsons/development.json \
  inst/study_execution_jsons/validation.json \
  inst/study_execution_jsons/new_models_validation.json \
  inst/study_execution_jsons/recalibration.json \
  inst/study_execution_jsons/recalibrated_models_validation.json \
  inst/study_execution_jsons/rolling_recalibrated_validation.json
  do
    if [[ -f "$src" ]]; then
      base="$(basename "$src" .json)"
      scripts/filter_strategus_coverI_only.sh \
        "$src" \
        "$out_dir/${base}_coverI_only.json" \
        "$outcome_id"
    else
      echo "Skipping missing JSON: $src" >&2
    fi
  done
