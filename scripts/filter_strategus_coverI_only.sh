#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 || $# -gt 4 ]]; then
  echo "Usage: $0 <input.json> <output.json> [target_outcome_id] [source_outcome_id]" >&2
  echo "Default: source_outcome_id=13, target_outcome_id=27" >&2
  exit 2
fi

input="$1"
output="$2"
target_outcome_id="${3:-27}"
source_outcome_id="${4:-13}"
cohort_json="inst/cohorts/27_[O] Pandemic Prediction intensive services or death.json"
cohort_name="[O] Pandemic Prediction intensive services or death"

if [[ ! -f "$input" ]]; then
  echo "Missing input JSON: $input" >&2
  exit 1
fi
if [[ ! -f "$cohort_json" ]]; then
  echo "Missing corrected COVER-I cohort JSON: $cohort_json" >&2
  exit 1
fi

mkdir -p "$(dirname "$output")"

if jq -e '.moduleSpecifications[1].settings.modelDesignList != null' "$input" >/dev/null; then
  list_path='modelDesignList'
  before=$(jq '.moduleSpecifications[1].settings.modelDesignList | length' "$input")
  jq \
    --argjson src_id "$source_outcome_id" \
    --argjson target_id "$target_outcome_id" \
    --arg cohort_name "$cohort_name" \
    --rawfile cohort_def "$cohort_json" \
    '(.moduleSpecifications[1].settings.modelDesignList) |= (map(select(.outcomeId == $src_id or .outcomeId == $target_id) | .outcomeId = $target_id)) |
     (.sharedResources[0].cohortDefinitions) |= (map(select(.cohortId != $src_id and .cohortId != $target_id)) + [{cohortId: $target_id, cohortName: $cohort_name, cohortDefinition: $cohort_def}])' \
    "$input" > "$output"
elif jq -e '.moduleSpecifications[1].settings.validationList != null' "$input" >/dev/null; then
  list_path='validationList'
  before=$(jq '.moduleSpecifications[1].settings.validationList | length' "$input")
  jq \
    --argjson src_id "$source_outcome_id" \
    --argjson target_id "$target_outcome_id" \
    --arg cohort_name "$cohort_name" \
    --rawfile cohort_def "$cohort_json" \
    '(.moduleSpecifications[1].settings.validationList) |= (map(select(.outcomeId == $src_id or .outcomeId == $target_id) | .outcomeId = $target_id)) |
     (.sharedResources[0].cohortDefinitions) |= (map(select(.cohortId != $src_id and .cohortId != $target_id)) + [{cohortId: $target_id, cohortName: $cohort_name, cohortDefinition: $cohort_def}])' \
    "$input" > "$output"
else
  echo "Could not find modelDesignList or validationList in $input" >&2
  exit 1
fi

after=$(jq ".moduleSpecifications[1].settings.${list_path} | length" "$output")
outcomes=$(jq -r ".moduleSpecifications[1].settings.${list_path} | [.[].outcomeId] | unique | join(\",\")" "$output")
cohorts=$(jq -r '.sharedResources[0].cohortDefinitions | map(.cohortId) | sort | join(",")' "$output")

echo "Input:  $input"
echo "Output: $output"
echo "Kept $after of $before $list_path entries; rewrote outcomeId $source_outcome_id -> $target_outcome_id where needed."
echo "Task outcome IDs: ${outcomes:-none}"
echo "Embedded cohort IDs: $cohorts"
if [[ "$after" == "0" ]]; then
  echo "WARNING: no entries kept." >&2
fi
