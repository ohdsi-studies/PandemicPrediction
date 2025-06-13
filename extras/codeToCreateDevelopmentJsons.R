library(PatientLevelPrediction)
library(Strategus)
library(lubridate)

# COHORT GENERATOR MODULE ------------------------------------------------------
cohortDefinitions <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  cohortFileNameFormat = "%s_%s",
  cohortFileNameValue = c("cohortId", "cohortName")
)

cohortDefinitions <- cohortDefinitions |>
  dplyr::filter(!.data$cohortId %in% c(30, 12, 22, 23, 24)) # only used for development

# modify the cohort
cohortGeneratorModule <- CohortGeneratorModule$new()
cohortDefShared <- 
  cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitions)

cohortGeneratorModuleSpecifications <- 
  cohortGeneratorModule$createModuleSpecifications(
    generateStats = TRUE
  )

# Model development 
plpModule <- PatientLevelPredictionModule$new()

# demographics + cohort covariates
covariateSettings <- list(
  FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE),
  createCohortCovariateSettings(cohortName = "history of cancer",
                                cohortId = 15,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1),
  createCohortCovariateSettings(cohortName = "history of COPD",
                                cohortId = 16,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1),
  createCohortCovariateSettings(cohortName = "history of diabetes",
                                cohortId = 17,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1),
  createCohortCovariateSettings(cohortName = "history of heart disease",
                                cohortId = 18,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1),
  createCohortCovariateSettings(cohortName = "history of hyperlipidemia",
                                cohortId = 19,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1),
  createCohortCovariateSettings(cohortName = "history of hypertension",
                                cohortId = 20,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1),
  createCohortCovariateSettings(cohortName = "history of kidney disease",
                                cohortId = 21,
                                settingId = 1,
                                analysisId = 699,
                                startDay = -9999,
                                endDay = -1))


# demographics + DrugEraGroup + ConditionEraGroup in long and short windows
dataDrivenCovariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAgeGroup = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  useDrugGroupEraShortTerm  = TRUE,
  useConditionGroupEraLongTerm = TRUE,
  useConditionGroupEraShortTerm = TRUE,
  endDays = -1)


populationSettings <- createStudyPopulationSettings(binary = TRUE,
                                                    includeAllOutcomes = TRUE,
                                                    firstExposureOnly = FALSE,
                                                    washoutPeriod = 0,
                                                    removeSubjectsWithPriorOutcome = TRUE,
                                                    priorOutcomeLookback = 99999,
                                                    requireTimeAtRisk = FALSE,
                                                    minTimeAtRisk = 1,
                                                    riskWindowStart = 0,
                                                    startAnchor = "cohort start",
                                                    riskWindowEnd = 30,
                                                    endAnchor = "cohort start",
                                                    restrictTarToCohortEnd = FALSE)

preprocessSettings <- createPreprocessSettings(
  minFraction = 0.001,
  normalize = TRUE,
  removeRedundancy = TRUE
)

splitSettings <- createDefaultSplitSetting(splitSeed = 42)

restrictList <- list(
  # first year covid with same sample size as influenza model
  createRestrictPlpDataSettings(
    studyStartDate = "20200101",
    studyEndDate = "20201231",
    sampleSize = 150000
  ),
  # first three months of 2021
  createRestrictPlpDataSettings(
    studyStartDate = "20210101",
    studyEndDate = "20210331",
  ),
  # first 6 months of 2021
  createRestrictPlpDataSettings(
    studyStartDate = "20210101",
    studyEndDate = "20210630",
  ),
  # first 9 months of 2021
  createRestrictPlpDataSettings(
    studyStartDate = "20210101",
    studyEndDate = "20210930",
  ),
  # first year of 2021 without sampling
  createRestrictPlpDataSettings(
    studyStartDate = "20210101",
    studyEndDate = "20211231"
  )
)

modelDesignList <- list()
for (i in seq_along(restrictList)) {
  covidH <- PatientLevelPrediction::createModelDesign(
    targetId = 31, # covid
    outcomeId = 11, # death. - 14 severe, 13 critial
    populationSettings = populationSettings,
    restrictPlpDataSettings = restrictList[[i]],
    covariateSettings = covariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = TRUE
  )

  covidI <- PatientLevelPrediction::createModelDesign(
    targetId = 31,
    outcomeId = 13,
    restrictPlpDataSettings = restrictList[[i]],

    populationSettings = populationSettings,
    covariateSettings = covariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = TRUE
  )

  covidF <- PatientLevelPrediction::createModelDesign(
    targetId = 31,
    outcomeId = 11,
    restrictPlpDataSettings = restrictList[[i]],
    populationSettings = populationSettings,
    covariateSettings = covariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = TRUE
  )

  dataDrivenCovidH <- PatientLevelPrediction::createModelDesign(
    targetId = 31,
    outcomeId = 14,
    restrictPlpDataSettings = restrictList[[i]],
    populationSettings = populationSettings,
    covariateSettings = dataDrivenCovariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = TRUE
  )

  dataDrivenCovidI <- PatientLevelPrediction::createModelDesign(
    targetId = 12,
    outcomeId = 13,
    restrictPlpDataSettings = restrictList[[i]],
    populationSettings = populationSettings,
    covariateSettings = dataDrivenCovariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = TRUE
  )

  dataDrivenCovidF <- PatientLevelPrediction::createModelDesign(
    targetId = 12,
    outcomeId = 11,
    restrictPlpDataSettings = restrictList[[i]],
    populationSettings = populationSettings,
    covariateSettings = dataDrivenCovariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = TRUE
  )
  modelDesignList <- c(modelDesignList, 
                        list(covidH, 
                             covidI,
                             covidF,
                             dataDrivenCovidH,
                             dataDrivenCovidI,
                             dataDrivenCovidF))

}
  
plpModuleSpecs <- plpModule$createModuleSpecifications(modelDesignList = modelDesignList)

# ANALYSIS SPECIFICATIONS ------------------------------------------------------

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortDefShared) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(plpModuleSpecs)

ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path("inst/study_execution_jsons", "development.json"))
