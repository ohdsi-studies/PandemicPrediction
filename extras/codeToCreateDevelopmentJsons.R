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

# modify the cohort
cohortGeneratorModule <- CohortGeneratorModule$new()
cohortDefShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitions)

cohortGeneratorModuleSpecifications <- cohortGeneratorModule$createModuleSpecifications(
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
                                cohortId = 15, # covariateId = cohortId * 100K + settingId * 1k + analysisId
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
  useConditionGroupEraShortTerm = TRUE)


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

firstYearCovid <- createRestrictPlpDataSettings(
  studyStartDate = "2020-01-01",
  studyEndDate = "2020-31-12"
)

covidH <- PatientLevelPrediction::createModelDesign(targetId = 12, 
                                                    outcomeId = 14,
                                                    restrictPlpDataSettings = firstYearCovid,
                                                    populationSettings = populationSettings,
                                                    covariateSettings =  covariateSettings,
                                                    preprocessSettings = preprocessSettings,
                                                    modelSettings = setLassoLogisticRegression(seed = 42),
                                                    splitSettings = splitSettings,
                                                    runCovariateSummary = TRUE)
                                                       
covidI <- PatientLevelPrediction::createModelDesign(
  targetId = 12,
  outcomeId = 13,
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = covariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

covidF <- PatientLevelPrediction::createModelDesign(
  targetId = 12,
  outcomeId = 11,
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = covariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

dataDrivenCovidH <- PatientLevelPrediction::createModelDesign(
  targetId = 12,
  outcomeId = 14,
  restrictPlpDataSettings = firstYearCovid,
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
  restrictPlpDataSettings = firstYearCovid,
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
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = dataDrivenCovariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

# influenza trained models, use sampling to same size as covid models ? 
coverH <- PatientLevelPrediction::createModelDesign(
  targetId = 30, 
  outcomeId = 14, 
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = covariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

coverI <- PatientLevelPrediction::createModelDesign(
  targetId = 30, 
  outcomeId = 13, 
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = covariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

coverF <- PatientLevelPrediction::createModelDesign(
  targetId = 30, 
  outcomeId = 11, 
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = covariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

dataDrivenCoverH <- PatientLevelPrediction::createModelDesign(
  targetId = 30, 
  outcomeId = 14, 
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = dataDrivenCovariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

dataDrivenCoverI <- PatientLevelPrediction::createModelDesign(
  targetId = 30, 
  outcomeId = 13, 
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = dataDrivenCovariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)

dataDrivenCoverF <- PatientLevelPrediction::createModelDesign(
  targetId = 30, 
  outcomeId = 11, 
  restrictPlpDataSettings = firstYearCovid,
  populationSettings = populationSettings,
  covariateSettings = dataDrivenCovariateSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = setLassoLogisticRegression(seed = 42),
  splitSettings = splitSettings,
  runCovariateSummary = TRUE
)


modelDesignList <- list(covidH, 
                        covidI,
                        covidF,
                        dataDrivenCovidH,
                        dataDrivenCovidI,
                        dataDrivenCovidF,
                        coverH,
                        coverI,
                        coverF,
                        dataDrivenCoverH,
                        dataDrivenCoverI,
                        dataDrivenCoverF) 
  
plpModuleSpecs <- plpModule$createModuleSpecifications(modelDesignList = modelDesignList)

# MODEL TRANSFER Module --------------------------------------------------------

localFileSettings <- data.frame(
  locations = "./inst/models/"
)

modelTransferModule <- ModelTransferModule$new()

modelTransferModuleSpecs <- modelTransferModule$createModuleSpecifications(localFileSettings = localFileSettings)

# ANALYSIS SPECIFICATIONS ------------------------------------------------------

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortDefShared) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(plpModuleSpecs)

ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path("study_execution_jsons", "development.json"))