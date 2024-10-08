# creates covariate importance with columns: 
# covariateId, covariateName, analysisId, conceptId and covariateValue
createCovariateImportance <- function(results) { 
  covariateImportance <- results$model$varImp
  return(covariateImportance)
}

createModel <- function(results) {
  model <- list()
  model["priorVariance"] <- results$model$model$priorVariance
  model["log_likelihood"] <- results$model$model$log_likelihood
  model["modelType"] <- results$model$model$modelType
  model["modelStatus"] <- results$model$model$modelStatus
  model["intercept"] <- results$model$model$coefficients["(Intercept)"]
  inputCoefficients <- results$model$model$coefficients
  inputCoefficients <- inputCoefficients[names(inputCoefficients) != "(Intercept)"]
  covariateIds <- as.numeric(names(inputCoefficients)) 
  coefficient <- as.numeric(inputCoefficients)
  model[["coefficients"]] <- data.frame(covariateIds=covariateIds, coefficient=coefficient)
  return(model)
}

createModelDesign <- function(results) {
  modelDesign <- list()
  modelDesign["targetId"] <- results$model$cohortId
  modelDesign["outcomeId"] <- results$model$outcomeId
  restrictPlpDataSettings <- list(studyStartDate = "",
                                  studyEndDate = "",
                                  firstExposureOnly = FALSE,
                                  washoutPeriod = 0,
                                  sampleSize = NULL)
  class(restrictPlpDataSettings) <- "restrictPlpDataSettings"
  modelDesign[["restrictPlpDataSettings"]] <- restrictPlpDataSettings
  covariateSettings <- results$model$metaData$call$covariateSettings
  modelDesign[["covariateSettings"]] <- covariateSettings
  populationSettings <- results$model$populationSettings
  populationSettings[c("cohortId", "studyStartDate", 
                       "studyEndDate", "attrition", "outcomeId")] <- NULL
  modelDesign[["populationSettings"]] <- populationSettings
  sampleSettings <- list(numbersOutcomestoNonOutcomes = 1,
                         sampleSeed = 1)
  attr(sampleSettings, "class") <- "sampleSettings"
  attr(sampleSettings, "fun") <- "sameData"
  modelDesign[["sampleSettings"]] <- sampleSettings
  featureEngineeringSettings <- list()
  attr(featureEngineeringSettings, "class") <- "featureEngineeringSettings"
  attr(featureEngineeringSettings, "fun") <- "sameData"
  
  preprocessSettings <- list(minFraction = 0.001,
                             normalize = TRUE,
                             removeRedundancy = TRUE)
  attr(preprocessSettings, "class") <- "preprocessSettings"
  modelDesign[["preprocessSettings"]] <- preprocessSettings
  modelSettings <- list(fitFunction = NA,
                        param = list())
  attr(modelSettings, "class") <- "modelSettings"
  attr(modelSettings, "settings") <- list(modelType = "GLM",
                                                seed = 1,
                                                name = "Existing GLM")
  attr(modelSettings$param, "modelType") <- "binary"
  attr(modelSettings$param, "saveType") <- "RToJson"
  modelDesign[["modelSettings"]] <- modelSettings
  splitSettings <- list(test = results$inputSetting$testFraction,
                        train = 1 - results$inputSetting$testFraction,
                        seed = results$inputSetting$splitSeed,
                        nfold = results$inputSetting$nfold)
  attr(splitSettings, "class") <- "splitSettings"
  attr(splitSettings, "fun") <- "subjectSplitter"
  modelDesign[["splitSettings"]] <- splitSettings
  executeSettings <- list(runSplitData = TRUE,
                          runSampleData = TRUE,
                          runFeatureEngineering = TRUE,
                          runPreprocessData = TRUE,
                          runModelDevelopment = TRUE,
                          runCovariateSummary = TRUE)
  attr(executeSettings, "class") <- "executeSettings"
  modelDesign[["executeSettings"]] <- executeSettings
  attr(modelDesign, "class") <- "modelDesign"
  return(modelDesign)
}

createPreprocessing <- function(results) {
  preprocessing <- list(requireDenseMatrix = FALSE)
  tidyCovariates <- results$model$metaData$preprocessSettings
  tidyCovariates$populationSize <- results$model$metaData$populationSize
  preprocessing[["tidyCovariates"]] <- tidyCovariates
  return(preprocessing)
}

createTrainDetails <- function(results) {
  trainDetails <- list(analysisId = results$analysisRef$analysisId,
                       analysisSource = "",
                       developmentDatabase = "optum_dod",
                       attrition = results$inputSetting$populationSettings$attrition,
                       trainingTime = results$executionSummary$TotalExecutionElapsedTime,
                       trainingDate = results$executionSummary$ExecutionDateTime,
                       modelName = "Existing GLM",
                       finalModelParameters = results$model$hyperParamSearch,
                       hyperParamSearch = data.frame(none = 1))
  return(trainDetails)
}
# extract seek cover models from downloaded result app on zenodo
# create jsons compatible with strategus v1

zenodoLocation <- "~/Downloads/Covid19CoverPrediction"

dataLocation <- file.path(zenodoLocation, "data")

# filter data driven models out
dataDrivenAnalysisIds <- c(1, 3, 4) # analysis 2 is hospitalization because of ARDS/Sepsis/AKI which wasn't used in seek cover

for (analysis in dataDrivenAnalysisIds) {
  workingDir <- file.path(dataLocation, paste0("Analysis_", analysis))
  results <- readRDS(file.path(workingDir, "plpResult.rds"))
  
  covariateImportance <- createCovariateImportance(results)
  model <- createModel(results)
  modelDesign <- createModelDesign(results)
  preprocessing <- createPreprocessing(results)
  trainDetails <- createTrainDetails(results)
  plpModel <- list(
    covariateImportance = covariateImportance,
    model = model,
    modelDesign = modelDesign,
    preprocessing = preprocessing,
    trainDetails = trainDetails
  )
  attr(plpModel, "class") <- "plpModel"
  attr(plpModel, "predictionFunction") <- "PatientLevelPrediction::predictGlm"
  attr(plpModel, "modelType") <- "binary"
  attr(plpModel, "saveType") <- "RtoJson"
  if (analysis == 1) {
    name <- "I" # intensive care from those hospitalized with pneumonia
  } else if (analysis == 3) {
    name <- "F" # death from those hospitalized with pneumonia
  } else if (analysis == 4) {
    name <- "H" # hospitalization from those with outpatient visit
  }
  PatientLevelPrediction::savePlpModel(plpModel, paste0("./inst/models/dataDriven", name))
}