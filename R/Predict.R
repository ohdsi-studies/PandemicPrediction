#' A custom prediction function that applies stored recalibration coefficients.
#'
#' @description
#' This function is designed to be called by `PatientLevelPrediction::predictPlp`.
#' It first calls the model's original prediction function (e.g., `predictGlm`)
#' and then applies a weak recalibration transformation using coefficients
#' stored on the plpModel object itself. In addition to returning calibrated
#' probabilities in the `value` column, this function also returns a
#' `linearPredictor` column so that rank-based metrics (e.g., AUROC) can be
#' computed on the linear predictor when available.
#'
#' @param plpModel    The plpModel object. This object is expected to have a
#'                    `$recalibration` list containing `$coefficients`. Which are
#' the weak recalibration intercept and gradient.
#' @param data        An object of type `plpData`.
#' @param cohort      The cohort to predict on.
#'
#' @return
#' A prediction data frame with the recalibrated risk values.
#' @export
predictWithRecalibration <- function(plpModel, data, cohort) {
  originalPredictionFunction <- attr(plpModel, "originalPredictionFunction")

  message(
    "predictWithRecalibration: Calling base function '",
    originalPredictionFunction, "'"
  )

  initialPrediction <- do.call(
    eval(parse(text = originalPredictionFunction)),
    list(
      plpModel = plpModel,
      data = data,
      cohort = cohort
    )
  )

  # Derive the original linear predictor (log-odds). Prefer provided column; fall back to qlogis(prob).
  epsilon <- .Machine$double.eps
  if ("linearPredictor" %in% colnames(initialPrediction)) {
    originalLP <- initialPrediction$linearPredictor
  } else {
    # Compute from probability, guarding against 0/1
    baseProb <- initialPrediction$value
    baseProb <- pmin(pmax(baseProb, epsilon), 1 - epsilon)
    originalLP <- stats::qlogis(baseProb)
  }

  # If no recalibration info is present, return original predictions but include linearPredictor
  if (is.null(attr(plpModel, "recalibration"))) {
    ParallelLogger::logWarn("No recalibration coefficients found on this model. Returning original predictions (with linearPredictor).")
    finalPrediction <- initialPrediction
    if (!("linearPredictor" %in% colnames(finalPrediction))) {
      finalPrediction$linearPredictor <- originalLP
    }
    return(finalPrediction)
  }

  # Apply weak recalibration in log-odds space and map back to probability
  coeffs <- attr(plpModel, "recalibration")$coefficients
  message(
    "Applying recalibration with Intercept: ",
    coeffs$adjustIntercept, ", Gradient: ", coeffs$adjustGradient
  )

  recalibratedLP <- (originalLP * coeffs$adjustGradient) + coeffs$adjustIntercept
  finalValues <- stats::plogis(recalibratedLP)

  finalPrediction <- initialPrediction
  finalPrediction$linearPredictor <- recalibratedLP
  finalPrediction$value <- finalValues
  return(finalPrediction)
}
