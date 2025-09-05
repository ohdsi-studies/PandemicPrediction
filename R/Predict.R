#' A custom prediction function that applies stored recalibration coefficients.
#'
#' @description
#' This function is designed to be called by `PatientLevelPrediction::predictPlp`.
#' It first calls the model's original prediction function (e.g., `predictGlm`)
#' and then applies a weak recalibration transformation using coefficients
#' stored on the plpModel object itself.
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

  if (is.null(plpModel$recalibration)) {
    warning("No recalibration coefficients found on this model. Returning original predictions.")
    return(initialPrediction)
  }

  coeffs <- plpModel$recalibration$coefficients
  message(
    "Applying recalibration with Intercept: ",
    coeffs$adjustIntercept, ", Gradient: ", coeffs$adjustGradient
  )
  recalibratedValues <- initialPrediction$value

  epsilon <- .Machine$double.eps
  recalibratedValues <- pmin(pmax(recalibratedValues, epsilon), 1 - epsilon)

  finalValues <- stats::plogis(
    (stats::qlogis(recalibratedValues) * coeffs$adjustGradient) + coeffs$adjustIntercept
  )

  finalPrediction <- initialPrediction
  finalPrediction$value <- finalValues
  return(finalPrediction)
}
