#' Calculate weight from length and the weight--length relationship
#'
#' Estimate fish weights from potentially sex-specific weight--length
#' relationship parameters returned from [getWLpars].
#'
#' @param length A vector of fish lengths in mm.
#' @param sex A vector of sexes for each entry in \code{length}.
#' Entries must be one of the following: 'U', 'F', 'M', or 'H'.
#' @param pars A data frame of parameters for the weight--length
#' relationship as determined from empirical data. The data frame
#' must have columns of \code{'A'} and \code{'B'}, as well as
#' rows named \code{'U'}, at a minimum, \code{'F'}, and \code{'M'}.
#' The output from \code{\link{getWLpars}} is formatted correctly as is.
#' @param unit.out The desired units you wish the output to be in. See the
#' function call for a list of default units available within the function.
#' For example, `"lb"` would return the units of pounds.
#' @param weight A vector of fish weights, where the units for each
#' measurement are specified in `unit.in`.
#' @param unit.in A vector of units for each measurement in `weight`.
#' Options include `KG`, `G`, `P`, `UNK`. Where, the latter leads to
#' the assumption that your units are in grams and will be converted
#' as such.
#'
#' @export
#' @author Kelli F. Johnson
#' @return A vector of measurements in the unit specified using
#' `unit.out`. If weights were provided, then the weights are also
#' the output. If lengths were provided, then they are converted
#' to weights. Where, weights are determined from lengths and weight--length
#' parameters input to the function.
#' Weights are in the same units used to calculate things, i.e., kg.
#'
getweight <- function(
  length,
  sex,
  pars,
  weight,
  unit.in,
  unit.out = c("lb", "kg")
) {
  unit.out <- match.arg(unit.out, several.ok = FALSE)

  #### Option # 1 ... Change units of weight
  if (!missing(weight)) {
    if (all(is.na(weight))) {
      return(weight)
    }
    if (is.null(unit.in)) {
      cli::cli_alert_danger(
        "Guessing unit.in for getweight; please input a non-null vector."
      )
      if (all(weight > 300)) {
        unit.in <- rep("G", length(weight))
      } else {
        cli::cli_abort(
          "Not sure of units, please recall getweight and input a unit.in vector."
        )
      }
    }
    if (any(unit.in == "H", na.rm = TRUE)) {
      n <- sum(unit.in == "H", na.rm = TRUE)
      cli::cli_inform(
        "FISH_WEIGHT units of H are changed to G for {n} fish."
      )
      unit.in[unit.in == "H"] <- "G"
    }
    transformweight <- weight *
      mapply(
        switch,
        unit.in,
        MoreArgs = list(
          G = 0.00220462,
          KG = 2.20462,
          UNK = 0.00220462,
          P = 1,
          0.00220462
        )
      )
    if (unit.out == "kg") {
      transformweight <- transformweight * 0.453592
    }
    return(transformweight)
  }

  #### Option # 2 ... a * (length / 10)^b * 2.20462 [length = cm; weight = kg]
  #### Checks
  stopifnot(all(sex %in% c(NA, "U", "F", "M", "H")))
  if (length(length) != length(sex)) {
    cli::cli_abort(
      "The vectors, length and sex, must be equal in length."
    )
  }
  if (is.matrix(pars)) {
    pars <- data.frame(pars)
  }
  if ((!"H" %in% row.names(pars)) & "H" %in% sex & "all" %in% row.names(pars)) {
    pars["H", ] <- pars["all", ]
  }
  if ((!"H" %in% row.names(pars)) & "H" %in% sex) {
    cli::cli_abort(
      "H is in the sex vector but no parameters in pars are available."
    )
  }
  if (!"females" %in% rownames(pars)) {
    pars["F", ] <- pars["all", ]
  }
  if (!"males" %in% rownames(pars)) {
    pars["M", ] <- pars["all", ]
  }
  pars$SEX_CODE <- rownames(pars)
  pars$SEX_CODE <- gsub("(^f|^m).+", "\\U\\1", pars$SEX_CODE, perl = TRUE)
  pars$SEX_CODE <- gsub("all", "U", pars$SEX_CODE)

  #### Calculate weight assuming length is in mm
  calcweight <- (pars[match(sex, pars[, "SEX_CODE"]), "A"] *
    (length / 10)^(pars[match(sex, pars[, "SEX_CODE"]), "B"]))
  if (unit.out == "lb") {
    calcweight <- calcweight * 2.20462
  }

  #### return
  return(calcweight)
}
