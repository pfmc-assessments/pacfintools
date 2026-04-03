#' Create a state column based on input column specified in `source`
#'
#'
#' @details
#' Create a categorical column, `state`, based upon the `AGENCY_CODE`.
#' It is no longer advisable as of February 14, 2021 to create states based on
#' `PSMFC_CATCH_AREA_CODE` or `PSMFC_ARID` because areas are not mutually
#' exclusive to a state. Previous code set areas `1[a-z]` to Washington,
#' `2[a-z]` to Oregon, and `3[a-z]` to California.
#' The [PacFIN documentation](https://pacfin.psmfc.org/wp-content/uploads/2019/03/PacFIN_Comprehensive_BDS_Commercial.pdf)
#' suggests that the following area codes can be assigned to the following states:
#' * WA: 1C, 2A, 2B, 2C, 2E, 2F, 3A, 3B, 3C, 3N, 3S
#' * OR: 1C, 2A, 2B, 2C, 2E, 2F, 3A, 3B, CS
#' * CA: 1A, 1B, 1C
#'
#' However, the look-up table for [PacFIN Catch Area Code](https://apex.psmfc.org/pacfin/f?p=501:817:3123899543376:INITIAL)
#' indicates that they should be:
#' * WA: 3A, 3B, 3C, 3S
#' * OR: 2A, 2B, 2C, 2E, 2F, 3A
#' * CA: 1A, 1B, 1C
#'
#' If you see the need to use PSMFC_CATCH_AREA_CODE to set states
#' please contact the package maintainer.
#'
#' @export
#' @seealso [cleanPacFIN] calls `getState`.
#'
#' @inheritParams cleanPacFIN
#' @param source The column name where state information is located in
#' \code{Pdata}. See the function call for options, where only the first
#' value will be used.
#' @inheritParams cleanPacFIN
#'
#' @return The input data frame is returned with an additional column,
#' `state`, which is filled with two-character values identifying the state or
#' a three-character value `UNK` for all rows that do not have an assigned state.
#' All rows are returned, but users should pay attention to the warning that is
#' returned for rows that have no state id.
#'
#' @examples
#' data <- data.frame(
#'   AGENCY_CODE = rep(c("W", "O", "C"), each = 2),
#'   info = 1:6
#' )
#' testthat::expect_true(
#'   all(getState(data)[["state"]] == rep(c("WA", "OR", "CA"), each = 2))
#' )
#'
getState <- function(
  Pdata,
  source = c("AGENCY_CODE"),
  verbose = TRUE
) {
  if (any(source %in% c("PSMFC_CATCH_AREA_CODE", "PSMFC_ARID"))) {
    cli::cli_abort(
      "'PSMFC_CATCH_AREA_CODE' and 'PSMFC_ARID' are no longer supported inputs to getState(source = )."
    )
  }
  source <- match.arg(source, several.ok = FALSE)
  colid <- match(source, colnames(Pdata))
  if (is.na(colid[1])) {
    cli::cli_inform("The column {source} was not found in Pdata.")
  }

  Pdata$state <- as.character(Pdata[, source])

  Pdata[, "state"] <- vapply(
    Pdata[, "state"],
    FUN = switch,
    FUN.VALUE = "character",
    C = "CA",
    CalCOM = "CA",
    CALCOM = "CA",
    O = "OR",
    W = "WA",
    "UNK"
  )
  states <- c("OR", "CA", "WA")
  nostate <- sum(!Pdata[, "state"] %in% states)

  if (verbose) {
    cli::cli_bullets(c(
      " " = "{.fn getState} summary information -",
      "i" = "There are {nostate} records for which the state (i.e., CA, OR, WA) could not be assigned and were labeled as UNK."
    ))
  } # End if verbose

  return(Pdata)
}
