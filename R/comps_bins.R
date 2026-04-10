#' Bin a vector of data into groups
#'
#' Bin a vector of data into distinct groups, this is often helpful
#' for grouping ages or lengths into bin categories if every year or
#' centimeter is not its own bin.
#'
#' @param vector A vector of information that will be manipulated.
#' @param breaks A vector of cut points to be passed to [cut].
#' @param includeplusgroup A logical defining if you want all values
#' larger than the largest `break` to be included in the last bin.
#' For example, if the largest value in `breaks` is 15 and you have
#' age-20 fish, then you should use `includeplusgroup = TRUE` if you
#' want to have data on this 20 year old fish. The default is to
#' include them.
#' @param returnclass The `class()` of the returned object. This is helpful
#' for those times when numeric values are characters or the opposite.
#' Sometimes you wish to force the results to be a certain class and
#' this argument can help define the class of the returned object. Note that
#' not all classes are always available. The default will be the first entry
#' in the function call.
#'
#' @export
#' @author Kelli F. Johnson
#' @examples
#' comps_bins(1:8, breaks = c(-Inf, 3:5))
#' comps_bins(1:8, breaks = c(3:5), includeplusgroup = FALSE)
#' testthat::expect_equal(
#'   comps_bins(1:8, breaks = c(-Inf, 3:5, Inf)),
#'   comps_bins(1:8, breaks = c(-Inf, 3:5), includeplusgroup = TRUE)
#' )
#'
comps_bins <- function(
  vector,
  breaks,
  includeplusgroup = TRUE,
  returnclass = c("character", "numeric")
) {
  returnclass <- match.arg(returnclass)
  breaks <- sort(utils::type.convert(breaks, as.is = TRUE))
  if (includeplusgroup && utils::tail(breaks, 1) != Inf) {
    breaks <- c(breaks, Inf)
  }
  out <- gsub(
    # Note that the hyphen must go last in the [] b/c it is
    # also used as a special character to represent ranges
    pattern = "[\\[\\(]([0-9\\Inf-]+),\\s*[0-9Inf\\)]+",
    replacement = "\\1",
    x = cut(vector, breaks = breaks, include.lowest = FALSE, right = FALSE)
  )
  out <- switch(returnclass,
    character = out,
    numeric = utils::type.convert(out, as.is = TRUE)
  )
  return(out)
}
