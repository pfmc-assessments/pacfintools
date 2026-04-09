#' Table of Sample Size By Fleet and Year
#'
#' Create a table of samples by fleet and year to
#' be included in an assessment document.
#'
#' @inheritParams cleanPacFIN
#' @param Pdata  A data frame returned from [cleanPacFIN()], [get_expanded_comps()],
#'   [getExpansion_1()], [getExpansion_2], or [getComps()]. If a data frame returned
#'   from [getComps()] is provided a table of samples by year and fleet with trips,
#'   samples, and input sample sizes will be returned.  If a data frame returned from
#'   the other functions listed, a table of samples by year and `stratification.cols`
#'   for lengths and ages will be returned.
#' @param savedir A file path to the directory where the results will be saved.
#'   The default is NULL.
#' @param stratification.cols Columns in `Pdata` to summarize the data for the
#'   calculation of samples sizes.  These columns are not used in the data
#'   frame is from [getComps()] because the grouping structure is already set in
#'   that data frame.
#' @param strat Deprecated. A vector of column names to stratify the output over. For
#'   example, if you wish to summarize by ageing method, then the argument
#'   would look like `strat = "agemethod"` or, if you want to look at fleets
#'   and gear, `strat = c("fleet", "usegear")`.
#' @param comps Deprecated. Specify whether to calculate the length or Age samples.
#' The default is to calculate the number of length samples.
#' @param remove_yrs Deprecated. A vector of years to remove from the data before
#' summarizing sample sizes. The default of \code{NULL} leads to no
#' sub setting of the data.
#' @param fname Deprecated. A filename with the appropriate extension, used to save the
#'   function output to the disk. Full, relative, or simple paths are
#'   allowed because the argument is used as is, i.e., not redirected to a
#'   directory different than [getwd()].
#'
#' @return
#' A data frame is returned of samples by year
#'
#' @author Chantel R. Wetzel
#' @export
#'
tableSample <- function(
  Pdata,
  stratification.cols = c("state", "geargroup"),
  savedir = NULL,
  verbose = TRUE,
  comps = lifecycle::deprecated(),
  remove_yrs = lifecycle::deprecated(),
  fname = lifecycle::deprecated(),
  strat = lifecycle::deprecated()
) {
  if (lifecycle::is_present(fname)) {
    lifecycle::deprecate_warn(
      when = "0.4.3",
      what = "tableSample(fname = )",
      details = "fname is deprecated, files are automatically named internally"
    )
  }
  if (lifecycle::is_present(strat)) {
    lifecycle::deprecate_warn(
      when = "0.4.3",
      what = "tableSample(strat = )",
      details = "strat is deprecated, please use stratification.cols"
    )
    stratification.cols <- strata
  }
  if (lifecycle::is_present(remove_yrs)) {
    lifecycle::deprecate_warn(
      when = "0.4.3",
      what = "tableSample(strat = )",
      details = "remove_yrs is deprecated, please filter data outside of function"
    )
  }
  if (lifecycle::is_present(comps)) {
    lifecycle::deprecate_warn(
      when = "0.4.3",
      what = "tableSample(comps = )",
      details = "comp is deprecated, if passing an object from the cleanPacFIN or 
      either of the expansion function will result in a table of lengths and ages, 
      otherwise, a table of trips, fish, and input sample size will be created 
      for either lengths or ages dependent upon the data type in the getComps() object."
    )
  }
  nwfscSurvey::check_dir(dir = savedir, verbose = verbose)

  # Evaluate whether the user wants raw samples sizes of summary of processed comps
  if ("n_stewart" %in% colnames(Pdata)) {
    file_name <- "commercial_trips_and_input_sample_sizes.csv"
    samples <- Pdata |>
      tibble::as_tibble() |>
      dplyr::summarise(
        .by = c(fishyr, fleet),
        trips = sum(unique(n_tows)),
        samples = sum(unique(n_fish)),
        input_n = sum(unique(n_stewart))
      ) |>
      dplyr::rename(
        year = fishyr
      ) |>
      dplyr::arrange(fleet) |>
      as.data.frame()
  } else {
    file_name <- "commercial_length_age_samples.csv"
    samples <- Pdata |>
      tibble::as_tibble() |>
      dplyr::summarise(
        .by = dplyr::all_of(c("fishyr", stratification.cols)),
        n_length = sum(!is.na(lengthcm)),
        n_age = sum(!is.na(Age))
      ) |>
      dplyr::rename(
        year = fishyr
      ) |>
      as.data.frame()
  }

  if (!is.null(savedir)) {
    utils::write.csv(
      x = samples,
      file = file.path(savedir, file_name),
      row.names = FALSE
    )
  }

  return(samples)
}
