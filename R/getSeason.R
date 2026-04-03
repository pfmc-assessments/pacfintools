#' Add a column for season.
#'
#' Several seasonal schemes are available, including Petrale seasons
#' (1 = winter months, 2 else).
#' Most assessments won't require `getSeason` because it is included
#' in [cleanPacFIN]. If a specialized season structure is required,
#' `getSeason` should be run immediately after [cleanPacFIN].
#'
#' @param Pdata A data object of biological data created using [cleanPacFIN()]].
#' @param season_type Specify a `numeric` value for season type.
#' If negative then all months will be assigned to season `1`.
#' If `0` then seasons will be assigned from `Pdata$SAMPLE_MONTH`,
#' where each month is a unique season.
#' If `1` then seasons are assigned according to methods used for Petrale,
#' where winter months (`c(11:12, 1:2)`) are season `1` and
#' the remaining months (summer) are assigned to season `2`.
#' Please contact the package maintainer should you wish to include an
#' additional seasonal scheme.
#' @param yearUp Used to provide a list of months (i.e., `1:12`)
#' for which to adjust the year (`Pdata$fishyr`) up. For example,
#' if winter months belong to the following year then use `yearUp = 11:12`.
#' @param yearDown Used to provide a list of months (i.e., `1:12`)
#' for which to adjust the year (`Pdata$fishyr`) down. For example,
#' if winter months belong to the previous year then use `yearUp = 1:2`.
#' @param plotResults Deprecated. A logical value specifying if plots should or should not
#' be created and shown in the console.
#' @param savedir Directory where output plots should be saved. The default
#' argument is NULL.
#' @inheritParams cleanPacFIN
#'
#' @return An additional column `season` is added to `Pdata`.
#' No columns are modified.
#' @export
#' @author Andi Stephens, Kelli F. Johnson
#' @examples
#' test <- getSeason(
#'   data.frame(SAMPLE_MONTH = 1:12, fishyr = rep(1:2, each = 6)),
#'   verbose = TRUE
#' )
#' testthat::expect_true(all(test[, "season"] == 1))
#' test <- getSeason(Pdata = test, season_type = 1, yearUp = 12)
#' testthat::expect_equivalent(test[test[, "fishyr"] == 3, "season"], 1)
#'
getSeason <- function(
  Pdata,
  season_type = -1,
  yearUp = NULL,
  yearDown = NULL,
  plotResults = lifecycle::deprecated(),
  savedir = NULL,
  verbose = TRUE
) {
  if (lifecycle::is_present(plotResults)) {
    lifecycle::deprecate_warn(
      when = "0.4.1",
      what = paste0("getSeason(plotResults = )"),
      details = "Please use getSeason(savedir = ) to save the plot to disk instead of showing it in the console."
    )
  }
  if (season_type < 0) {
    Pdata$season <- 1
  }

  if (season_type == 0) {
    if (verbose) {
      cli::cli_inform("Assigning season from SAMPLE_MONTH.")
    }
    Pdata[, "season"] <- utils::type.convert(as.is = TRUE, Pdata$SAMPLE_MONTH)
  } # End if

  # Petrale seasons

  if (season_type == 1) {
    if (verbose) {
      cli::cli_inform(
        "Assigning seasons for winter == 1 (month 11, 12, 1, 2) and summer == 2 month (3-10)."
      )
    }

    Pdata[, "season"] <- ifelse(
      Pdata[, "SAMPLE_MONTH"] %in% c(11:12, 1:2),
      1,
      2
    )
  } # End if Petrale

  if (!is.null(yearUp)) {
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] <-
      Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] + 1

    if (verbose) {
      year_message <- paste(yearUp, collapse = ", ")
      cli::cli_inform(
        "Incremented fishyr for months {year_message} to the next year."
      )
    }
  } # End if yearUp

  if (!is.null(yearDown)) {
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] <-
      Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] - 1

    if (verbose) {
      year_message <- paste(yearDown, collapse = ", ")
      cli::cli_inform(
        "Decremented fishyr for months {year_message} to the previous year."
      )
    }
  } # End if yearDown

  if (!is.null(savedir)) {
    plot_data <- Pdata |>
      dplyr::mutate(
        season = as.factor(season),
        Count = 1
      )
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = year, y = Count, fill = season)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_viridis_d(begin = 0, end = 0.5) +
      ggplot2::theme_bw() +
      ggplot2::xlab("Year")
    ggplot2::ggsave(
      plot = p,
      filename = file.path(savedir, "PacFIN_comp_season.png"),
      height = 7,
      width = 7,
      units = "in"
    )
  } # End if plotResults

  return(Pdata)
}
