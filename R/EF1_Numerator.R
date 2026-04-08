#' Calculate the numerator for the first level expansion factor
#'
#' Calculate the numerator for the first-level expansion factor, where
#' the numerator is the species-specific landing weight for a given sample.
#' Thus, if two clusters were sampled from a single trip,
#' they would both use the same landing weight.
#'
#' @details
#' Previously, `Trip_Sampled_Lbs` was calculated differently for each state.
#' For California, `Species_Percent_Sampled * WEIGHT_OF_LANDING_LBS`.
#' For Oregon, `Pdata$EXPANDED_SAMPLE_WEIGHT` and if missing, the same as California.
#' For Washington, `Pdata$WEIGHT_OF_LANDING_LBS`, or
#' `median(Pdata$WEIGHT_OF_LANDING_LBS)`.
#' Then, if all else failed, per-year, state-specific medians.
#'
#' Now, PacFIN works hard behind the scenes to provide species-specific landing
#' weights for each sampled fish. Therefore, we no longer rely on code to
#' calculate a fabricated landing weight. Species-specific landing weights are
#' available in either `EXPANDED_SAMPLE_WEIGHT` or `WEIGHT_OF_LANDING_LBS`.
#' The former, is specific to Oregon and samples that do not provide an expanded
#' sample weight should not be used more than likely anyway.
#'
#' **todo**:
#' * determine if we want to flag some bad samples in [cleanPacFIN].
#' * fix up the plotting and summary code
#'
#' @seealso [getExpansion_1] calls this function.
#' @return A \code{Pdata} with additional columns, where
#' `Trip_Sampled_Lbs` is the sample weight in pounds.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams cleanPacFIN
#' @template plot
#' @template savedir
#' @author Andi Stephens, Kelli F. Johnson, Chantel R. Wetzel

EF1_Numerator <- function(
  Pdata,
  verbose = TRUE,
  plot = lifecycle::deprecated(),
  savedir = NULL
) {
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "EF1_Denominator(plot)",
      details = "Please use savedir to create and save plots."
    )
  }
  # Define trip sample pounds based upon either EXPANDED_SAMPLE_WEIGHT or WEIGHT_OF_LANDING_LBS
  # Oregon primarily uses EXPANDED_SAMPLE_WEIGHT
  # California and Washington primarily use WEIGHT OF LANDED LBS
  Pdata$Trip_Sampled_Lbs <- dplyr::coalesce(
    Pdata[["EXPANDED_SAMPLE_WEIGHT"]],
    Pdata[["WEIGHT_OF_LANDING_LBS"]]
  )

  if (!is.null(savedir)) {
    plot_filename <- fs::path(
      savedir,
      "PacFIN_expansion_1_numerator.png"
    )
    g1 <- ggplot2::ggplot(
      Pdata,
      ggplot2::aes(x = as.factor(fishyr), y = Trip_Sampled_Lbs)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw() +
      ggplot2::ylab("Sample weight per trip (lbs)") +
      ggplot2::xlab("Year") +
      ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = TRUE)
      ) +
      ggplot2::ggtitle(
        "First Stage Expansion Numerator Before the Application of Maximum Quantile Cap"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 0.5
        )
      ) +
      ggplot2::facet_grid("state", scales = "free_y")
    ggplot2::ggsave(
      filename = plot_filename,
      plot = g1,
      height = 12,
      width = 12
    )
    plot_filename <- fs::path(
      savedir,
      "PacFIN_expansion_1_NA_numerator.png"
    )
    g2 <- ggplot2::ggplot(
      Pdata |>
        dplyr::filter(is.na(Trip_Sampled_Lbs)) |>
        dplyr::mutate(Count = 1),
      ggplot2::aes(x = as.factor(fishyr), y = Count)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::ylab("Count") +
      ggplot2::xlab("Year") +
      ggplot2::ggtitle(
        "First Stage Expansion Numerator with NA Trip_Sampled_Lbs"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 0.5
        )
      ) +
      ggplot2::facet_grid("state")
    ggplot2::ggsave(
      filename = plot_filename,
      plot = g2,
      height = 12,
      width = 12
    )
  }

  return(Pdata)
}
