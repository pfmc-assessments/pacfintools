#' First-Stage expansion for composition data
#'
#' First-stage expansions account for unsampled fish in the smallest measured
#' unit. Where, in PacFIN data, the smallest measured unit typically a trip
#' because that is what is available to the port or dockside sampler. Whereas,
#' in survey data the smallest measured unit is typically a tow. Tow would be
#' the smallest if we had samples from onboard observers rather than from
#' dockside samplers.
#'
#' @export
#'
#' @author Andi Stephens, Kelli F. Johnson, Chantel R. Wetzel
#'
#' @seealso
#' * [cleanPacFIN()] (upstream)
#' * [nwfscSurvey::estimate_weight_length()] (upstream)
#' * [EF1_Numerator()] (contained within)
#' * [EF1_Denominator()] (contained within)
#' * [getExpansion_2()] (downstream)
#'
#' @details
#' The workflow is to run this function after [cleanPacFIN()],
#' which assures that all of the necessary columns are available and that the
#' data are in the correct units. This function then calls two helper functions,
#' [EF1_Numerator()] and [EF1_Denominator()] to calculate the weight of sampled
#' fish and the weight of all fish of the respective species in the tow,
#' respectively. Finally, the ratio of the two values is returned. Alternatively,
#' users should consider using [get_pacfin_expansions()] which is a wrapper
#' function that calls both the first and second stage functions for expanding
#' composition data.
#'
#' @section Expansion:
#' \itemize{
#' \item{Age data are expanded separately from lengths.}
#' \item{WA fish are generally only expanded using Expansion_Factor_2.}
#' \item{Other expansions are the product of
#'   Expansion_Factor_1 * Expansion_Factor_2.
#' }
#' \item{For age-at-length comps, set Final_Expansion_Factor to 1 because
#'   each fish represents only itself.}
#' }
#'
#' @inheritParams cleanPacFIN
#' @param maxExp The maximum expansion factor (either a number or a quantile)
#'   for building expansions. Typically, the default is 0.95. Set `maxExp =
#'   Inf` to see largest values.
#' @param Exp_WA A logical values specifying if the samples from Washington
#'   should be exanded. The default is `FALSE`.
#' @param fa,ma,ua Female-, male-, and unsexed-specific weight--length
#'   coefficients for Stock Synthesis where the relationships were calculated
#'   using length in cm and weight in kg. There are no default values. You must
#'   calculate these values and pass them to the function. If a particular sex
#'   does not pertain to your data, then just pass `NA` for that relationship.
#' @param fb,mb,ub Female-, male, and unsexed-specific weight--length exponents
#'   for Stock Synthesis where the relationships were calculated using length in
#'   cm and weight in kg. If a particular sex does not pertain to your data,
#'   then just pass `NA` for that relationship.
#' @param plot Deprecated. Typically, a logical is passed defining if you would like the
#'   default plots to be created but users can also pass a string providing a
#'   path to a directory where those plots will be stored. The default is
#'   `FALSE` and no figures will be created unless this is changed. If `TRUE` is
#'   passed, then the figures will be saved to your current working directory.
#' @param savedir A file path to the directory where the results will be saved.
#' The default is NULL.
#'
#' @examples
#' \dontrun{
#' # Calculate the weight--length parameters for input to this function
#' bds_survey <- nwfscSurvey::pull_bio(
#'   common_name = "widow rockfish",
#'   survey = "NWFSC.Combo"
#' )
#' pars <- nwfscSurvey::estimate_weight_length(
#'   data = bds_survey,
#'   col_length = "length_cm",
#'   col_weight = "weight_kg",
#'   verbose = FALSE
#' )
#' data_exp1 <- getExpansion_1(
#'   Pdata = bds_cleaned,
#'   fa = weight_length_estimates |>
#'     dplyr::filter(sex == "female") |>
#'     dplyr::pull("A"),
#'   fb = weight_length_estimates |>
#'     dplyr::filter(sex == "female") |>
#'     dplyr::pull("B"),
#'   ma = weight_length_estimates |>
#'     dplyr::filter(sex == "male") |>
#'     dplyr::pull("A"),
#'   mb = weight_length_estimates |>
#'     dplyr::filter(sex == "female") |>
#'     dplyr::pull("B"),
#'   ua = weight_length_estimates |>
#'     dplyr::filter(sex == "all") |>
#'     dplyr::pull("A"),
#'   ub = weight_length_estimates |>
#'     dplyr::filter(sex == "all") |>
#'     dplyr::pull("B"),
#'   maxExp = 0.95
#' )
#' }
#' @return
#' A `data.frame` where all of the original columns in `Pdata` remain unaltered
#' but additional columns are added. In particular columns starting with
#' `Expansion_Factor_1` are available for setting the `Final_Expansion_Factor`.
#'
getExpansion_1 <- function(
  Pdata,
  fa,
  fb,
  ma,
  mb,
  ua,
  ub,
  maxExp = 0.95,
  Exp_WA = TRUE,
  verbose = TRUE,
  plot = lifecycle::deprecated(),
  savedir = NULL
) {
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_soft(
      when = "0.2.10",
      what = "getExpansion_1(plot)",
      details = "Please use savedir to create and save plots."
    )
  }

  nwfscSurvey::check_dir(dir = savedir, verbose = verbose)

  Pdata <- EF1_Denominator(
    Pdata = Pdata,
    fa = fa,
    fb = fb,
    ma = ma,
    mb = mb,
    ua = ua,
    ub = ub,
    verbose = verbose,
    savedir = savedir
  )

  # Get Trip_Sampled_Lbs
  Pdata <- EF1_Numerator(
    Pdata = Pdata,
    verbose = verbose,
    savedir = savedir
  )

  # Expansion_Factor_1
  Pdata$Expansion_Factor_1_L <- Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled_L
  Pdata$Expansion_Factor_1_A <- Pdata$Trip_Sampled_Lbs / Pdata$Wt_Sampled_A

  Pdata$Expansion_Factor_1_L[Pdata$Expansion_Factor_1_L < 1] <- 1
  Pdata$Expansion_Factor_1_A[Pdata$Expansion_Factor_1_A < 1] <- 1

  # In most cases, WA data can't be expanded.
  if (Exp_WA != TRUE) {
    Pdata$Expansion_Factor_1_L[Pdata$state == "WA"] <- 1
    Pdata$Expansion_Factor_1_A[Pdata$state == "WA"] <- 1
    cli::cli_alert_info(
      #"i" = "Fish tickets do not represent whole trips in WA.",
      "Washington state expansions set to 1 because {.code Exp_WA = {Exp_WA}}."
    )
  }

  # Used in plotting later on
  NA_EF1 <- Pdata[is.na(Pdata$Expansion_Factor_1_L), ]
  nNA <- sum(is.na(Pdata$Expansion_Factor_1_L))
  # Counts to report
  n_na <- sum(is.na(Pdata$Expansion_Factor_1_L))
  n_inf <- sum(
    !is.na(Pdata$Expansion_Factor_1_L) &
      !is.finite(Pdata$Expansion_Factor_1_L)
  )
  Pdata$replace_na_inf <- FALSE
  replace <- c(
    which(is.na(Pdata$Expansion_Factor_1_L)),
    which(
      !is.na(Pdata$Expansion_Factor_1_L) &
        !is.finite(Pdata$Expansion_Factor_1_L)
    )
  )
  Pdata$replace_na_inf[replace] <- TRUE
  if (verbose) {
    na_year <- table(Pdata$fishyr[is.na(Pdata$Expansion_Factor_1_L)])
    inf_year <- table(Pdata$fishyr[
      !is.na(Pdata$Expansion_Factor_1_L) &
        !is.finite(Pdata$Expansion_Factor_1_L)
    ])

    message_na <- paste0(
      names(na_year),
      ": ",
      na_year
    )
    message_inf <- paste0(
      names(inf_year),
      ": ",
      inf_year
    )
    if (n_na / dim(Pdata)[1] > 0.01) {
      cli::cli_bullets(c(
        "x" = "{n_na} {.code NA} Expansion_Factor_1 values replaced by 1. Number of samples by year:",
        " " = "{message_na}"
      ))
    } else {
      cli::cli_bullets(c(
        "!" = "{n_na} {.code NA} Expansion_Factor_1 values replaced by 1. Number of samples by year:",
        " " = "{message_na}"
      ))
    }
    if (n_inf / dim(Pdata)[1] > 0.01) {
      # Report the number of 1/inf per year
      cli::cli_bullets(c(
        "x" = "{n_inf} not finite numbers and Expansion_Factor_1 values replaced by 1. Number of samples by year:",
        " " = "{message_inf}"
      ))
    } else {
      cli::cli_bullets(c(
        "!" = "{n_inf} not finite numbers and Expansion_Factor_1 values replaced by 1. Number of samples by year:",
        " " = "{message_inf}"
      ))
    }
    # Report the number of 1/inf per year
    cli::cli_bullets(c(
      " " = "These represent records that could not be expanded due to missing sample weight
      or landing weight. Depending upon the magnitude and distribution across years, users may want to remove these records.",
      " " = "These records are noted in the replace_na_inf column as TRUE."
    ))
  }

  # Now replace NAs with 1.
  Pdata$Expansion_Factor_1_L[is.na(Pdata$Expansion_Factor_1_L)] <- 1
  Pdata$Expansion_Factor_1_A[is.na(Pdata$Expansion_Factor_1_A)] <- 1
  # Now replace Inf with 1
  Pdata$Expansion_Factor_1_L[!is.finite(Pdata$Expansion_Factor_1_L)] <- 1
  Pdata$Expansion_Factor_1_A[!is.finite(Pdata$Expansion_Factor_1_A)] <- 1

  max_quantile_length <- round(
    stats::quantile(Pdata$Expansion_Factor_1_L, 1),
    2
  )
  max_quantile_age <- round(stats::quantile(Pdata$Expansion_Factor_1_A, 1), 2)
  Pdata$Expansion_Factor_1_L <- capValues(Pdata$Expansion_Factor_1_L, maxExp)
  Pdata$Expansion_Factor_1_A <- capValues(Pdata$Expansion_Factor_1_A, maxExp)
  if (verbose) {
    cli::cli_bullets(c(
      "i" = "Maximum first-stage length expansion is {max_quantile_length} and is capped at the {maxExp} quantile of {round(max(Pdata$Expansion_Factor_1_L), 2)}",
      "i" = "Maximum first-stage age expansion is {max_quantile_age} and is capped at the {maxExp} quantile of {round(max(Pdata$Expansion_Factor_1_A), 2)}"
    ))
  }

  # Generate plots and save them to the disk if specified.
  # TODO: move away from {grDevices}
  if (!is.null(savedir)) {
    plot_name1 <- file.path(savedir, "PacFIN_expansion_1_NA_Inf.png")
    plot_name2 <- file.path(savedir, "PacFIN_expansion_1.png")
    if (nNA > 0) {
      g1 <- ggplot2::ggplot(
        Pdata |>
          dplyr::mutate(
            Count = dplyr::case_when(replace_na_inf == TRUE ~ 1, .default = 0)
          ),
        ggplot2::aes(x = as.factor(fishyr), y = Count, fill = state)
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::theme_bw() +
        ggplot2::ylab("Count") +
        ggplot2::xlab("Year") +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::ggtitle(
          "First Stage Expansion that are NA of Inf"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 0.5
          )
        )
      ggplot2::ggsave(
        filename = plot_name1,
        plot = g1,
        height = 9,
        width = 9
      )
    }
    g2 <- ggplot2::ggplot(
      Pdata,
      ggplot2::aes(x = as.factor(fishyr), y = Expansion_Factor_1_L)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw() +
      ggplot2::ylab("Expansion Factor") +
      ggplot2::xlab("Year") +
      ggplot2::ggtitle(
        "First Stage Expansion"
      ) +
      ggplot2::facet_grid("state", scales = "free_y") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 0.5
        )
      )
    ggplot2::ggsave(
      filename = plot_name2,
      plot = g2,
      height = 9,
      width = 9
    )
  }

  return(Pdata)
}
