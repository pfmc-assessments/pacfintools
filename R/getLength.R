#' Get length for PacFIN commercial samples
#'
#' Get length for PacFIN commercial samples in millimeters.
#' Lengths are converted for skates from type R and A to actual lengths.
#' Other, more standard lengths, are filtered for types in `keep`.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams cleanPacFIN
#' @param keep A vector of values that represent what you want to keep. Values
#'   of `NA`, `""`, and numeric values are acceptable. Often, it is helpful to
#'   run `unique(, useNA = "always") on the relevant data prior to running the
#'   function to see what types are present.
#'
#' @export
#' @return A vector of lengths in millimeters. Values of `NA` indicate that
#' the length should not be used in down-stream calculations.
#'
getLength <- function(Pdata, keep, verbose = TRUE) {
  # Initial checks
  # Early return
  if (all(is.na(Pdata[["FISH_LENGTH"]]))) {
    if (verbose) {
      cli::cli_alert_success("No lengths were found, moving on.")
    }
    return(rep(NA, NROW(Pdata)))
  }
  # Stop
  data_with_length <- dplyr::filter(
    .data = Pdata,
    !is.na(FISH_LENGTH),
    !(FISH_LENGTH_UNITS %in% c("MM", "CM"))
  )
  if (NROW(data_with_length)) {
    cli::cli_abort(
      "FISH_LENGTH_UNITS contains units other than 'CM' or 'MM' for fish
        with lengths, please assign a unit like 'CM' or 'MM' to each row."
    )
  }

  # Find columns
  var_spid <- grep(
    pattern = "^SPID$|PACFIN_SPECIES_CODE",
    x = colnames(Pdata),
    value = TRUE
  )[1]
  var_state <- grep("SOURCE_AGID|AGENCY_CODE", colnames(Pdata), value = TRUE)[1]
  var_fish_length_type <- grep(
    pattern = "^FISH_LENGTH_TYPE$|^FISH_LENGTH_TYPE_CODE",
    x = colnames(Pdata),
    value = TRUE
  )

  # Can only accommodate good types
  good_types <- c("", "A", "D", "F", "R", "S", "T", "U", NA)
  good_types_string <- glue::glue_collapse(sQuote(good_types), sep = ", ")
  if (any(!Pdata[[var_fish_length_type]] %in% good_types)) {
    cli::cli_abort(
      "getLength() can only accommodate the following FISH_LENGTH_TYPE_CODEs: {good_types_string}.
       Please contact the package maintainer to add additional types."
    )
  }

  # Check for "F" FISH_LENGTH_TYPE_CODE from California for spiny dogfish, a hack that
  # will eventually be removed (todo).
  check.calt <- which(
    Pdata[[var_spid]] == "DSRK" &
      Pdata[[var_state]] == "C" &
      Pdata[[var_fish_length_type]] == "F"
  )
  if (length(check.calt) > 0) {
    change_vals <- length(check.calt)
    cli::cli_inform(
      "Changing {change_vals} CA FISH_LENGTH_TYPE == F to T.
      Vlada is working on getting these entries fixed in PacFIN."
    )
    Pdata[check.calt, var_fish_length_type] <- "T"
  }

  # Move FISH_LENGTH to FORK_LENGTH if FORK_LENGTH is NA and type is F
  # for downstream code to work
  Pdata[, "FORK_LENGTH"] <- ifelse(
    is.na(Pdata[["FORK_LENGTH"]]) & Pdata[[var_fish_length_type]] == "F",
    yes = Pdata[, "FISH_LENGTH"],
    no = Pdata[, "FORK_LENGTH"]
  )

  # Spiny dogfish (Squalus suckleyi; DSRK)
  if (length(grep("DSRK", Pdata[[var_spid]])) > 0) {
    check.dogfish <- !is.na(Pdata[["FORK_LENGTH"]])
    if (sum(check.dogfish) > 0 & verbose) {
      total_check_dogfish <- sum(check.dogfish)
      cli::cli_inform(
        "{total_check_dogfish} fork lengths were converted to total lengths using Tribuzio and Kruse (2012)."
      )
    }
    Pdata[check.dogfish, "FORK_LENGTH"] <-
      ifelse(Pdata[check.dogfish, "FISH_LENGTH_UNITS"] == "MM", 12.2, 1.22) +
      1.07 * Pdata[check.dogfish, "FORK_LENGTH"]
  }

  # Fix incorrect FISH_LENGTH_UNITS for hake
  if (length(grep("PWHT", Pdata[[var_spid]])) > 0) {
    if (verbose) {
      cli::cli_inform("Still fixing WA FISH_LENGTH_UNITS for Pacific hake.")
    }
    Pdata[, "FISH_LENGTH_UNITS"] <- ifelse(
      tolower(Pdata[, "FISH_LENGTH_UNITS"]) == "cm" &
        Pdata[, "FISH_LENGTH"] > 90,
      "MM",
      Pdata[, "FISH_LENGTH_UNITS"]
    )
  }

  # Make "length" column in mm
  # Start with fork lengths for those that are available and if "F" in keep
  keep_list <- keep[keep %in% c("", "A", "F", NA)]
  Pdata$length <- ifelse(
    Pdata[[var_fish_length_type]] %in% keep_list,
    yes = Pdata$FORK_LENGTH,
    no = NA
  )

  Pdata$length <- ifelse(
    "D" %in%
      keep &
      Pdata[[var_fish_length_type]] == "D" &
      Pdata$FORK_LENGTH != Pdata$FISH_LENGTH,
    yes = Pdata$FORK_LENGTH,
    no = Pdata$length
  )

  # Work with standard length measurements and unknown type
  Pdata$length <- ifelse(
    "S" %in% keep & Pdata[[var_fish_length_type]] == "S",
    yes = Pdata$FISH_LENGTH,
    no = Pdata$length
  )
  Pdata$length <- ifelse(
    "T" %in% keep & Pdata[[var_fish_length_type]] == "T",
    yes = Pdata$FISH_LENGTH,
    no = Pdata$length
  )
  Pdata$length <- ifelse(
    "U" %in% keep & Pdata[[var_fish_length_type]] == "U",
    yes = ifelse(
      is.na(Pdata$FORK_LENGTH),
      yes = Pdata$FISH_LENGTH,
      no = Pdata$FORK_LENGTH
    ),
    no = Pdata$length
  )
  Pdata$length <- ifelse(
    "" %in% keep & Pdata[[var_fish_length_type]] == "",
    yes = Pdata$FISH_LENGTH,
    no = Pdata$length
  )
  Pdata$length <- ifelse(
    NA %in% keep & is.na(Pdata[[var_fish_length_type]]),
    yes = ifelse(
      is.na(Pdata$FORK_LENGTH),
      yes = Pdata$FISH_LENGTH,
      no = Pdata$FORK_LENGTH
    ),
    no = Pdata$length
  )

  # Work with skate data
  # A is disc width
  # R is inter-spiracle width for skates (used by WDFW)
  if (all(Pdata[[var_spid]] %in% c("LSKT", "BSKT"))) {
    # Species-specific code
    # Convert FISH_LENGTH from disk width to length
    width2length <- convertlength_skate(Pdata, returntype = "estimated")

    Pdata$length <- ifelse(
      "A" %in% keep & Pdata[[var_fish_length_type]] == "A",
      yes = width2length,
      no = Pdata$length
    )

    Pdata$length <- ifelse(
      "R" %in% keep & Pdata[[var_fish_length_type]] == "R",
      yes = width2length,
      no = Pdata$length
    )
  }

  # A double check that lengths for methods not in keep are NA
  Pdata$length <- ifelse(
    Pdata[[var_fish_length_type]] %in% keep,
    yes = ifelse(
      is.na(Pdata$FORK_LENGTH),
      yes = Pdata$FISH_LENGTH,
      no = Pdata$FORK_LENGTH
    ),
    no = NA
  )

  # Assign all fish of length zero to NA
  i_length_0 <- Pdata[["length"]] == 0
  Pdata$length[i_length_0] <- NA

  # Ensure everything is in mm
  # As of 2023-02-28 there are only two valid units in PacFIN for length
  # MM and CM, everything else is NULL or UNK
  Pdata$length <- ifelse(
    test = Pdata[, "FISH_LENGTH_UNITS"] == "CM",
    yes = Pdata[, "length"] * 10,
    no = Pdata[, "length"]
  )

  if (verbose) {
    available_length_types <- table(
      Pdata[[var_fish_length_type]],
      useNA = "always"
    )
    message_available_length_types <- paste0(
      names(available_length_types),
      " (",
      available_length_types,
      ")"
    )
    types <- unique(Pdata[
      !is.na(Pdata[["length"]]),
      var_fish_length_type
    ])
    n0 <- sum(Pdata[["length"]] == 0, na.rm = TRUE)
    n <- sum(Pdata[['FISH_LENGTH_UNITS']] == 'CM', na.rm = TRUE)
    n_not_include <- NROW(Pdata) -
      sum(Pdata[["FISH_LENGTH_TYPE_CODE"]] %in% keep)
    cli::cli_bullets(c(
      " " = "{.fn getLength} summary information -",
      "i" = "The following length types were available in the data: {message_available_length_types}.",
      "i" = "The following length types were kept in the data: {types}.",
      "i" = "Lengths range from {min(Pdata[['length']], na.rm = TRUE)}--{max(Pdata[['length']], na.rm = TRUE)} (mm)."
    ))
    if (n_not_include / NROW(Pdata) > 0.05) {
      cli::cli_alert_warning(
        "{n_not_include} lengths were not included because their FISH_LENGTH_TYPE_CODE was not in keep_length_type with lengthcm set to NA. 
        This is more than 5% of the data, investigate if you are missing important length data."
      )
    }

    if ("D" %in% types) {
      cli::cli_alert_danger(
        "The data includes dorsal lengths and should be investigated to ensure that these lengths should be used."
      )
    }
    if ("T" %in% types) {
      cli::cli_alert_danger(
        "The data includes total lengths which may or may not make sense to keep depending upon your species.."
      )
    }
  }
  return(Pdata[["length"]])
}
