#' Create column for gear called `geargroup` according to PacFIN gears
#'
#'
#' @details
#' Data from the PacFIN [gear
#' table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' is used to create a column in `Pdata` called `geargroup`, where
#' `Pdata[["PACFIN_GEAR_CODE"]]` is recoded to `geargroup` according to the gear group listed
#' in the table.
#' For some species the recoding is more intense. For example, sablefish
#' `GROUPS` are recoded into just three gears, hook and line (HKL), pot (POT),
#' and trawl (TWL). This is done internally to maintain consistency across
#' assessment authors. Other species can easily be added to this function. If a
#' species name is not passed then no additional recoding is performed compared
#' to the matching available in the table pulled from online.
#'
#' @param Pdata A data frame, typically one extracted from PacFIN but for this
#'   function just one column is needed, `PACFIN_GEAR_CODE`.
#' @inheritParams cleanPacFIN
#'
#' @export
#' @seealso
#' * [cleanPacFIN()] uses this function
#'
#' @return
#' A modified data frame where an additional column labeled \code{geargroup} is
#' added to \code{Pdata}. No original columns are modified in the process.
#' @author Andi Stephens, Kelli F. Johnson, Chantel R. Wetzel
#' @examples
#' gears <- c("PRT", "FPT", "TWL", "MDP")
#' X <- getGearGroup(data.frame(PACFIN_GEAR_CODE = gears), verbose = TRUE)
#'
getGearGroup <- function(Pdata, keep_gears = NULL, spp = NULL, verbose = TRUE) {
  if (!"PACFIN_GEAR_CODE" %in% colnames(Pdata)) {
    cli::cli_abort("Pdata must have 'PACFIN_GEAR_CODE' column.")
  }
  if (is.factor(Pdata[, "PACFIN_GEAR_CODE"])) {
    Pdata[, "PACFIN_GEAR_CODE"] <- as.character(Pdata[, "PACFIN_GEAR_CODE"])
  }
  # gear_table <- pacfintools::GearTable
  gear_table <- GearTable

  # Species-specific code to alter the PacFIN gear table
  if (!is.null(spp)) {
    if (tolower(spp) %in% c("sablefish", "sabl")) {
      change_to_trawl <- c(
        # Midwater trawl, diving (b/c there is so little), other trawls, dredge
        "DGN",
        "DPN",
        "DVG",
        "DRG",
        "GLN",
        "MDT",
        "MPT",
        "MSC",
        "ODG",
        "ONT",
        "OTH",
        "PWT",
        "RVT",
        "SCD",
        "SEN",
        "STN",
        "TWS",
        "USP",
        # all nets
        "BTR",
        "DST",
        "NET",
        "SGN",
        "SHT",
        "SST",
        "TML",
        "TRL"
      )
      change_to_hkl <- c("HTR", "NTW", "PTR", "TLS")
      if (verbose) {
        cli::cli_bullets(
          c(
            "i" = "sablefish uses HKL, POT, and TWL",
            "i" = "{change_to_hkl} are recoded to HKL.",
            "i" = "everything not assigned to HKL or POT is recoded to TWL."
          )
        )
      }
      gear_table[, "GROUP"][
        gear_table$PACFIN_GEAR_CODE %in% change_to_trawl
      ] <- "TWL"
      # Trolling gear and non-trawl
      gear_table[, "GROUP"][
        gear_table$PACFIN_GEAR_CODE %in% change_to_hkl
      ] <- "HKL"
    } # end if spp == sablefish
    if (any(grepl("dogfish|dsrk", spp, ignore.case = TRUE))) {
      if (verbose) {
        cli::cli_alert_info(
          "Dogfish uses a mid-water trawl (MID), TWL (including shrimp), and HKL fleets. Everything else is assigned to MSC."
        )
      }
      gear_table[
        grepl("MIDWATER", gear_table[["DESCRIPTION"]]),
        "GROUP"
      ] <- "MID"
      gear_table[grepl("TWS", gear_table[["GROUP"]]), "GROUP"] <- "TWL"
      # Assign everything else to MSC
      gear_table[!gear_table[["GROUP"]] %in% c("MID", "TWL"), "GROUP"] <- "MSC"
    } # end if spp == dogfish
  }

  #### Create geargroup
  Pdata[, "geargroup"] <- gear_table[
    match(Pdata[, "PACFIN_GEAR_CODE"], gear_table[, "GRID"]),
    "GROUP"
  ]
  Pdata[, "geargroup"] <- ifelse(
    test = is.na(Pdata[, "geargroup"]),
    yes = Pdata[, "PACFIN_GEAR_CODE"],
    no = Pdata[, "geargroup"]
  )
  if (verbose) {
    table_gears <- table(Pdata[, "geargroup"])
    message_table <- paste0(
      names(table_gears),
      " (",
      table_gears,
      ")"
    )
    other_gears <- table(Pdata[
      which(!Pdata$geargroup %in% keep_gears),
      "geargroup"
    ])
    if (length(other_gears) > 0) {
      other_gear_message_table <- paste0(
        names(other_gears),
        " (",
        other_gears,
        ")"
      )
    } else {
      other_gear_message_table <- "none"
    }
    cli::cli_bullets(c(
      " " = "{.fn getGearGroup} summary information -",
      "i" = "geargroup includes: {message_table}",
      "i" = "The following gears were not included in keep_gears and will be removed if clean = TRUE: {other_gear_message_table}."
    ))
  }

  return(Pdata)
}
