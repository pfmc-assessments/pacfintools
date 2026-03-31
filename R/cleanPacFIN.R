#' Clean raw PacFIN data
#'
#' Clean raw PacFIN data to remove unsuitable samples if `clean = TRUE` and
#' convert units of measured quantities to work with downstream functions.
#' Raw data are meant to be inclusive of everything from PacFIN so users can
#' explore all that is available, but this means that raw data will **ALWAYS**
#' include information that is not appropriate for use in
#' US West Coast stock assessments.
#'
#' @param Pdata A data frame returned from [PullBDS.PacFIN()] containing
#'   biological samples. These data are stored in the Pacific Fishieries
#'   Information Network (PacFIN) data warehouse, which originated in 2014 and
#'   are pulled using sql calls.
#' @param keep_INPFC *Deprecated*. Areas are now defined using different methods.
#' @param keep_gears A character vector including the gear types you want
#'   to label as unique fleets. Order matters and will define fleet numbering.
#'   If the argument is missing, which is the default, then all found gear
#'   groups are maintained and ordered alphabetically. For more details see
#'   [getGearGroup()], which lists a link where you can find the available gear
#'   groupings and how they link to `"PACFIN_GEAR_CODE"` within your data. The vector
#'   supplied to this argument should consist of only options available in
#'   `unique(GearTable[["GROUP"]])`.
#'  Typical entries will include character values
#'   such as `HKL`, `POT`, `TWL`, where the latter is short for all non-shrimp
#'   trawls and `TWS` is shrimp trawls. Other gear identification codes and
#'   their definitions include `DRG` which is dredge gear, `MSC` which is all
#'   other miscellaneous gear such as diving or river trawls, `NET` which is all
#'   non-trawl net gear, `NTW` which is non-trawl gear, and `TLS` which is
#'   trolling gear. As a special case, `MID` is available for spiny dogfish to
#'   extract mid-water trawl data as a separate fleet.
#' @param keep_sample_type A vector of character values specifying the types of
#'   samples you want to keep. The default is to keep `c("M")`. Available types
#'   include market (M), research (R), special request (S), and commercial
#'   on-board (C). There are additional samples without a `SAMPLE_TYPE`, but
#'   they are only kept if you include `NA` in your call. All sample types from
#'   California are assigned to `M`. Including commercial on-board samples is
#'   not recommended because they might also be in WCGOP data and would lead to
#'    double counting.
#' @param keep_sample_method A vector of character values specifying the types
#'   of sampling methods you want to keep. The default is to keep \code{"R"},
#'   which refers to samples that were sampled randomly. Available types include
#'   random (R), stratified (S), systematic (N), purposive (P), and special (X).
#'   As of February 17, 2021, Washington is the only state with a sample type of
#'   `""`, and it was limited to two special samples of yelloweye rockfish.
#' @param keep_length_type A vector of character values specifying the types of
#'   length samples to keep. There is no default value, though users will
#'   typically want to keep `c("", "F", "A")`, but should also think about using
#'   `c("", "F", "A", NA)`. Note that types other than those listed below can be
#'   present, especially if you are dealing with a skate.
#'   `A` is alternate length,
#'   `D` is dorsal length,
#'   `F` is fork length,
#'   `S` is standard length, and
#'   `T` is total length.
#' @param keep_age_method A vector of ageing methods to retain in the data. All
#'   fish aged with methods other than those listed will no longer be considered
#'   aged. A value of `NULL`, the default, will keep all ageing methods.
#'   However, a vector of `c("B", "BB", S", "", NA, 1, 2)` will keep all unaged
#'   fish and those that were aged with break and burn and surface reads. You do
#'   not really need to include such a verbose vector of values though because
#'   numbers are converted to appropriate character codes in [getAge()].
#'   Therefore, something like `c("B", "S")` would be sufficient to keep all
#'   break and burn and surface reads.
#' @param keep_missing_lengths *Deprecated*. Just subset them using
#'   `is.na(Pdata[, 'length']) after running `cleanPacFIN` if you want to remove
#'   lengths, though there is no need because the package accommodates keeping
#'   them in.
#' @param keep_states A vector of states that you want to keep, where each state
#'   is defined using a two-letter abbreviation, e.g., `WA`. The default is to
#'   keep data from all three states, `keep_states = c("WA", "OR", "CA")`. Add
#'   `'UNK'` to the vector if you want to keep data not assigned to a state.
#' @param clean A logical value used when you want to remove data from the input
#'   data set. The default is `TRUE`. Where the opposite returns the original
#'   data with additional columns and reports on what would have been removed.
#' @param CLEAN *Deprecated* A logical value used when you want to remove data from the input
#'   data set. The default is `TRUE`. Where the opposite returns the original
#'   data with additional columns and reports on what would have been removed.
#' @param spp A character string giving the species name to ensure that the
#'   methods are species specific. Leave \code{NULL} if generic methods work for
#'   your species. Currently, sablefish is the only species with
#'   species-specific code.
#' @param verbose A logical specifying if output should be written to the
#'   screen or not. Good for testing and exploring your data but can be turned
#'   off when output indicates information that you already know. The printing
#'   of output to the screen does not affect any of the returned objects. The
#'   default is to always print to the screen, i.e., `verbose = TRUE`.
#' @param savedir Deprecated. This function no longer creates plots of cleaned data,
#'   but you can call [plotCleaned()] separately if you want to create these plots.
#'
#' @export
#' @return The input data filtered for desired areas and record types
#' specified, with added columns
#'
#' * year: initialized from SAMPLE_YEAR\cr
#' * fleet: initialized to 1
#' * fishery: initialized to 1
#' * season: initialized to 1.  Change using [getSeason]
#' * state: initialized from AGENCY_CODE.  Change using [getState]
#' * length: length in mm, where `NA` indicates length is not available
#' * lengthcm: floored cm from FORK_LENGTH when available, otherwise FISH_LENGTH
#' * geargroup: the gear group associated with each [PACFIN_GEAR_CODE](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' * weightkg: fish weight in kg from FISH_WEIGHT and FISH_WEIGHT_UNITS
#'
#' @details
#' The original fields in the returned data are left untouched,
#' with the exception of
#' * `SEX_CODE`: modified using [nwfscSurvey::codify_sex()] and upon return will
#' only include character values such that fish with an unidentified sex are
#' now `"U"`.
#' * Age: the best ages to use going forward rather than just the first age read.
#'
#' \subsection{clean}{
#' The data are put through various tests before they are returned
#' and the results of these tests are stored in the \code{clean} column.
#' Thus, sometimes it is informative to run \code{cleanPacFIN(clean = FALSE)}
#' and use frequency tables to inspect which groups of data will be removed
#' from the data set when you change the code to be \code{clean = FALSE}.
#' For example, many early length compositions do not have information on
#' the weight of fish that were sampled, and thus, there is no way to infer
#' how much the entire sample weighed or how much the tow/trip weighed.
#' Therefore, these data cannot be expanded and are removed using
#' \code{clean = TRUE}. Some stock assessment authors or even previous
#' versions of this very code attempted to use adjacent years to inform
#' weights. The number of assumptions for this was great and state
#' representatives discouraged inferring data that did not exist.
#' }
#'
#' \subsection{Furthermore}{
#' The values created as new columns are for use by other functions in this package.
#' In particular, `fishyr` and `season` are useful if there are multiple
#' seasons (e.g., winter and summer, as in the petrale sole assessment), and the
#' year is adjusted so that "winter" occurs in one year, rather than across two.
#'
#' The `fleet`, `fishery`, and `state` columns are meant for use in
#' stratifying the data according to the particulars of an assessment.
#' }
#'
#' @seealso [getState], [getSeason]
#'
#' @author Andi Stephens, Kelli F. Johnson, Chantel R. Wetzel

cleanPacFIN <- function(
  Pdata,
  keep_gears = NULL,
  keep_length_type = NULL,
  keep_sample_type = c("M"),
  keep_sample_method = "R",
  keep_age_method = NULL,
  keep_states = c("WA", "OR", "CA"),
  clean = TRUE,
  CLEAN = lifecycle::deprecated(),
  keep_INPFC = lifecycle::deprecated(),
  keep_missing_lengths = lifecycle::deprecated(),
  spp = NULL,
  verbose = TRUE,
  savedir = lifecycle::deprecated()
) {
  #### Deprecate old input arguments
  if (lifecycle::is_present(keep_INPFC)) {
    lifecycle::deprecate_stop(
      when = "0.0.1.0005",
      what = paste0("cleanPacFIN(keep_INPFC = )"),
      details = paste0(
        "It is thought that PSMFC areas can decipher much of what was\n",
        "previously determined with INPFC areas."
      )
    )
  }
  if (lifecycle::is_present(keep_missing_lengths)) {
    lifecycle::deprecate_stop(
      when = "0.0.1.0005",
      what = paste0("cleanPacFIN(keep_missing_lengths = )"),
      details = paste0(
        "All down-stream functionality works without filtering,\n",
        "but Pdata[is.na(Pdata[['length']]), ] can be used to filter them out."
      )
    )
  }
  if (lifecycle::is_present(CLEAN)) {
    lifecycle::deprecate_warn(
      when = "0.4.1",
      what = paste0("cleanPacFIN(CLEAN = )"),
      details = "Please use cleanPacFIN(clean =)."
    )
    clean = CLEAN
  }
  if (lifecycle::is_present(savedir)) {
    lifecycle::deprecate_warn(
      when = "0.4.1",
      what = paste0("cleanPacFIN(savedir = )"),
      details = paste0(
        "This function no calls plotCleaned() which used savedir to create and save plots. Please call this function separately if plots of cleaned data are needed."
      )
    )
  }

  #### CLEAN COLUMNS
  if (check_columns_downloaded(Pdata)) {
    data_columns <- cleanColumns(Pdata)
  } else {
    data_columns <- Pdata
  }
  check_calcom <- any(data_columns[["AGENCY_CODE"]] == "CalCOM")

  # If the user has not specified which gears to keep, set keep_gears to
  # all present gear groups
  if (is.null(keep_gears)) {
    keep_gears <- unique(GearTable$GROUP)
  }
  #### Fill in missing input arguments
  data <- getGearGroup(
    Pdata = data_columns,
    keep_gears = keep_gears,
    spp = spp,
    verbose = verbose
  )
  data[, "fleet"] <- data[, "geargroup"]

  #### Column names
  if (!"fishery" %in% colnames(data)) {
    data[, "fishery"] <- 1
  }
  data$fishyr <- data$SAMPLE_YEAR
  data$year <- data$SAMPLE_YEAR
  # removing the getSeason call since there are no arguements passed to
  # cleanPacFIN that are used in this function.
  data$season <- 1

  #### Areas
  data <- getState(
    Pdata = data,
    source = ifelse(
      "AGENCY_CODE" %in% colnames(data),
      "AGENCY_CODE",
      "AGID"
    ),
    verbose = verbose
  )
  # California doesn't record SAMPLE_TYPE so we assume they are all Market samples
  data[data$state == "CA" & is.na(data$SAMPLE_TYPE), "SAMPLE_TYPE"] <- "M"
  # Perhaps add check here to change SAMPLE_TYPE == S samples from Oregon before 1987 to M

  #### Sex
  data[, "SEX_CODE"] <- nwfscSurvey::codify_sex(
    data[, "SEX_CODE"]
  )

  #### Lengths
  if (is.null(keep_length_type)) {
    keep_length_type <- sort(unique(c(
      data[, "FISH_LENGTH_TYPE_CODE"],
      "",
      "A", # alternative length
      "D", # dorsal length
      "F", # fork length
      "R",
      "S",
      "T",
      "U", #unknown
      NA
    )))
  }
  data[, "length"] <- getLength(
    Pdata = data,
    keep = keep_length_type,
    verbose = verbose
  )
  data[, "lengthmm"] <- data[, "length"]
  data[, "lengthcm"] <- floor(data[, "lengthmm"] / 10)

  # Deal with ages
  if (is.null(keep_age_method)) {
    keep_age_method <- unique(
      unlist(data[, grep("AGE_METHOD[0-9]*$", colnames(data))])
    )
  }
  data[, "Age"] <- getAge(
    Pdata = data,
    verbose = verbose,
    keep = keep_age_method
  )
  # TODO: speed up this function
  data[, "age_method"] <- getAgeMethod(
    Pdata = data,
    verbose = verbose
  )

  #### Weight (random units in)
  data[, "weightkg"] <- getweight(
    length = lengthmm,
    weight = data[["FISH_WEIGHT"]],
    unit.in = data[["FISH_WEIGHT_UNITS"]],
    unit.out = "kg"
  )

  # Check record area:
  data[, "area"] <- getArea(Pdata = data, verbose = verbose)

  #### Bad samples
  # Remove bad OR samples
  # data$SAMPLE_TYPE[data$SAMPLE_NUMBER %in% paste0("OR", badORnums)] <- "S"
  # Via Chantel, from Ali at ODFW, do not keep b/c they don't have EXPANDED_SAMPLE_WEIGHT or FTID
  # if ("SAMPLE_QUALITY" %in% colnames(Pdata)) {
  #  Pdata[Pdata[["SAMPLE_QUALITY"]] == 63, "SAMPLE_TYPE"] <- "S"
  # }

  #### Summary and return
  # Identify good records: keep TRUEs
  bad <- data[, 1:2]
  bad[, "badarea"] <- !is.na(data$area)
  bad[, "badstype"] <- !data$SAMPLE_TYPE %in% keep_sample_type
  bad[, "badsmeth"] <- !data$SAMPLE_METHOD_CODE %in% keep_sample_method
  bad[, "badsno"] <- is.na(data$SAMPLE_NUMBER)
  bad[, "badstate"] <- !data[, "state"] %in% keep_states
  bad[, "badgear"] <- !data[, "geargroup"] %in% keep_gears
  bad[, "bador_type_weight"] <- (is.na(data[[
    "EXPANDED_SAMPLE_WEIGHT"
  ]]) &
    data[["SAMPLE_TYPE"]] %in% keep_sample_type &
    data[["state"]] == "OR")
  bad[, "remove"] <- ifelse(
    apply(bad[, grep("^bad", colnames(bad))], 1, sum) > 0,
    yes = TRUE,
    no = FALSE
  )
  bad[, "n_removed_reasons"] <- apply(
    bad[, grep("^bad", colnames(bad))],
    1,
    sum
  )

  # Report removals
  if (verbose) {
    narea <- sum(bad[, "badarea"])
    ntype <- sum(bad[, "badstype"])
    nmethod <- sum(bad[, "badsmeth"])
    nnumber <- sum(bad[, "badsno"])
    nstate <- sum(bad[, "badstate"])
    ngear <- sum(bad[, "badgear"])
    nweight_or <- sum(bad[, "bador_type_weight"])
    nlength <- sum(is.na(data$lengthmm))
    nage <- sum(is.na(data$Age))
    nlenage <- sum(is.na(data$lengthmm) & is.na(data$Age))
    nclean <- NROW(data) - sum(bad[, "remove"])
    nremoved <- sum(bad[, "remove"])

    cli::cli_bullets(c(
      " " = "Summary of data processing and cleaning checks:",
      "i" = "The number of records potentially removed if clean = TRUE are not mutually exclusive.",
      "x" = "Number of records not in federal waters: {narea}",
      "x" = "Number of records not in keep_sample_type (SAMPLE_TYPE): {ntype}",
      "x" = "Number of records not in keep_sample_method (SAMPLE_METHOD_CODE): {nmethod}",
      "x" = "Number of records without SAMPLE_NUMBER: {nnumber}",
      "x" = "Number of records not in keep_states: {nstate}",
      "x" = "Number of records not in keep_gears: {ngear}",
      "x" = "Number of records in Oregon within the requested sample type without a EXPANDED_SAMPLE_WEIGHT: {nweight_or}",
      "i" = "Number of records with bad length type of NA: {nlength}. These lengths should be investigated further to determine if they can be used or not.",
      "i" = "Number of records without length and Age: {nlenage}",
      "i" = "Number of records: {NROW(Pdata)}",
      "i" = "Number of records remaining if clean = TRUE: {nclean}",
      "i" = "Number of records removed if clean = TRUE: {nremoved}"
    ))

    if (check_pacfin_species_code_calcom(Pdata$PACFIN_SPECIES_CODE)) {
      if (!check_calcom) {
        cli::cli_bullets(
          "x" = "Additional biological data are available from CALCOM for flatfish species pre-1990, please contact E.J. (edward.dick@noaa.gov) and Brenda (BErwin@psmfc.org)."
        )
      }
    }
  }

  clean_vector <- ifelse(
    bad[, "remove"] == TRUE,
    yes = FALSE,
    no = TRUE
  )
  data[, "clean"] <- clean_vector
  if (clean) {
    data <- data[clean_vector, ]
  }

  return(data)
}
