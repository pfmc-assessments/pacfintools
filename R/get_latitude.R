#' Assign latitudes to records
#'
#' @inheritParams cleanPacFIN
#'
#' @export
#' @author Chantel Wetzel
#' @return The data frame is returned with an additional column for latitude
#'
get_latitude <- function(
  Pdata,
  verbose = TRUE
) {
  pacfin_port_lats <- pacfintools::pacfin_ports_withlatlong |>
    dplyr::rename(
      PACFIN_PORT_CODE = pcid,
      pacfin_port_latitude = latitude
    ) |>
    dplyr::distinct(PACFIN_PORT_CODE, .keep_all = TRUE) |>
    dplyr::select(PACFIN_PORT_CODE, pacfin_port_latitude) |>
    tibble::tibble()

  agency_port_lats <- pacfintools::agency_ports_withlatlong |>
    dplyr::rename(
      STATE_AND_AGENCY_PORT_CODE = agid_port,
      agency_port_latitude = latitude
    ) |>
    dplyr::select(STATE_AND_AGENCY_PORT_CODE, agency_port_latitude) |>
    tibble::tibble()

  add_pacfin_lat <- dplyr::left_join(
    Pdata,
    pacfin_port_lats,
    by = "PACFIN_PORT_CODE"
  )

  add_agency_lat <- dplyr::left_join(
    add_pacfin_lat |>
      dplyr::mutate(
        STATE_AND_AGENCY_PORT_CODE = paste0(AGENCY_CODE, AGENCY_PORT_CODE)
      ),
    agency_port_lats,
    by = "STATE_AND_AGENCY_PORT_CODE"
  )

  Pdata_with_latitude <- add_agency_lat |>
    dplyr::mutate(
      port_latitude = dplyr::case_when(
        !is.na(agency_port_latitude) ~ agency_port_latitude,
        !is.na(pacfin_port_latitude) ~ pacfin_port_latitude,
        .default = NA
      )
    ) |>
    dplyr::select(
      -pacfin_port_latitude,
      -agency_port_latitude
    )

  if (verbose) {
    missing_lats <- table(
      Pdata_with_latitude[["PACFIN_PORT_CODE"]][is.na(Pdata_with_latitude[[
        "port_latitude"
      ]])],
      useNA = "always"
    )
    message_missing_lats <- paste0(
      names(missing_lats),
      " (",
      missing_lats,
      ")"
    )
    if (sum(missing_lats) > 0) {
      cli::cli_bullets(c(
        " " = "{.fn add_latitude} summary information -",
        "!" = "A port_latitude was not available for the following PACFIN_PORT_CODE entries in the data: {message_missing_lats}."
      ))
    } else {
      cli::cli_bullets(c(
        " " = "{.fn add_latitude} summary information -",
        "!" = "A port_latitude was available for all PACFIN_PORT_CODE entries in the data."
      ))
    }
  }

  return(Pdata_with_latitude)
}
