#' Assign latitudes to records
#'
#' @inheritParams cleanPacFIN
#'
#' @export
#' @author Chantel Wetzel
#' @return The data frame is returned with an additional column for latitude
#'
add_latitude <- function(
  Pdata,
  verbose = TRUE
) {
  port_lats <- PEPtools::pacfin_ports_withlatlong |>
    dplyr::rename(
      PACFIN_PORT_CODE = pcid,
      AGENCY_CODE = agid,
      port_latitude = latitude
    ) |>
    dplyr::distinct(PACFIN_PORT_CODE, .keep_all = TRUE) |>
    dplyr::select(PACFIN_PORT_CODE, port_latitude) |>
    tibble::tibble()

  Pdata_with_latitude <- dplyr::left_join(
    Pdata,
    port_lats,
    by = "PACFIN_PORT_CODE"
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
