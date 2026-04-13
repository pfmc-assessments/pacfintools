# Read in the csv file with port lat and long:
pacfin_ports_withlatlong_raw <- tibble::as_tibble(read.csv(
  file.path("data-raw", "PacFIN_Ports_withlatlong.csv")
))

colnames(pacfin_ports_withlatlong_raw) <- tolower(colnames(
  pacfin_ports_withlatlong_raw
))

pacfin_ports_withlatlong <- pacfin_ports_withlatlong_raw |>
  labelled::set_variable_labels(
    name = "U.S. West Coast port name.",
    pcid = "Port code used in PacFIN",
    agencydesc = "State agency port name.",
    agid = "State agency identifier where W = Washington, O = Oregon, and C = California.",
    longitude = "Longitude of the port location with minutes and seconds in radians.",
    latitude = "Latitude of the port location with minutes and seconds in radians."
  )
usethis::use_data(pacfin_ports_withlatlong, overwrite = TRUE)

agency_ports_withlatlong_raw <- tibble::as_tibble(read.csv(
  file.path("data-raw", "Ports_Crosswalk_2024-07-17.csv")
))
colnames(agency_ports_withlatlong_raw) <- tolower(colnames(
  agency_port_withlatlong_raw
))

agency_ports_withlatlong <- agency_ports_withlatlong_raw |>
  labelled::set_variable_labels(
    agid_port = "Agency port abbreviation.",
    agency_portdescription = "Agency port description",
    longitude = "Longitude of the port location with minutes and seconds in radians.",
    latitude = "Latitude of the port location with minutes and seconds in radians.",
    area = "North or south of 40.10 N. latitude",
    ifq_area = "IFQ area code used by the West Coast Groundfish Observer Program."
  )
usethis::use_data(agency_ports_withlatlong, overwrite = TRUE)
