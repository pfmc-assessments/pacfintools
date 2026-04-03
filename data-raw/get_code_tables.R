GearTable <- get_codelist("GEAR")
PortTable <- get_codelist("PORT")
INPFCTable <- get_codelist("INPFC")

usethis::use_data(
  GearTable,
  overwrite = TRUE
)
usethis::use_data(
  PortTable,
  overwrite = TRUE
)
usethis::use_data(
  INPFCTable,
  overwrite = TRUE
)
