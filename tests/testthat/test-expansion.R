test_that("Expand biological data", {
  skip_on_ci()
  password_file_name <- "password.txt"
  skip_if_not(
    fs::file_exists(password_file_name),
    message = "Your password is not saved."
  )
  species <- "SABL"
  password <- readLines(password_file_name, warn = FALSE)
  bds.pacfin <- PullBDS.PacFIN(
    pacfin_species_code = species,
    password = password,
    verbose = FALSE
  )

  bds_cleaned <- cleanPacFIN(
    Pdata = bds.pacfin |> dplyr::filter(SAMPLE_YEAR %in% c(2007:2010)),
    keep_gears = c("HKL", "POT", "TWL"),
    verbose = FALSE
  ) |>
    dplyr::mutate(
      stratification = paste(state, geargroup, sep = ".")
    )
  expect_equal(nrow(bds_cleaned), 40125)
  expect_equal(length(unique(bds_cleaned$stratification)), 9)

  weight_length_estimates <- getWLpars(
    data = bds_cleaned,
    verbose = FALSE
  ) |>
    dplyr::rename(sex = group)

  expect_equal(nrow(weight_length_estimates), 3)
  expect_equal(ncol(weight_length_estimates), 5)
  expect_equal(weight_length_estimates[3, "SD"], 0.2036146, tolerance = 1e-3)

  catch_formatted <- utils::read.csv(here::here(
    "data-raw",
    "catch_for_expansions.csv"
  )) |>
    formatCatch(
      strat = c("state", "geargroup"),
      valuename = "catch_mt"
    )
  expect_equal(nrow(catch_formatted), 4)
  expect_equal(ncol(catch_formatted), 10)

  expanded_comps <- get_pacfin_expansions(
    Pdata = bds_cleaned,
    Catch = catch_formatted,
    weight_length_estimates = weight_length_estimates,
    Units = "MT",
    maxExp = 0.95,
    verbose = FALSE
  )
  expect_equal(sum(expanded_comps[, "Final_Sample_Size_L"]), 9219564)
  expect_equal(
    round(sum(expanded_comps[, "Final_Sample_Size_A"]), 0),
    1899150
  )
})
