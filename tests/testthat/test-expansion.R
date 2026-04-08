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
  expect_equal(nrow(bds_cleaned), 40102)
  expect_equal(length(unique(bds_cleaned$stratification)), 9)
  expect_equal(sum(bds_cleaned$geargroup == "TWL"), 14223)
  expect_equal(sum(bds_cleaned$geargroup == "HKL"), 19698)
  expect_equal(sum(bds_cleaned$geargroup == "POT"), 6181)

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
  expect_equal(
    sum(expanded_comps[, "Final_Sample_Size_L"]),
    12552620,
    tolerance = 1e-2
  )
  expect_equal(
    sum(expanded_comps[, "Final_Sample_Size_A"]),
    1898862,
    tolerance = 1e-2
  )

  length_comps_long <- getComps(
    Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm)),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L",
    verbose = FALSE
  )
  expect_equal(nrow(length_comps_long), 1533)
  expect_equal(ncol(length_comps_long), 9)
  expect_equal(sum(length_comps_long[, "comp"]), 12552620, tolerance = 1e-2)

  length_composition_data <- writeComps(
    inComps = length_comps_long,
    column_with_input_n = "n_stewart",
    comp_bins = seq(18, 90, by = 2),
    verbose = FALSE
  )
  expect_equal(nrow(length_composition_data), 24)
  expect_equal(ncol(length_composition_data), 80)
  expect_equal(sum(length_composition_data$input_n), 7485.9, tolerance = 1e-3)
  expect_equal(
    sum(length_composition_data[, 7:ncol(length_composition_data)]),
    12552620,
    tolerance = 1e-2
  )

  age_comps_long <- getComps(
    Pdata = dplyr::filter(expanded_comps, !is.na(Age)),
    Comps = "AGE",
    weightid = "Final_Sample_Size_A",
    verbose = FALSE
  )
  expect_equal(nrow(age_comps_long), 701)
  expect_equal(ncol(age_comps_long), 9)
  expect_equal(sum(age_comps_long[, "comp"]), 1445160, tolerance = 1e-2)

  age_composition_data <- writeComps(
    inComps = age_comps_long,
    comp_bins = 0:50,
    column_with_input_n = "n_tows",
    verbose = FALSE
  )
  expect_equal(nrow(age_composition_data), 17)
  expect_equal(ncol(age_composition_data), 111)
  expect_equal(sum(age_composition_data$input_n), 371, tolerance = 1e-3)
  expect_equal(sum(age_composition_data[, 10:111]), 1445082, tolerance = 1e-3)
})
