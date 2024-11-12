remove_inactive_companies <- function(config) {
  config <- load_config(config)

  # input path for remove_inactive_companies
  path_abcd <- get_path_abcd(config)
  sheet_abcd <- get_sheet_abcd(config)

  # output path for remove_inactive_companies
  dir_prepared_abcd <- get_dir_prepared_abcd(config)

  remove_inactive_companies <- get_remove_inactive_companies(config)
  start_year <- get_start_year(config)
  time_frame <- get_time_frame(config)

  # validate config values----
  assert_length(path_abcd, 1L)
  assert_inherits(path_abcd, "character")
  assert_file_exists(path_abcd, desc = "ABCD")

  assert_length(sheet_abcd, 1L)
  assert_inherits(sheet_abcd, "character")
  assert_sheet_exists(sheet_abcd, path_abcd)

  if (!is.null(remove_inactive_companies)) {
    assert_length(remove_inactive_companies, 1L)
    assert_inherits(remove_inactive_companies, "logical")
  }

  assert_length(start_year, 1L)
  assert_inherits(start_year, "integer")

  assert_length(time_frame, 1L)
  assert_inherits(time_frame, "integer")


  # load data----
  abcd <- read_abcd_raw(path_abcd, sheet_abcd)
  assert_expected_columns(abcd, cols_abcd, desc = "ABCD")

  # optional: remove inactive companies----

  # (1) remove company-sector combinations where production in t5 = 0 when
  # it was greater than 0 in t0.
  # (2) remove company-sector combinations where production is 0 for the entire
  # time frame from t0 to t5.
  rm_inactive_companies <- function(data,
                                    start_year,
                                    time_frame) {
    comp_sec_no_prod_t5 <- data %>%
      dplyr::filter(
        .data[["year"]] %in% c(.env[["start_year"]], .env[["start_year"]] + .env[["time_frame"]])
      ) %>%
      dplyr::summarise(
        sum_production = sum(.data[["production"]], na.rm = TRUE),
        .by = c("name_company", "sector", "year")
      ) %>%
      tidyr::pivot_wider(
        names_from = "year",
        names_prefix = "prod_",
        values_from = "sum_production"
      ) %>%
      dplyr::filter(
        .data[[paste0("prod_", start_year)]] > 0,
        .data[[paste0("prod_", start_year + time_frame)]] == 0
      ) %>%
      dplyr::distinct(
        .data[["name_company"]],
        .data[["sector"]]
      )

    comp_sec_no_prod_t0_to_t5 <- data %>%
      dplyr::filter(
        .data[["year"]] %in% c(.env[["start_year"]], .env[["start_year"]] + .env[["time_frame"]])
      ) %>%
      dplyr::summarise(
        sum_production = sum(.data[["production"]], na.rm = TRUE),
        .by = c("name_company", "sector")
      ) %>%
      dplyr::filter(
        .data[["sum_production"]] == 0
      ) %>%
      dplyr::distinct(
        .data[["name_company"]],
        .data[["sector"]]
      )

    data <- data %>%
      dplyr::anti_join(
        comp_sec_no_prod_t5,
        by = c("name_company", "sector")
      ) %>%
      dplyr::anti_join(
        comp_sec_no_prod_t0_to_t5,
        by = c("name_company", "sector")
      )

    return(data)
  }

  if (remove_inactive_companies) {
    abcd_keep <- abcd %>%
      rm_inactive_companies(
        start_year = start_year,
        time_frame = time_frame
      )

    abcd_removed <- abcd %>%
      dplyr::anti_join(
        abcd_keep,
        by = c("company_id", "sector")
      )

    # write removed inactive companies to file for inspection
    abcd_removed %>%
      readr::write_csv(
        file.path(dir_prepared_abcd, "abcd_removed_inactive_companies.csv"),
        na = ""
      )

    abcd <- abcd_keep

    rm(abcd_keep)
  }

  # write final version of abcd to file for use PACTA analysis
  abcd %>%
    readr::write_csv(
      file.path(dir_prepared_abcd, "abcd_final.csv"),
      na = ""
    )
}
