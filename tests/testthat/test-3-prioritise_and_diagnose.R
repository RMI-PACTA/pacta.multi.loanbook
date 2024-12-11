test_that("with known input, runs without error", {
  config <-
    list(
      directories = list(
        dir_input = file.path(test_tmpdir, "input"),
        dir_prepared_abcd = file.path(test_tmpdir, "output", "prepared_abcd"),
        dir_matched_loanbooks = file.path(test_tmpdir, "output", "matched_loanbooks"),
        dir_prioritized_loanbooks_and_diagnostics = file.path(test_tmpdir, "output", "prioritized_loanbooks_and_diagnostics")
      ),
      project_parameters = list(
        scenario_source = "weo_2022",
        scenario_select = "sds",
        region_select = "global",
        start_year = 2022L,
        time_frame = 5L,
        by_group = "group_id"
      ),
      sector_split = list(
        apply_sector_split = FALSE
      ),
      matching = list(
        manual_sector_classification = list(
          use_manual_sector_classification = FALSE
        )
      ),
      match_prioritize = list(
        priority =  NULL
      ),
      match_success_rate = list(
        plot_width = 12L,
        plot_height = 8L,
        plot_units = "in",
        plot_resolution = 300L
      )
    )

  matched_path <- file.path(test_tmpdir, "output/matched_loanbooks/matched_lbk_raw_loanbook_1.csv")
  manual_path <- file.path(test_tmpdir, "output/matched_loanbooks/matched_lbk_raw_loanbook_1_manual.csv")

  readr::read_csv(matched_path, show_col_types = FALSE) %>%
    dplyr::slice_max(order_by = score, by = id_loan) %>%
    dplyr::slice_head(by = id_loan) %>%
    readr::write_csv(manual_path, na = "")

  expect_no_error(suppressWarnings(prioritise_and_diagnose(config)))
})
