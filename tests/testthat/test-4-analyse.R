test_that("with known input, runs without error", {
  config <-
    list(
      directories = list(
        dir_input = file.path(test_tmpdir, "input"),
        dir_prepared_abcd = file.path(test_tmpdir, "output", "prepared_abcd"),
        dir_matched_loanbooks = file.path(test_tmpdir, "output", "matched_loanbooks"),
        dir_prioritized_loanbooks_and_diagnostics = file.path(test_tmpdir, "output", "prioritized_loanbooks_and_diagnostics"),
        dir_analysis = file.path(test_tmpdir, "output", "analysis")
      ),
      file_names = list(
        filename_scenario_tms = "scenario_data_tms.csv",
        filename_scenario_sda = "scenario_data_sda.csv",
        filename_abcd = "ABCD.xlsx",
        sheet_abcd = "Company Indicators - PACTA Comp"
      ),
      project_parameters = list(
        scenario_source = "weo_2022",
        scenario_select = "nze_2050",
        region_select = "global",
        start_year = 2022L,
        time_frame = 5L,
        by_group = "group_id"
      ),
      sector_split = list(
        apply_sector_split = FALSE
      )
    )

  expect_no_error(suppressMessages(suppressWarnings(analyse(config))))
})
