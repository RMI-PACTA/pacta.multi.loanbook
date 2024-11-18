test_that("with known input, runs without error", {
  config <-
    list(
      directories = list(
        dir_input = file.path(test_tmpdir, "input"),
        dir_prepared_abcd = file.path(test_tmpdir, "output", "prepared_abcd")
      ),
      file_names = list(
        filename_abcd = "ABCD.xlsx",
        sheet_abcd = "Company Indicators - PACTA Comp"
      ),
      project_parameters = list(
        start_year = 2022L,
        time_frame = 5L
      ),
      sector_split = list(
        apply_sector_split = FALSE
      ),
      prepare_abcd = list(
        remove_inactive_companies = TRUE
      )
    )

  expect_no_condition(prepare_abcd(config))
})
