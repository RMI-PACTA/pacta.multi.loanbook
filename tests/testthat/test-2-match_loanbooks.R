test_that("with known input, runs without error", {
  config <-
    list(
      directories = list(
        dir_input = file.path(test_tmpdir, "input"),
        dir_prepared_abcd = file.path(test_tmpdir, "output", "prepared_abcd"),
        dir_matched_loanbooks = file.path(test_tmpdir, "output", "matched_loanbooks")
      ),
      matching = list(
        params_match_name = list(
          by_sector = TRUE,
          min_score = 0.9,
          method = "jw",
          p = 0.1,
          overwrite = NULL,
          join_id = NULL
        ),
        manual_sector_classification = list(
          use_manual_sector_classification = FALSE
        )
      )
    )

  expect_no_error(suppressMessages(match_loanbooks(config)))
})
