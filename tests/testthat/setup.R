# Run before any test
file.copy(test_path("test-data"), tempdir(), recursive = TRUE)
test_tmpdir <- file.path(tempdir(), "test-data")


# Run after all tests
withr::defer(unlink(test_tmpdir), teardown_env())
