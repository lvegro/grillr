test_that("extract_data errors on missing file", {
  guide <- data.frame(sheet = "S", row = 1L, col = 1L, tag_raw = "",
                      metric = "x", stringsAsFactors = FALSE)
  expect_error(extract_data("no_such_file.xlsx", guide), "File not found")
})

test_that("extract_data errors on guide missing required columns", {
  bad_guide <- data.frame(x = 1)
  expect_error(
    extract_data(tempfile(), bad_guide),
    "missing required columns"
  )
})

test_that("extract_data returns long dataset with value + dimensions", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  # Guide: two tagged cells
  guide <- data.frame(
    sheet    = c("Results", "Results"),
    row      = c(1L, 2L),
    col      = c(1L, 1L),
    tag_raw  = c(
      make_tag(metric = "rwa",     scenario = "stressed"),
      make_tag(metric = "capital", scenario = "base")
    ),
    metric   = c("rwa", "capital"),
    scenario = c("stressed", "base"),
    stringsAsFactors = FALSE
  )

  # Data file: matching layout
  data_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(data_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Results")
  openxlsx::writeData(wb, "Results", 1234.5,
    startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Results", 987.0,
    startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, data_file, overwrite = TRUE)

  result <- extract_data(data_file, guide)

  expect_equal(nrow(result), 2L)
  expect_true(all(c("value", "metric", "scenario") %in% names(result)))
  expect_true("1234.5" %in% result$value || "1234.5" %in% as.character(as.numeric(result$value)))
})

test_that("extract_data attaches metadata as constant columns", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide <- data.frame(
    sheet   = "S", row = 1L, col = 1L,
    tag_raw = make_tag(metric = "x"),
    metric  = "x",
    stringsAsFactors = FALSE
  )

  data_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(data_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "S")
  openxlsx::writeData(wb, "S", 42, startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, data_file, overwrite = TRUE)

  meta   <- list(reference_date = "2025-12-31", entity = "Test Bank")
  result <- extract_data(data_file, guide, metadata = meta)

  expect_equal(result$reference_date, "2025-12-31")
  expect_equal(result$entity,         "Test Bank")
})
