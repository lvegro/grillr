test_that("parse_guide errors on missing file", {
  expect_error(parse_guide("no_such_file.xlsx"), "File not found")
})

test_that("parse_guide returns expected columns and expands dimensions", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(guide_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Results")
  openxlsx::writeData(wb, "Results",
    make_tag(metric = "rwa", scenario = "stressed"),
    startRow = 2, startCol = 3, colNames = FALSE)
  openxlsx::writeData(wb, "Results",
    make_tag(metric = "capital", scenario = "base"),
    startRow = 5, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, guide_file, overwrite = TRUE)

  guide <- parse_guide(guide_file)

  expect_true(all(c("sheet", "address", "row", "col", "tag_raw",
                     "metric", "scenario") %in% names(guide)))
  expect_equal(nrow(guide), 2L)
  expect_equal(sort(guide$metric), c("capital", "rwa"))
})

test_that("parse_guide drops empty tags when skip_empty = TRUE", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(guide_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", "[[grillr|]]",
    startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Sheet1", make_tag(metric = "nii"),
    startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, guide_file, overwrite = TRUE)

  guide <- parse_guide(guide_file, skip_empty = TRUE)
  expect_equal(nrow(guide), 1L)
  expect_equal(guide$metric, "nii")
})

test_that("parse_guide fills NA for dimensions absent in some tags", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(guide_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "S")
  openxlsx::writeData(wb, "S", make_tag(metric = "rwa", tenor = "t+3"),
    startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, "S", make_tag(metric = "capital"),
    startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, guide_file, overwrite = TRUE)

  guide <- parse_guide(guide_file)
  expect_true("tenor" %in% names(guide))
  expect_true(is.na(guide$tenor[guide$metric == "capital"]))
})

test_that("parse_guide reorders columns to match schema", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(guide_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "S")
  # Tags have tenor before scenario, but schema says scenario comes first
  openxlsx::writeData(wb, "S",
    make_tag(metric = "rwa", tenor = "t+3", scenario = "stressed"),
    startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, guide_file, overwrite = TRUE)

  meta  <- list(dimensions = c("metric", "scenario", "tenor"))
  guide <- parse_guide(guide_file, metadata = meta)

  dim_cols <- setdiff(names(guide), c("sheet", "address", "row", "col", "tag_raw"))
  expect_equal(dim_cols, c("metric", "scenario", "tenor"))
})

test_that("parse_guide warns on required dimension missing from guide", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(guide_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "S")
  openxlsx::writeData(wb, "S", make_tag(metric = "rwa"),  # scenario absent
    startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, guide_file, overwrite = TRUE)

  meta <- list(dimensions = list(
    list(name = "metric",   required = TRUE),
    list(name = "scenario", required = TRUE)
  ))
  expect_warning(
    parse_guide(guide_file, metadata = meta),
    "Required dimensions missing"
  )
})

test_that("parse_guide warns on values outside allowed list", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx")

  guide_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(guide_file))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "S")
  openxlsx::writeData(wb, "S",
    make_tag(metric = "rwa", scenario = "typo_scenario"),
    startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::saveWorkbook(wb, guide_file, overwrite = TRUE)

  meta <- list(dimensions = list(
    list(name = "metric",   required = TRUE),
    list(name = "scenario", required = TRUE, allowed = list("base", "stressed"))
  ))
  expect_warning(
    parse_guide(guide_file, metadata = meta),
    "not in allowed list"
  )
})
