test_that("build_guide errors on missing required columns", {
  bad_df <- data.frame(sheet = "S", row = 1, col = 1)
  expect_error(build_guide(bad_df), "Missing required columns")
})

test_that("build_guide errors when file exists and overwrite = FALSE", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  file.create(tmp)
  on.exit(unlink(tmp))

  cells <- data.frame(
    sheet      = "Sheet1",
    row        = 1L,
    col        = 1L,
    grillr_tag = make_tag("Sheet1", "A1", 1L, 1L, "character", "x"),
    stringsAsFactors = FALSE
  )
  expect_error(build_guide(cells, tmp, overwrite = FALSE), "already exists")
})

test_that("build_guide writes a readable xlsx file", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  cells <- data.frame(
    sheet      = "Sheet1",
    row        = c(1L, 2L),
    col        = c(1L, 2L),
    grillr_tag = c(
      make_tag("Sheet1", "A1", 1L, 1L, "character", "foo"),
      make_tag("Sheet1", "B2", 2L, 2L, "numeric",   "3.14")
    ),
    stringsAsFactors = FALSE
  )

  out <- build_guide(cells, tmp, overwrite = TRUE)
  expect_equal(out, tmp)
  expect_true(file.exists(tmp))
})
