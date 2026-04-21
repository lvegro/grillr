test_that("parse_template errors on missing file", {
  expect_error(parse_template("no_such_file.xlsx"), "File not found")
})

test_that("parse_template returns expected columns on example file", {
  skip_if_not_installed("tidyxl")
  example <- system.file("extdata", "example_template.xlsx", package = "grillr")
  skip_if(example == "", "example_template.xlsx not installed")

  cells <- parse_template(example)

  expect_true(all(c("sheet", "address", "row", "col", "data_type",
                     "value", "grillr_tag") %in% names(cells)))
  expect_gt(nrow(cells), 0L)
})

test_that("parse_template grillr_tag column contains valid tags", {
  skip_if_not_installed("tidyxl")
  example <- system.file("extdata", "example_template.xlsx", package = "grillr")
  skip_if(example == "", "example_template.xlsx not installed")

  cells <- parse_template(example)
  tags  <- cells$grillr_tag

  expect_true(all(vapply(tags, grillr:::is_grillr_tag, logical(1L))))
})
