test_that("read_metadata errors on missing file", {
  expect_error(read_metadata("no_such_file.yaml"), "Metadata file not found")
})

test_that("read_metadata parses flat key-value fields", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines(c(
    'reference_date: "2025-12-31"',
    'entity: "Test Bank"',
    'template_version: "1.0"'
  ), tmp)

  meta <- read_metadata(tmp)
  expect_equal(meta$reference_date,   "2025-12-31")
  expect_equal(meta$entity,           "Test Bank")
  expect_equal(meta$template_version, "1.0")
})

test_that("read_metadata parses simple dimensions list", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines(c(
    'reference_date: "2025-12-31"',
    "dimensions: [metric, scenario, tenor]"
  ), tmp)

  meta   <- read_metadata(tmp)
  schema <- grillr:::dim_schema(meta)

  expect_equal(length(schema), 3L)
  expect_equal(schema[[1L]]$name, "metric")
  expect_equal(schema[[2L]]$name, "scenario")
  expect_equal(schema[[3L]]$name, "tenor")
  expect_true(schema[[1L]]$required)
})

test_that("read_metadata parses detailed dimensions block", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines(c(
    "dimensions:",
    "  - name: metric",
    "    required: true",
    "  - name: scenario",
    "    required: true",
    "    allowed: [base, stressed]",
    "  - name: tenor",
    "    required: false"
  ), tmp)

  meta   <- read_metadata(tmp)
  schema <- grillr:::dim_schema(meta)

  expect_equal(schema[[2L]]$name,               "scenario")
  expect_equal(schema[[2L]]$allowed,            list("base", "stressed"))
  expect_false(schema[[3L]]$required)
})

test_that("write_metadata_template creates parseable YAML with dimensions block", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))

  write_metadata_template(tmp, dimensions = c("metric", "scenario", "tenor"))
  expect_true(file.exists(tmp))

  meta   <- yaml::read_yaml(tmp)
  schema <- grillr:::dim_schema(meta)
  expect_equal(length(schema), 3L)
  expect_equal(schema[[1L]]$name, "metric")
})

test_that("write_metadata_template errors if file exists and overwrite = FALSE", {
  tmp <- tempfile(fileext = ".yaml")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(write_metadata_template(tmp, overwrite = FALSE), "already exists")
})
