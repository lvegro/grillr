test_that("read_metadata errors on missing file", {
  expect_error(read_metadata("no_such_file.yaml"), "Metadata file not found")
})

test_that("read_metadata parses a simple YAML file", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines(c(
    'reference_date: "2025-12-31"',
    'entity: "Test Bank"',
    'template_version: "1.0"'
  ), tmp)

  meta <- read_metadata(tmp)

  expect_equal(meta$reference_date,    "2025-12-31")
  expect_equal(meta$entity,            "Test Bank")
  expect_equal(meta$template_version,  "1.0")
})

test_that("write_metadata_template creates a parseable YAML file", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))

  write_metadata_template(tmp)
  expect_true(file.exists(tmp))

  # Must be valid YAML
  result <- yaml::read_yaml(tmp)
  expect_true(is.list(result))
})

test_that("write_metadata_template errors if file exists and overwrite = FALSE", {
  tmp <- tempfile(fileext = ".yaml")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(write_metadata_template(tmp, overwrite = FALSE), "already exists")
})
