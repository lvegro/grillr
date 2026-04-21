test_that("make_tag produces the expected format", {
  tag <- make_tag("Sheet1", "B3", 3L, 2L, "numeric", "42")
  expect_true(startsWith(tag, "[[grillr|"))
  expect_true(endsWith(tag, "]]"))
  expect_match(tag, "sheet=Sheet1")
  expect_match(tag, "address=B3")
  expect_match(tag, "row=3")
  expect_match(tag, "col=2")
  expect_match(tag, "type=numeric")
  expect_match(tag, "value=42")
})

test_that("parse_tag round-trips make_tag", {
  tag    <- make_tag("Data", "A1", 1L, 1L, "character", "Hello world")
  result <- parse_tag(tag)

  expect_equal(result$sheet,   "Data")
  expect_equal(result$address, "A1")
  expect_equal(result$row,     1L)
  expect_equal(result$col,     1L)
  expect_equal(result$type,    "character")
  expect_equal(result$value,   "Hello world")
})

test_that("NA value is encoded and decoded correctly", {
  tag    <- make_tag("Sheet1", "C5", 5L, 3L, "blank", NA)
  result <- parse_tag(tag)
  expect_true(is.na(result$value))
})

test_that("values containing reserved characters round-trip cleanly", {
  tricky <- "key=val;another=pair"
  tag    <- make_tag("S", "D4", 4L, 4L, "character", tricky)
  result <- parse_tag(tag)
  expect_equal(result$value, tricky)
})

test_that("parse_tag errors on non-tag input", {
  expect_error(parse_tag("not a tag"), "Not a valid grillr tag")
  expect_error(parse_tag("[[grillr|broken"), "Not a valid grillr tag")
})

test_that("is_grillr_tag detects tags correctly", {
  expect_true(grillr:::is_grillr_tag(make_tag("S", "A1", 1, 1, "numeric", "1")))
  expect_false(grillr:::is_grillr_tag("plain text"))
  expect_false(grillr:::is_grillr_tag(NA_character_))
})
