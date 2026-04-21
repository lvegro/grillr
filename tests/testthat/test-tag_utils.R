test_that("make_tag produces the expected format", {
  tag <- make_tag(metric = "regulatory_capital", scenario = "stressed", tenor = "t+3")
  expect_true(startsWith(tag, "[[grillr|"))
  expect_true(endsWith(tag, "]]"))
  expect_match(tag, "metric=regulatory_capital")
  expect_match(tag, "scenario=stressed")
  expect_match(tag, "tenor=t\\+3")
})

test_that("make_tag with no args returns empty tag", {
  tag <- make_tag()
  expect_equal(tag, "[[grillr|]]")
  expect_true(grillr:::is_grillr_tag(tag))
})

test_that("parse_tag round-trips make_tag", {
  tag    <- make_tag(metric = "rwa", scenario = "base", currency = "EUR")
  result <- parse_tag(tag)
  expect_equal(result$metric,   "rwa")
  expect_equal(result$scenario, "base")
  expect_equal(result$currency, "EUR")
})

test_that("parse_tag returns empty list for empty tag", {
  result <- parse_tag("[[grillr|]]")
  expect_equal(length(result), 0L)
})

test_that("values containing reserved characters round-trip cleanly", {
  tricky <- "key=val;another=pair"
  tag    <- make_tag(label = tricky)
  result <- parse_tag(tag)
  expect_equal(result$label, tricky)
})

test_that("make_tag errors on unnamed arguments", {
  expect_error(make_tag("unnamed"), "must be named")
})

test_that("parse_tag errors on non-tag input", {
  expect_error(parse_tag("not a tag"),     "Not a valid grillr tag")
  expect_error(parse_tag("[[grillr|broken"), "Not a valid grillr tag")
  expect_error(parse_tag(NA_character_),   "Not a valid grillr tag")
})

test_that("is_grillr_tag detects tags correctly", {
  expect_true( grillr:::is_grillr_tag(make_tag(x = "1")))
  expect_true( grillr:::is_grillr_tag("[[grillr|]]"))
  expect_false(grillr:::is_grillr_tag("plain text"))
  expect_false(grillr:::is_grillr_tag(NA_character_))
  expect_false(grillr:::is_grillr_tag(1L))
})
