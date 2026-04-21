#' grillr: Parse and Extract Data from Excel Templates Using Functional Tags
#'
#' grillr implements a guide-based workflow for extracting structured,
#' labelled data from Excel files that follow a fixed template layout.
#'
#' ## Workflow
#'
#' ### 1. Create a guide skeleton
#' ```r
#' create_guide_skeleton("template.xlsx", "guide.xlsx")
#' ```
#' Opens as an Excel workbook where every occupied cell contains an empty
#' tag placeholder `[[grillr|]]`.  A hidden `_grillr_ref` sheet shows the
#' original cell values for reference.
#'
#' ### 2. Fill in the guide (manually)
#' Open `guide.xlsx` and replace each `[[grillr|]]` with a tag that
#' describes the data at that position in functional terms:
#' ```
#' [[grillr|metric=regulatory_capital;scenario=stressed;tenor=t+3]]
#' ```
#' Keys are free-form — define whatever dimensions make sense for your
#' template (metric, scenario, tenor, currency, entity, …).
#'
#' ### 3. Parse the guide
#' ```r
#' guide <- parse_guide("guide.xlsx")
#' # sheet | address | row | col | tag_raw | metric | scenario | tenor
#' ```
#'
#' ### 4. Extract data from actual files
#' ```r
#' meta   <- read_metadata("template.yaml")   # optional
#' result <- extract_data("q1_data.xlsx", guide, metadata = meta)
#' # sheet | address | row | col | value | metric | scenario | tenor | reference_date | …
#' ```
#'
#' The result is a long tidy dataset: one row per tagged cell, with the
#' observed value and all dimensions as columns.  Split the `tag_raw`
#' column or filter on dimension columns to subset the data as needed.
#'
#' @keywords internal
"_PACKAGE"
