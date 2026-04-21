#' Read template-level metadata from a YAML file
#'
#' Parses an optional YAML sidecar file that holds two kinds of information:
#'
#' 1. **Template metadata** — flat key-value pairs appended as constant columns
#'    by [extract_data()] (e.g. `reference_date`, `entity`).
#' 2. **Dimension schema** — an optional `dimensions` block consumed by
#'    [parse_guide()] to order columns, flag missing required dimensions, and
#'    validate allowed values.
#'
#' @section YAML structure:
#' ```yaml
#' # ── Template metadata ──────────────────────────────────
#' reference_date: "2025-12-31"
#' entity: "Grillr Bank SA"
#' template_version: "2.1"
#' currency: "EUR"
#'
#' # ── Dimension schema ───────────────────────────────────
#' # Defines expected tag dimensions, their order, and optional constraints.
#' # Simple form (order only, all required):
#' #   dimensions: [metric, scenario, tenor]
#' #
#' # Detailed form:
#' dimensions:
#'   - name: metric
#'     required: true
#'   - name: scenario
#'     required: true
#'     allowed: [base, stressed, adverse]
#'   - name: tenor
#'     required: false
#' ```
#'
#' @param path  Path to the `.yaml` / `.yml` metadata file.
#'
#' @return A named list with all top-level YAML fields.  The `dimensions`
#'   element (if present) is passed as-is to [parse_guide()] via `metadata`.
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- read_metadata("template.yaml")
#'
#' # Parse guide using dimension schema from YAML
#' guide <- parse_guide("guide.xlsx", metadata = meta)
#'
#' # Extract data and attach metadata fields
#' result <- extract_data("q1_data.xlsx", guide, metadata = meta)
#' }
read_metadata <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste0("Metadata file not found: ", path), call = NULL)
  }

  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("yaml", "yml")) {
    rlang::warn(
      paste0("Unexpected file extension '.", ext, "' — expected .yaml or .yml."),
      call = NULL
    )
  }

  yaml::read_yaml(path)
}


#' Write a starter metadata YAML file
#'
#' Scaffolds a `.yaml` file with common template metadata fields and a
#' `dimensions` block ready to fill in.  Open the file and edit it before
#' passing to [read_metadata()].
#'
#' @param path        Path for the `.yaml` file to create.
#' @param fields      Character vector of flat metadata field names.
#' @param dimensions  Character vector of expected tag dimension names, written
#'   in order.  Each dimension gets a stub entry in the `dimensions` block.
#' @param overwrite   Overwrite an existing file.
#'
#' @return The path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' write_metadata_template(
#'   "template.yaml",
#'   dimensions = c("metric", "scenario", "tenor")
#' )
#' }
write_metadata_template <- function(path,
                                    fields     = c("reference_date",
                                                   "entity",
                                                   "template_version",
                                                   "currency"),
                                    dimensions = character(0L),
                                    overwrite  = FALSE) {
  if (file.exists(path) && !overwrite) {
    rlang::abort(
      paste0("File already exists (use overwrite = TRUE): ", path),
      call = NULL
    )
  }

  lines <- c(
    "# grillr metadata",
    "# Fill in values and pass this file to read_metadata().",
    "",
    "# ── Template metadata ─────────────────────────────────────────────────",
    paste0(fields, ': ""'),
    ""
  )

  if (length(dimensions) > 0L) {
    dim_block <- c(
      "# ── Dimension schema ──────────────────────────────────────────────────",
      "# Defines expected tag dimensions in canonical order.",
      "# required: true  → parse_guide() warns if this dimension is absent.",
      "# allowed:  [...]  → parse_guide() warns on values outside this list.",
      "dimensions:"
    )
    for (nm in dimensions) {
      dim_block <- c(dim_block,
        paste0("  - name: ", nm),
        "    required: true"
      )
    }
    lines <- c(lines, dim_block)
  }

  writeLines(lines, path)
  invisible(path)
}
