#' Read template-level metadata from a YAML file
#'
#' Parses an optional YAML sidecar file that records information common to all
#' data files extracted from a given template (e.g. reference date, reporting
#' entity, template version).  The returned list can be passed directly to
#' [extract_data()] via its `metadata` argument, which appends each field as a
#' constant column.
#'
#' @section Expected YAML structure:
#' Any flat key-value pairs are accepted.  Example:
#' ```yaml
#' reference_date: "2025-12-31"
#' entity: "Grillr Bank SA"
#' template_version: "2.1"
#' currency: "EUR"
#' ```
#'
#' @param path  Path to the `.yaml` / `.yml` metadata file.
#'
#' @return A named list.  Values are kept as-is from YAML (character, numeric,
#'   logical, or `NULL`).
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- read_metadata("template.yaml")
#' meta$reference_date
#'
#' # Use with extract_data()
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


#' Write a metadata template YAML file
#'
#' Creates a starter `.yaml` file with common fields pre-populated as empty
#' strings.  Edit the file to fill in template-specific values.
#'
#' @param path        Path to write the `.yaml` file.
#' @param fields      Character vector of field names.  Defaults to a sensible
#'   starter set.
#' @param overwrite   Overwrite if the file already exists.
#'
#' @return The path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' write_metadata_template("template.yaml")
#' }
write_metadata_template <- function(path,
                                    fields    = c("reference_date",
                                                  "entity",
                                                  "template_version",
                                                  "currency"),
                                    overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    rlang::abort(
      paste0("File already exists (use overwrite = TRUE): ", path),
      call = NULL
    )
  }

  entries <- paste0(fields, ': ""', collapse = "\n")
  header  <- paste0(
    "# grillr metadata\n",
    "# Fill in the values below and pass this file to read_metadata().\n\n"
  )

  writeLines(paste0(header, entries), path)
  invisible(path)
}
