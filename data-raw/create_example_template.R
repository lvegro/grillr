# Run this script once to (re)generate inst/extdata/example_template.xlsx
# and inst/extdata/example_guide.xlsx.
# Requires: openxlsx, grillr (installed or loaded via devtools::load_all())

library(openxlsx)
devtools::load_all()

# ── Template ──────────────────────────────────────────────────────────────────
wb_tmpl <- createWorkbook()
addWorksheet(wb_tmpl, "Results")

# Header row
writeData(wb_tmpl, "Results",
  data.frame(t(c("Metric", "Scenario", "Tenor", "Value"))),
  startRow = 1, startCol = 1, colNames = FALSE)

# Data rows
rows <- data.frame(
  Metric   = c("Regulatory Capital", "Regulatory Capital", "RWA", "RWA"),
  Scenario = c("Base", "Stressed", "Base", "Stressed"),
  Tenor    = c("T+0", "T+3", "T+0", "T+3"),
  Value    = c(1200.0, 1050.0, 8500.0, 9200.0)
)
writeData(wb_tmpl, "Results", rows, startRow = 2, startCol = 1, colNames = FALSE)

out_tmpl <- here::here("inst", "extdata", "example_template.xlsx")
saveWorkbook(wb_tmpl, out_tmpl, overwrite = TRUE)
message("Written: ", out_tmpl)

# ── Guide skeleton (then fill in manually) ────────────────────────────────────
out_guide <- here::here("inst", "extdata", "example_guide.xlsx")
create_guide_skeleton(out_tmpl, out_guide, overwrite = TRUE)
message("Written guide skeleton: ", out_guide)
message("Open example_guide.xlsx and fill in the [[grillr|]] tags.")
