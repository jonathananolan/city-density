#!/usr/bin/env Rscript

# Knit the city ring area analysis RMarkdown document
# Run this script to generate the HTML report

library(rmarkdown)

cat("=== Knitting City Ring Area Analysis Report ===\n")

# Input and output paths
input_file <- "R/analysis/city_ring_area_analysis.Rmd"
output_dir <- "R/analysis/"

# Check if input file exists
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# Knit the document
tryCatch({
  cat("Knitting", input_file, "...\n")
  
  output_file <- render(
    input = input_file,
    output_dir = output_dir,
    quiet = FALSE
  )
  
  cat("✓ Successfully created:", output_file, "\n")
  cat("✓ Report is ready for viewing!\n\n")
  
  # Try to open in browser (optional)
  if (interactive() && file.exists(output_file)) {
    cat("Opening report in browser...\n")
    browseURL(output_file)
  }
  
}, error = function(e) {
  cat("✗ Error knitting document:\n")
  cat(e$message, "\n")
  stop("Knitting failed")
})

cat("=== Knitting Complete ===\n")