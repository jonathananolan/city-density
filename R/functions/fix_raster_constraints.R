library(RPostgres)
library(DBI)

# Fix raster constraints for QGIS compatibility
# Use this if constraints are missing or corrupted

fix_raster_constraints <- function(
  con,
  table_name = "worldpop_2025",
  verbose = TRUE
) {
  
  if (verbose) cat("=== Fixing Raster Constraints for QGIS ===\n")
  
  # Check if table exists
  if (!dbExistsTable(con, table_name)) {
    stop("Table '", table_name, "' does not exist")
  }
  
  # Check current constraint status
  if (verbose) {
    constraints <- dbGetQuery(con, sprintf("
      SELECT constraint_name FROM information_schema.table_constraints 
      WHERE table_name = '%s' AND constraint_type = 'CHECK' 
      AND constraint_name LIKE 'enforce_%%'
    ", table_name))
    
    cat("Current constraints:", nrow(constraints), "\n")
    if (nrow(constraints) > 0) {
      cat("  -", paste(constraints$constraint_name, collapse = "\n  - "), "\n")
    }
  }
  
  # Remove any existing problematic constraints
  if (verbose) cat("\nCleaning up existing constraints...\n")
  
  # Suppress PostgreSQL notices for cleaner output
  tryCatch(dbExecute(con, "SET client_min_messages TO WARNING"), error = function(e) {})
  
  constraint_names <- c(
    "enforce_srid_rast", "enforce_scalex_rast", "enforce_scaley_rast",
    "enforce_width_rast", "enforce_height_rast", "enforce_same_alignment_rast", 
    "enforce_num_bands_rast", "enforce_pixel_types_rast", "enforce_nodata_values_rast",
    "enforce_out_db_rast", "enforce_max_extent_rast"
  )
  
  for (constraint_name in constraint_names) {
    tryCatch({
      dbExecute(con, sprintf("ALTER TABLE %s DROP CONSTRAINT IF EXISTS %s", table_name, constraint_name))
    }, error = function(e) {
      if (verbose) cat("Warning: Could not drop", constraint_name, ":", e$message, "\n")
    })
  }
  
  # Restore normal message level
  tryCatch(dbExecute(con, "SET client_min_messages TO NOTICE"), error = function(e) {})
  
  # Add all constraints based on existing data
  if (verbose) cat("Adding fresh constraints based on current data...\n")
  
  tryCatch({
    result <- dbExecute(con, sprintf("SELECT AddRasterConstraints('%s', 'rast')", table_name))
    if (verbose) cat("✓ All raster constraints added successfully\n")
  }, error = function(e) {
    if (verbose) cat("✗ Error adding constraints:", e$message, "\n")
    stop("Failed to add raster constraints")
  })
  
  # Verify final state
  if (verbose) {
    cat("\nVerifying raster_columns catalog...\n")
    
    tryCatch({
      catalog <- dbGetQuery(con, sprintf("
        SELECT 
          blocksize_x, blocksize_y, 
          scale_x, scale_y,
          same_alignment,
          CASE WHEN extent IS NULL THEN 'NULL' ELSE 'HAS_EXTENT' END as extent_status
        FROM raster_columns WHERE r_table_name = '%s'
      ", table_name))
      
      cat("Catalog status:\n")
      cat("  - Blocksize:", catalog$blocksize_x, "x", catalog$blocksize_y, "\n")
      cat("  - Scale:", round(catalog$scale_x, 10), ",", round(catalog$scale_y, 10), "\n")
      cat("  - Alignment:", catalog$same_alignment, "\n")
      cat("  - Extent:", catalog$extent_status, "\n")
      
      if (catalog$extent_status == "HAS_EXTENT" && !is.na(catalog$blocksize_x)) {
        cat("✓ QGIS should work properly now!\n")
      } else {
        cat("⚠ QGIS may still have issues - check catalog values\n")
      }
      
    }, error = function(e) {
      cat("✗ Error checking catalog:", e$message, "\n")
      cat("This suggests catalog parsing issues - constraints may be malformed\n")
    })
  }
  
  if (verbose) cat("\n=== Constraint Fix Complete ===\n")
  return(TRUE)
}

# Quick fix function for interactive use
fix_worldpop_constraints <- function(
  db_name = "worldpop_db",
  table_name = "worldpop_2025", 
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey")
) {
  
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = port,
    user = user
  )
  
  tryCatch({
    result <- fix_raster_constraints(con, table_name)
    return(result)
  }, finally = {
    dbDisconnect(con)
  })
}

# Export functions
if (FALSE) {
  # Example usage:
  
  # Method 1: With existing connection
  con <- dbConnect(RPostgres::Postgres(), dbname="worldpop_db", host="localhost", port=5432, user="abrey")
  fix_raster_constraints(con, "worldpop_2025")
  dbDisconnect(con)
  
  # Method 2: Quick fix (auto-connects)
  fix_worldpop_constraints()
  
}