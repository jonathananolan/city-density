library(RPostgres)
library(DBI)

# Verify raster alignment before adding covariate data
verify_raster_alignment <- function(
  con,
  base_table = "worldpop_2025",
  base_column = "rast", 
  new_raster_file = NULL,
  country_iso3 = NULL,
  verbose = TRUE
) {
  
  if (verbose) cat("=== Verifying Raster Alignment ===\n")
  
  # Get reference raster properties from base table
  if (verbose) cat("Getting reference raster properties...\n")
  
  base_query <- sprintf("
    SELECT 
      ST_SRID(%s) as srid,
      ST_ScaleX(%s) as scale_x, 
      ST_ScaleY(%s) as scale_y,
      ST_Width(%s) as width,
      ST_Height(%s) as height,
      ST_BandPixelType(%s, 1) as pixel_type
    FROM %s 
    LIMIT 1
  ", base_column, base_column, base_column, base_column, base_column, base_column, base_table)
  
  base_props <- dbGetQuery(con, base_query)
  
  if (nrow(base_props) == 0) {
    stop("No data found in base table: ", base_table)
  }
  
  if (verbose) {
    cat("Reference properties:\n")
    cat("  - SRID:", base_props$srid, "\n")
    cat("  - Scale:", base_props$scale_x, ",", base_props$scale_y, "\n") 
    cat("  - Tile size:", base_props$width, "x", base_props$height, "\n")
    cat("  - Pixel type:", base_props$pixel_type, "\n")
  }
  
  # If checking a file, load it temporarily to check alignment
  if (!is.null(new_raster_file)) {
    if (verbose) cat("\nChecking alignment with new raster file...\n")
    
    if (!file.exists(new_raster_file)) {
      stop("Raster file not found: ", new_raster_file)
    }
    
    # Create temporary table to load sample of new raster
    temp_table <- paste0("temp_alignment_check_", as.integer(Sys.time()))
    
    tryCatch({
      # Load just a few tiles for checking
      cmd <- sprintf(
        'raster2pgsql -s 4326 -c -t 100x100 %s %s | head -20 | psql -h localhost -p 5432 -U %s -d %s > /dev/null 2>&1',
        shQuote(new_raster_file),
        temp_table,
        Sys.getenv("POSTGRES_USER", "abrey"),
        "worldpop_db"
      )
      
      result <- system(cmd)
      if (result != 0) {
        stop("Failed to load sample raster data for alignment check")
      }
      
      # Check properties of new raster
      new_query <- sprintf("
        SELECT 
          ST_SRID(rast) as srid,
          ST_ScaleX(rast) as scale_x,
          ST_ScaleY(rast) as scale_y, 
          ST_Width(rast) as width,
          ST_Height(rast) as height,
          ST_BandPixelType(rast, 1) as pixel_type,
          COUNT(*) as sample_tiles
        FROM %s
        GROUP BY ST_SRID(rast), ST_ScaleX(rast), ST_ScaleY(rast), ST_Width(rast), ST_Height(rast), ST_BandPixelType(rast, 1)
      ", temp_table)
      
      new_props <- dbGetQuery(con, new_query)
      
      if (verbose) {
        cat("New raster properties:\n")
        cat("  - SRID:", new_props$srid, "\n")
        cat("  - Scale:", new_props$scale_x, ",", new_props$scale_y, "\n")
        cat("  - Tile size:", new_props$width, "x", new_props$height, "\n") 
        cat("  - Pixel type:", new_props$pixel_type, "\n")
        cat("  - Sample tiles:", new_props$sample_tiles, "\n")
      }
      
      # Check alignment
      alignment_issues <- c()
      
      if (base_props$srid != new_props$srid) {
        alignment_issues <- c(alignment_issues, paste("SRID mismatch:", base_props$srid, "vs", new_props$srid))
      }
      
      if (abs(base_props$scale_x - new_props$scale_x) > 1e-10) {
        alignment_issues <- c(alignment_issues, paste("Scale X mismatch:", base_props$scale_x, "vs", new_props$scale_x))
      }
      
      if (abs(base_props$scale_y - new_props$scale_y) > 1e-10) {
        alignment_issues <- c(alignment_issues, paste("Scale Y mismatch:", base_props$scale_y, "vs", new_props$scale_y))
      }
      
      # Check if pixel grids align using PostGIS function
      if (verbose) cat("\nChecking pixel grid alignment...\n")
      
      alignment_query <- sprintf("
        SELECT ST_SameAlignment(b.%s, n.rast) as grids_align
        FROM %s b, %s n 
        LIMIT 1
      ", base_column, base_table, temp_table)
      
      alignment_result <- dbGetQuery(con, alignment_query)
      
      if (!alignment_result$grids_align) {
        alignment_issues <- c(alignment_issues, "Pixel grids do not align - different grid origins or rotations")
      }
      
      if (length(alignment_issues) == 0) {
        if (verbose) cat("✓ Perfect alignment - rasters are compatible!\n")
        return(list(compatible = TRUE, issues = NULL))
      } else {
        if (verbose) {
          cat("✗ Alignment issues found:\n")
          for (issue in alignment_issues) {
            cat("  -", issue, "\n")
          }
        }
        return(list(compatible = FALSE, issues = alignment_issues))
      }
      
    }, finally = {
      # Clean up temporary table
      tryCatch({
        dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table))
      }, error = function(e) {})
    })
  }
  
  # If checking existing data by country
  if (!is.null(country_iso3)) {
    if (verbose) cat("Checking alignment within country data...\n")
    
    # Check if all tiles in this country have consistent alignment
    consistency_query <- sprintf("
      SELECT 
        COUNT(DISTINCT ST_ScaleX(%s)) as scale_x_variants,
        COUNT(DISTINCT ST_ScaleY(%s)) as scale_y_variants, 
        COUNT(DISTINCT ST_SRID(%s)) as srid_variants,
        COUNT(*) as total_tiles
      FROM %s 
      WHERE country_iso3 = '%s'
    ", base_column, base_column, base_column, base_table, country_iso3)
    
    consistency <- dbGetQuery(con, consistency_query)
    
    if (consistency$scale_x_variants > 1 || consistency$scale_y_variants > 1 || consistency$srid_variants > 1) {
      if (verbose) cat("⚠ Inconsistent properties within country", country_iso3, "\n")
      return(list(compatible = FALSE, issues = "Inconsistent raster properties within country"))
    } else {
      if (verbose) cat("✓ Consistent properties within country", country_iso3, "\n") 
      return(list(compatible = TRUE, issues = NULL))
    }
  }
  
  if (verbose) cat("✓ Reference raster properties verified\n")
  return(list(compatible = TRUE, issues = NULL, properties = base_props))
}

# Quick verification function for interactive use
check_raster_alignment <- function(
  new_raster_file,
  base_table = "worldpop_2025",
  db_name = "worldpop_db",
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
    result <- verify_raster_alignment(con, base_table, "rast", new_raster_file)
    return(result)
  }, finally = {
    dbDisconnect(con)
  })
}

# Export functions  
if (FALSE) {
  # Example usage:
  
  # Check if a new elevation raster aligns with existing population data
  result <- check_raster_alignment("data/elevation/afg_elevation_2025.tif")
  
  if (result$compatible) {
    cat("✓ Safe to add as new column\n")
  } else {
    cat("✗ Alignment issues:", result$issues, "\n")
  }
}