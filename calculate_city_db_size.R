library(dplyr)

# Database size estimation for city rings
calculate_city_db_size <- function(cities = 1500, rings = 150) {
  
  cat("=== City Rings Database Size Estimation ===\n")
  cat("Cities:", cities, "\n")
  cat("Rings per city:", rings, "\n\n")
  
  # Table 1: city_centers (one row per city per ring)
  centers_rows <- cities * rings
  centers_bytes_per_row <- 8 + 255 + 255 + 8 + 4 + 32  # bigint + varchar + varchar + bigint + int + point
  centers_size_mb <- (centers_rows * centers_bytes_per_row) / 1024 / 1024
  
  cat("TABLE 1: city_centers\n")
  cat("- Rows:", format(centers_rows, big.mark = ","), "\n")
  cat("- Size:", round(centers_size_mb, 1), "MB\n\n")
  
  # Table 2: city_circles (one row per city per ring)
  circles_rows <- cities * rings
  # Polygon geometry averages ~500 bytes for circles (depends on precision)
  circles_bytes_per_row <- 8 + 255 + 255 + 4 + 4 + 4 + 32 + 500 + 8  # + circle geometry
  circles_size_mb <- (circles_rows * circles_bytes_per_row) / 1024 / 1024
  
  cat("TABLE 2: city_circles\n")
  cat("- Rows:", format(circles_rows, big.mark = ","), "\n") 
  cat("- Size:", round(circles_size_mb, 1), "MB\n\n")
  
  # Table 3: city_annulus (one row per city per ring)
  annulus_rows <- cities * rings
  # Annulus polygons are more complex, ~800 bytes average
  annulus_bytes_per_row <- 8 + 255 + 255 + 4 + 4 + 4 + 4 + 32 + 800 + 8  # + annulus geometry
  annulus_size_mb <- (annulus_rows * annulus_bytes_per_row) / 1024 / 1024
  
  cat("TABLE 3: city_annulus\n")
  cat("- Rows:", format(annulus_rows, big.mark = ","), "\n")
  cat("- Size:", round(annulus_size_mb, 1), "MB\n\n")
  
  # Indexes (roughly 20-30% of table size for spatial data)
  total_table_size_mb <- centers_size_mb + circles_size_mb + annulus_size_mb
  index_overhead_mb <- total_table_size_mb * 0.25  # 25% overhead
  
  cat("INDEXES & OVERHEAD\n")
  cat("- Spatial indexes:", round(index_overhead_mb, 1), "MB\n\n")
  
  # Total
  total_size_mb <- total_table_size_mb + index_overhead_mb
  total_size_gb <- total_size_mb / 1024
  
  cat("=== TOTAL DATABASE SIZE ===\n")
  cat("Tables:", round(total_table_size_mb, 1), "MB\n")
  cat("Indexes:", round(index_overhead_mb, 1), "MB\n")
  cat("TOTAL:", round(total_size_mb, 1), "MB (", round(total_size_gb, 2), "GB)\n\n")
  
  # Comparison scenarios
  cat("=== SCALING SCENARIOS ===\n")
  
  scenarios <- data.frame(
    cities = c(500, 1000, 1500, 2000, 3000),
    rings = c(150, 150, 150, 150, 150)
  )
  
  scenarios$total_rows <- scenarios$cities * scenarios$rings * 3  # 3 tables
  scenarios$estimated_gb <- (scenarios$total_rows * 500 * 1.25) / 1024 / 1024 / 1024  # avg 500 bytes + 25% overhead
  
  print(scenarios)
  
  cat("\n=== MEMORY RECOMMENDATIONS ===\n")
  
  if (total_size_gb < 1) {
    cat("✓ SMALL: Any modern system can handle this\n")
    cat("- RAM needed: 2-4GB for PostgreSQL\n")
    cat("- Disk: Any SSD >10GB\n")
  } else if (total_size_gb < 5) {
    cat("✓ MEDIUM: Standard server/workstation\n")
    cat("- RAM needed: 8-16GB for PostgreSQL\n") 
    cat("- Disk: SSD recommended, >50GB\n")
  } else {
    cat("⚠ LARGE: Dedicated database server\n")
    cat("- RAM needed: 16-32GB for PostgreSQL\n")
    cat("- Disk: Fast SSD required, >100GB\n")
  }
  
  cat("\n=== QUERY PERFORMANCE ESTIMATES ===\n")
  cat("Single city analysis (150 rings): <1 second\n")
  cat("All cities aggregate: 5-30 seconds\n") 
  cat("Spatial joins with WorldPop: 1-10 minutes per city\n")
  
  return(list(
    total_size_mb = total_size_mb,
    total_size_gb = total_size_gb,
    total_rows = centers_rows + circles_rows + annulus_rows
  ))
}

# Run calculation
result <- calculate_city_db_size(cities = 1500, rings = 150)