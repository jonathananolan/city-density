library(RPostgres)
library(DBI)
library(dplyr)
library(ggplot2)
library(sf)

# Test area calculations across all cities and ring sizes
# Analyzes variation in ring areas to identify coordinate system issues

test_area_calculations <- function(
  db_name = "worldpop_db",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("POSTGRES_USER", "abrey"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  annulus_table = "city_annulus",
  circles_table = "city_circles",
  output_plots = TRUE
) {
  
  cat("=== Testing Area Calculations Across All Cities ===\n")
  
  # Connect to database
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = port,
    user = user,
    password = password
  )
  
  tryCatch({
    
    # Check what tables exist
    cat("Checking available tables...\n")
    
    annulus_exists <- dbExistsTable(con, annulus_table)
    circles_exists <- dbExistsTable(con, circles_table)
    
    cat("Annulus table exists:", annulus_exists, "\n")
    cat("Circles table exists:", circles_exists, "\n")
    
    if (!annulus_exists && !circles_exists) {
      stop("Neither annulus nor circles table found. Run the ring generation first.")
    }
    
    # === ANALYZE CIRCLES (if available) ===
    if (circles_exists) {
      cat("\n=== ANALYZING CIRCLES TABLE ===\n")
      
      # Get circle data with proper area measurements
      circles_query <- sprintf("
        SELECT 
          geoname_id,
          city_name,
          country,
          ring_number,
          ring_radius_km,
          circle_area_m2 as stored_area_m2,
          ST_Area(circle_geom::geography) as geography_area_m2,
          ST_Area(circle_geom) as geometry_area_deg2,
          ST_Y(ST_Centroid(center_point)) as center_lat
        FROM %s
        WHERE ring_number <= 10  -- Focus on first 10 rings for analysis
        ORDER BY geoname_id, ring_number
      ", circles_table)
      
      circles_data <- dbGetQuery(con, circles_query)
      
      if (nrow(circles_data) > 0) {
        cat("Loaded", nrow(circles_data), "circle records\n")
        
        # Calculate theoretical circle areas
        circles_data <- circles_data %>%
          mutate(
            # Theoretical area in m2 for perfect circle
            theoretical_area_m2 = pi * (ring_radius_km * 1000)^2,
            # Compare different area calculations
            stored_vs_theoretical_ratio = stored_area_m2 / theoretical_area_m2,
            geography_vs_theoretical_ratio = geography_area_m2 / theoretical_area_m2,
            # Latitude category for analysis
            lat_category = case_when(
              abs(center_lat) < 15 ~ "Equatorial (0-15°)",
              abs(center_lat) < 45 ~ "Temperate (15-45°)",
              TRUE ~ "Polar (45°+)"
            )
          )
        
        # Summary statistics by ring size
        circles_summary <- circles_data %>%
          group_by(ring_radius_km) %>%
          summarise(
            cities = n(),
            stored_area_cv = sd(stored_area_m2) / mean(stored_area_m2),
            geography_area_cv = sd(geography_area_m2) / mean(geography_area_m2),
            stored_vs_theoretical_mean = mean(stored_vs_theoretical_ratio),
            geography_vs_theoretical_mean = mean(geography_vs_theoretical_ratio),
            stored_vs_theoretical_sd = sd(stored_vs_theoretical_ratio),
            geography_vs_theoretical_sd = sd(geography_vs_theoretical_ratio),
            .groups = 'drop'
          )
        
        cat("\nCircle Area Analysis by Ring Size:\n")
        print(circles_summary)
        
        # Analysis by latitude
        circles_lat_summary <- circles_data %>%
          filter(ring_radius_km == 5) %>%  # Focus on 5km rings
          group_by(lat_category) %>%
          summarise(
            cities = n(),
            stored_area_mean = mean(stored_area_m2),
            geography_area_mean = mean(geography_area_m2),
            theoretical_area = mean(theoretical_area_m2),
            stored_vs_theoretical = mean(stored_vs_theoretical_ratio),
            geography_vs_theoretical = mean(geography_vs_theoretical_ratio),
            .groups = 'drop'
          )
        
        cat("\nCircle Area Analysis by Latitude (5km rings):\n")
        print(circles_lat_summary)
        
        # === EQUAL AREA VERIFICATION ===
        cat("\n=== EQUAL AREA VERIFICATION FOR CIRCLES ===\n")
        
        # Check area consistency within each ring size
        equal_area_check <- circles_data %>%
          group_by(ring_radius_km) %>%
          summarise(
            cities = n(),
            geography_area_min = min(geography_area_m2),
            geography_area_max = max(geography_area_m2),
            geography_area_mean = mean(geography_area_m2),
            geography_area_cv = sd(geography_area_m2) / mean(geography_area_m2) * 100,
            stored_area_min = min(stored_area_m2),
            stored_area_max = max(stored_area_m2),
            stored_area_mean = mean(stored_area_m2),
            stored_area_cv = sd(stored_area_m2) / mean(stored_area_m2) * 100,
            area_variation_pct = (geography_area_max - geography_area_min) / geography_area_mean * 100,
            .groups = 'drop'
          )
        
        cat("Equal Area Analysis (Geography Method - should be consistent):\n")
        print(equal_area_check %>% select(ring_radius_km, cities, geography_area_cv, area_variation_pct))
        
        # Flag problematic ring sizes
        problem_rings <- equal_area_check %>%
          filter(geography_area_cv > 5 | area_variation_pct > 10)  # >5% CV or >10% variation
        
        if (nrow(problem_rings) > 0) {
          cat("\n⚠ EQUAL AREA VIOLATIONS DETECTED:\n")
          cat("Ring sizes with >5% coefficient of variation or >10% area variation:\n")
          print(problem_rings %>% select(ring_radius_km, geography_area_cv, area_variation_pct))
        } else {
          cat("\n✓ EQUAL AREA REQUIREMENT MET for all ring sizes\n")
        }
        
        # Detailed analysis for worst case
        if (nrow(problem_rings) > 0) {
          worst_ring <- problem_rings$ring_radius_km[which.max(problem_rings$geography_area_cv)]
          cat(sprintf("\nDetailed analysis for worst ring (%d km):\n", worst_ring))
          
          worst_ring_cities <- circles_data %>%
            filter(ring_radius_km == worst_ring) %>%
            mutate(
              area_ratio_vs_mean = geography_area_m2 / mean(geography_area_m2),
              area_deviation_pct = (geography_area_m2 - mean(geography_area_m2)) / mean(geography_area_m2) * 100
            ) %>%
            arrange(desc(abs(area_deviation_pct))) %>%
            head(10)
          
          print(worst_ring_cities %>% 
            select(city_name, center_lat, geography_area_m2, area_ratio_vs_mean, area_deviation_pct))
        }
        
        # Create plots if requested
        if (output_plots) {
          
          # Plot 1: Area variation by ring size
          p1 <- circles_data %>%
            filter(ring_radius_km <= 10) %>%
            ggplot(aes(x = ring_radius_km, y = geography_vs_theoretical_ratio, color = lat_category)) +
            geom_point(alpha = 0.6) +
            geom_smooth(method = "loess", se = FALSE) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
            labs(
              title = "Circle Area Accuracy vs Theoretical (Geography Method)",
              x = "Ring Radius (km)",
              y = "Actual Area / Theoretical Area Ratio",
              color = "Latitude Zone",
              subtitle = "Ratio should be 1.0 for perfect circles. Geography method accounts for Earth curvature."
            ) +
            theme_minimal()
          
          print(p1)
          
          # Plot 2: Stored vs Geography area comparison
          p2 <- circles_data %>%
            filter(ring_radius_km <= 10) %>%
            ggplot(aes(x = stored_area_m2, y = geography_area_m2, color = abs(center_lat))) +
            geom_point(alpha = 0.6) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
            scale_color_viridis_c(name = "Abs Latitude") +
            labs(
              title = "Stored Area vs Geography-Calculated Area",
              x = "Stored Area (m²)",
              y = "Geography Area (m²)",
              subtitle = "Points should fall on red line if methods agree"
            ) +
            theme_minimal()
          
          print(p2)
          
          # Plot 3: Equal area verification - coefficient of variation by ring size
          p3 <- equal_area_check %>%
            ggplot(aes(x = ring_radius_km)) +
            geom_col(aes(y = geography_area_cv), fill = "steelblue", alpha = 0.7) +
            geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
            labs(
              title = "Area Consistency Check - Geography Method",
              x = "Ring Radius (km)",
              y = "Coefficient of Variation (%)",
              subtitle = "Red line = 5% threshold. Lower is better for equal-area requirement."
            ) +
            theme_minimal()
          
          print(p3)
          
          # Plot 4: Area distribution for a specific ring size
          sample_ring <- 5  # 5km rings
          p4 <- circles_data %>%
            filter(ring_radius_km == sample_ring) %>%
            ggplot(aes(x = geography_area_m2 / 1e6)) +  # Convert to km²
            geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
            geom_vline(aes(xintercept = mean(geography_area_m2) / 1e6), 
                      linetype = "dashed", color = "red", size = 1) +
            labs(
              title = sprintf("Area Distribution for %d km Rings", sample_ring),
              x = "Area (km²)",
              y = "Number of Cities",
              subtitle = "Red line = mean. Should be tightly distributed for equal areas."
            ) +
            theme_minimal()
          
          print(p4)
          
          # Plot 5: Annulus area consistency by ring number  
          if (exists("annulus_equal_area_check") && nrow(annulus_equal_area_check) > 0) {
            p5 <- annulus_equal_area_check %>%
              ggplot(aes(x = ring_number, y = geography_area_cv)) +
              geom_col(fill = "darkgreen", alpha = 0.7) +
              geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
              labs(
                title = "Annulus Area Consistency by Ring Number",
                x = "Ring Number", 
                y = "Coefficient of Variation (%)",
                subtitle = "Red line = 5% threshold. Ring 2+ should have equal areas for each ring."
              ) +
              theme_minimal()
            
            print(p5)
          }
        }
      }
    }
    
    # === ANALYZE ANNULUS (if available) ===
    if (annulus_exists) {
      cat("\n=== ANALYZING ANNULUS TABLE ===\n")
      
      # Get annulus data with proper area measurements
      annulus_query <- sprintf("
        SELECT 
          geoname_id,
          city_name,
          country,
          ring_number,
          ring_radius_km,
          inner_radius_km,
          outer_radius_km,
          annulus_area_m2 as stored_area_m2,
          ST_Area(annulus_geom::geography) as geography_area_m2,
          ST_Area(annulus_geom) as geometry_area_deg2,
          ST_Y(ST_Centroid(center_point)) as center_lat
        FROM %s
        WHERE ring_number <= 10  -- Focus on first 10 rings for analysis
        ORDER BY geoname_id, ring_number
      ", annulus_table)
      
      annulus_data <- dbGetQuery(con, annulus_query)
      
      if (nrow(annulus_data) > 0) {
        cat("Loaded", nrow(annulus_data), "annulus records\n")
        
        # Calculate theoretical annulus areas
        annulus_data <- annulus_data %>%
          mutate(
            # Theoretical annulus area = π(R²-r²) where R=outer, r=inner
            theoretical_area_m2 = pi * ((outer_radius_km * 1000)^2 - (inner_radius_km * 1000)^2),
            # For ring 1, should equal full circle (inner = 0)
            theoretical_area_m2 = ifelse(ring_number == 1, 
                                       pi * (ring_radius_km * 1000)^2, 
                                       theoretical_area_m2),
            # Compare different area calculations
            stored_vs_theoretical_ratio = stored_area_m2 / theoretical_area_m2,
            geography_vs_theoretical_ratio = geography_area_m2 / theoretical_area_m2,
            # Latitude category for analysis
            lat_category = case_when(
              abs(center_lat) < 15 ~ "Equatorial (0-15°)",
              abs(center_lat) < 45 ~ "Temperate (15-45°)",
              TRUE ~ "Polar (45°+)"
            )
          )
        
        # Summary statistics by ring size
        annulus_summary <- annulus_data %>%
          group_by(ring_number, outer_radius_km) %>%
          summarise(
            cities = n(),
            stored_area_cv = sd(stored_area_m2) / mean(stored_area_m2),
            geography_area_cv = sd(geography_area_m2) / mean(geography_area_m2),
            stored_vs_theoretical_mean = mean(stored_vs_theoretical_ratio),
            geography_vs_theoretical_mean = mean(geography_vs_theoretical_ratio),
            stored_vs_theoretical_sd = sd(stored_vs_theoretical_ratio),
            geography_vs_theoretical_sd = sd(geography_vs_theoretical_ratio),
            .groups = 'drop'
          )
        
        cat("\nAnnulus Area Analysis by Ring Number:\n")
        print(annulus_summary)
        
        # === EQUAL AREA VERIFICATION FOR ANNULUS ===
        cat("\n=== EQUAL AREA VERIFICATION FOR ANNULUS ===\n")
        
        # Check area consistency for annulus rings (excluding ring 1 which is full circle)
        annulus_equal_area_check <- annulus_data %>%
          filter(ring_number > 1) %>%  # Only annulus rings, not full circles
          group_by(ring_number, outer_radius_km) %>%
          summarise(
            cities = n(),
            geography_area_min = min(geography_area_m2),
            geography_area_max = max(geography_area_m2),
            geography_area_mean = mean(geography_area_m2),
            geography_area_cv = sd(geography_area_m2) / mean(geography_area_m2) * 100,
            area_variation_pct = (geography_area_max - geography_area_min) / geography_area_mean * 100,
            theoretical_area = mean(theoretical_area_m2),
            .groups = 'drop'
          )
        
        cat("Annulus Equal Area Analysis (Ring 2+ only):\n")
        print(annulus_equal_area_check %>% 
          select(ring_number, cities, geography_area_cv, area_variation_pct, theoretical_area))
        
        # Flag problematic annulus rings
        problem_annulus_rings <- annulus_equal_area_check %>%
          filter(geography_area_cv > 5 | area_variation_pct > 10)
        
        if (nrow(problem_annulus_rings) > 0) {
          cat("\n⚠ ANNULUS EQUAL AREA VIOLATIONS DETECTED:\n")
          print(problem_annulus_rings %>% 
            select(ring_number, geography_area_cv, area_variation_pct))
        } else {
          cat("\n✓ EQUAL AREA REQUIREMENT MET for all annulus rings\n")
        }
        
        # Compare annulus area consistency vs circles
        cat("\n=== COMPARISON: Circle vs Annulus Area Consistency ===\n")
        
        # Get ring 1 from both tables for comparison
        circle_ring1_cv <- equal_area_check %>% 
          filter(ring_radius_km == 1) %>% 
          pull(geography_area_cv)
        
        annulus_ring1_cv <- annulus_data %>%
          filter(ring_number == 1) %>%
          summarise(cv = sd(geography_area_m2) / mean(geography_area_m2) * 100) %>%
          pull(cv)
        
        cat("Ring 1 area consistency:\n")
        cat("  Circles table CV:", round(circle_ring1_cv, 3), "%\n")
        cat("  Annulus table CV:", round(annulus_ring1_cv, 3), "%\n")
        
        if (abs(circle_ring1_cv - annulus_ring1_cv) < 0.1) {
          cat("  ✓ Ring 1 consistency matches between tables\n")
        } else {
          cat("  ⚠ Ring 1 consistency differs between tables\n")
        }
        
        # Check for problematic annulus geometries
        problem_annulus <- annulus_data %>%
          filter(
            stored_vs_theoretical_ratio < 0.5 | stored_vs_theoretical_ratio > 2.0 |
            geography_vs_theoretical_ratio < 0.5 | geography_vs_theoretical_ratio > 2.0 |
            is.na(stored_area_m2) | is.na(geography_area_m2)
          ) %>%
          select(geoname_id, city_name, ring_number, stored_vs_theoretical_ratio, 
                geography_vs_theoretical_ratio, center_lat)
        
        if (nrow(problem_annulus) > 0) {
          cat("\nProblematic Annulus Geometries (ratio < 0.5 or > 2.0):\n")
          print(head(problem_annulus, 20))
          cat("Total problematic rings:", nrow(problem_annulus), "\n")
        }
        
        # Create plots if requested
        if (output_plots) {
          
          # Plot 3: Annulus area accuracy by ring number
          p3 <- annulus_data %>%
            filter(ring_number <= 10) %>%
            ggplot(aes(x = ring_number, y = geography_vs_theoretical_ratio, color = lat_category)) +
            geom_point(alpha = 0.6) +
            geom_smooth(method = "loess", se = FALSE) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
            labs(
              title = "Annulus Area Accuracy vs Theoretical (Geography Method)",
              x = "Ring Number",
              y = "Actual Area / Theoretical Area Ratio",
              color = "Latitude Zone",
              subtitle = "Ring 1 = full circle, Ring 2+ = annulus. Ratio should be 1.0."
            ) +
            theme_minimal()
          
          print(p3)
          
          # Plot 4: Problem detection scatter
          p4 <- annulus_data %>%
            filter(ring_number <= 10) %>%
            ggplot(aes(x = center_lat, y = geography_vs_theoretical_ratio, color = ring_number)) +
            geom_point(alpha = 0.7) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
            geom_hline(yintercept = c(0.5, 2.0), linetype = "dotted", color = "orange") +
            scale_color_viridis_c() +
            labs(
              title = "Annulus Area Issues by Latitude and Ring Number",
              x = "Center Latitude",
              y = "Geography Area / Theoretical Area Ratio",
              color = "Ring Number",
              subtitle = "Points outside orange lines indicate coordinate system issues"
            ) +
            theme_minimal()
          
          print(p4)
        }
      }
    }
    
    # === COMPARISON ANALYSIS ===
    if (circles_exists && annulus_exists) {
      cat("\n=== COMPARING CIRCLES VS ANNULUS ===\n")
      
      # Compare first ring of annulus vs circles table
      comparison_query <- sprintf("
        SELECT 
          c.geoname_id,
          c.city_name,
          c.ring_radius_km,
          c.circle_area_m2 as circle_stored_area,
          ST_Area(c.circle_geom::geography) as circle_geography_area,
          a.annulus_area_m2 as annulus_stored_area,
          ST_Area(a.annulus_geom::geography) as annulus_geography_area
        FROM %s c
        JOIN %s a ON c.geoname_id = a.geoname_id AND c.ring_number = a.ring_number
        WHERE c.ring_number = 1  -- First ring should be identical
        ORDER BY c.geoname_id
        LIMIT 100
      ", circles_table, annulus_table)
      
      comparison_data <- dbGetQuery(con, comparison_query)
      
      if (nrow(comparison_data) > 0) {
        comparison_data <- comparison_data %>%
          mutate(
            circle_vs_annulus_stored = circle_stored_area / annulus_stored_area,
            circle_vs_annulus_geography = circle_geography_area / annulus_geography_area
          )
        
        cat("Circle vs Annulus Ring 1 Comparison (should be identical):\n")
        cat("Stored area ratio mean:", round(mean(comparison_data$circle_vs_annulus_stored), 4), "\n")
        cat("Geography area ratio mean:", round(mean(comparison_data$circle_vs_annulus_geography), 4), "\n")
        cat("Stored area ratio SD:", round(sd(comparison_data$circle_vs_annulus_stored), 4), "\n")
        cat("Geography area ratio SD:", round(sd(comparison_data$circle_vs_annulus_geography), 4), "\n")
        
        if (abs(mean(comparison_data$circle_vs_annulus_geography) - 1.0) > 0.01) {
          cat("⚠ ISSUE: Ring 1 circles and annulus have different areas!\n")
        } else {
          cat("✓ Ring 1 circles and annulus match correctly\n")
        }
      }
    }
    
    cat("\n=== RECOMMENDATIONS ===\n")
    cat("1. Use ST_Area(geom::geography) for accurate area calculations\n")
    cat("2. Geography method accounts for Earth's curvature automatically\n")
    cat("3. Check coordinate system transformations if ratios are far from 1.0\n")
    cat("4. Annulus geometries may need ST_Transform fixes in generation code\n")
    
  }, finally = {
    dbDisconnect(con)
  })
  
  return(TRUE)
}

# Run the analysis
cat("Starting area calculation analysis...\n")
test_area_calculations()