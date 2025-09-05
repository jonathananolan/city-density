# Get list of countries directly from WorldPop FTP server

get_worldpop_countries_ftp <- function(year = 2025, version = "R2025A", resolution = 100) {
  
  # FTP directory to list recursively
  ftp_base <- paste0("ftp://ftp.worldpop.org/GIS/Population/Global_2015_2030/", version, "/", year, "/")
  
  cat("Getting recursive file listing from FTP server:", ftp_base, "\n")
  cat("Looking for", resolution, "m constrained files...\n")
  
  # Method 1: First get list of country directories, then check each one
  tryCatch({
    # Get list of country directories first
    result <- system2("curl", args = c(
      "--list-only",
      "--ftp-pasv",
      shQuote(ftp_base)
    ), stdout = TRUE, stderr = FALSE)
    
    if (length(result) > 0) {
      # Filter for 3-letter country codes from directory listing
      country_dirs <- result[nchar(result) == 3 & grepl("^[A-Z]{3}$", result)]
      
      if (length(country_dirs) > 0) {
        cat("Found", length(country_dirs), "country directories\n")
        cat("First 10:", paste(head(country_dirs, 10), collapse = ", "), "\n")
        return(sort(country_dirs))
      } else {
        stop("No country directories found")
      }
    } else {
      stop("No recursive listing returned")
    }
    
  }, error = function(e) {
    cat("FTP listing failed:", e$message, "\n")
    
    # Fallback: Use lftp if available
    tryCatch({
      cat("Trying with lftp...\n")
      result <- system2("lftp", args = c(
        "-c", 
        paste0("'open ", ftp_base, "; ls; quit'")
      ), stdout = TRUE, stderr = FALSE)
      
      # Parse lftp output (usually has more details)
      countries <- gsub(".*\\s([A-Z]{3})\\s*$", "\\1", result)
      countries <- countries[nchar(countries) == 3 & grepl("^[A-Z]{3}$", countries)]
      countries <- sort(unique(countries))
      
      if (length(countries) > 0) {
        cat("Found", length(countries), "countries with lftp\n")
        return(countries)
      } else {
        stop("lftp also failed")
      }
      
    }, error = function(e2) {
      cat("Both curl and lftp failed, using fallback list\n")
      
      # Fallback to a curated list based on what we know works
      fallback_countries <- c(
          'ABW', 'AFG', 'AGO', 'AIA', 'ALA', 'ALB', 'AND', 'ARE', 'ARG', 
          'ARM', 'ASM', 'ATF', 'ATG', 'AUS', 'AUT', 'AZE', 'BDI', 'BEL', 
          'BEN', 'BES', 'BFA', 'BGD', 'BGR', 'BHR', 'BHS', 'BIH', 'BLM', 
          'BLR', 'BLZ', 'BMU', 'BOL', 'BRA', 'BRB', 'BRN', 'BTN', 'BVT', 
          'BWA', 'CAF', 'CAN', 'CCK', 'CHE', 'CHL', 'CHN', 'CIV', 'CMR', 
          'COD', 'COG', 'COK', 'COL', 'COM', 'CPT', 'CPV', 'CRI', 'CUB', 
          'CUW', 'CXR', 'CYM', 'CYP', 'CZE', 'DEU', 'DJI', 'DMA', 'DNK', 
          'DOM', 'DZA', 'ECU', 'EGY', 'ERI', 'ESH', 'ESP', 'EST', 'ETH', 
          'FIN', 'FJI', 'FLK', 'FRA', 'FRO', 'FSM', 'GAB', 'GBR', 'GEO', 
          'GGY', 'GHA', 'GIB', 'GIN', 'GLP', 'GMB', 'GNB', 'GNQ', 'GRC', 
          'GRD', 'GRL', 'GTM', 'GUF', 'GUM', 'GUY', 'HKG', 'HMD', 'HND', 
          'HRV', 'HTI', 'HUN', 'IDN', 'IMN', 'IND', 'IOT', 'IRL', 'IRN', 
          'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JEY', 'JOR', 'JPN', 'KAZ', 
          'KEN', 'KGZ', 'KHM', 'KIR', 'KNA', 'KOR', 'KWT', 'LAO', 'LBN', 
          'LBR', 'LBY', 'LCA', 'LIE', 'LKA', 'LSO', 'LTU', 'LUX', 'LVA', 
          'MAC', 'MAF', 'MAR', 'MCO', 'MDA', 'MDG', 'MDV', 'MEX', 'MHL', 
          'MKD', 'MLI', 'MLT', 'MMR', 'MNE', 'MNG', 'MNP', 'MOZ', 'MRT', 
          'MSR', 'MTQ', 'MUS', 'MWI', 'MYS', 'MYT', 'NAM', 'NCL', 'NER', 
          'NFK', 'NGA', 'NIC', 'NIU', 'NLD', 'NOR', 'NPL', 'NRU', 'NZL', 
          'OMN', 'PAK', 'PAN', 'PCN', 'PER', 'PHL', 'PLW', 'PNG', 'POL', 
          'PRI', 'PRK', 'PRT', 'PRY', 'PSE', 'PYF', 'QAT', 'REU', 'ROU', 
          'RUS', 'RWA', 'SAU', 'SDN', 'SEN', 'SGP', 'SGS', 'SHN', 'SJM', 
          'SLB', 'SLE', 'SLV', 'SMR', 'SOM', 'SPM', 'SRB', 'SSD', 'STP', 
          'SUR', 'SVK', 'SVN', 'SWE', 'SWZ', 'SXM', 'SYC', 'SYR', 'TCA', 
          'TCD', 'TGO', 'THA', 'TJK', 'TKL', 'TKM', 'TLS', 'TON', 'TTO', 
          'TUN', 'TUR', 'TUV', 'TWN', 'TZA', 'UGA', 'UKR', 'UMI', 'URY', 
          'USA', 'UZB', 'VAT', 'VCT', 'VEN', 'VGB', 'VIR', 'VNM', 'VUT', 
          'WLF', 'WSM', 'XDI', 'XIB', 'XIK', 'XKX', 'XMA', 'XSI', 'YEM', 
          'ZAF', 'ZMB', 'ZWE'
      )
      
      cat("Using fallback list of", length(fallback_countries), "countries\n")
      return(fallback_countries)
    })
  })
}