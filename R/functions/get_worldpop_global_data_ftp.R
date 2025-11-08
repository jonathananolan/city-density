# Get list of countries and dataset information from various global data sources
# Supports both FTP (WorldPop) and local file systems

library(glue)

get_worldpop_global_data_ftp <- function(
  year = 2025, 
  version = "R2025A", 
  resolution = 100,
  dataset_type = "population",  # "population", "water", etc.
  dataset_config = NULL        # Custom dataset configuration
) {
  
  # Define dataset configurations
  dataset_configs <- list(
    population = list(
      source_type = "ftp",
      base_path = "ftp://ftp.worldpop.org/GIS/Population/Global_2015_2030",
      folder_structure = "{version}/{year}/",
      filename_pattern = "{country}_pop_{year}_CN_{resolution}m_{version}_v1.tif",
      subdirectory = "v1/{resolution}m/constrained/",
      description = "WorldPop constrained population estimates"
    ),
    water = list(
      source_type = "ftp",
      base_path = "ftp://ftp.worldpop.org/GIS/Covariates/Global_2015_2030",
      folder_structure = "{country}/Inland_water/esa_worldcover/v1/pct/",
      filename_pattern = "{country}_inland_water_pct_{resolution}m_v1.tif",
      subdirectory = "",
      description = "ESA WorldCover inland water percentage"
    )
  )
  
  # Use provided config or default
  config <- dataset_config %||% dataset_configs[[dataset_type]]
  
  if (is.null(config)) {
    stop("Unknown dataset_type: ", dataset_type, ". Available types: ", 
         paste(names(dataset_configs), collapse = ", "))
  }
  
  cat("Getting country list for dataset:", dataset_type, "\n")
  cat("Description:", config$description, "\n")
  
  if (config$source_type == "ftp") {
    return(get_countries_from_ftp(config, year, version, resolution))
  } else if (config$source_type == "local") {
    return(get_countries_from_local(config, year, version, resolution))
  } else {
    stop("Unsupported source_type: ", config$source_type)
  }
}

# Get countries from FTP server (WorldPop style)
get_countries_from_ftp <- function(config, year, version, resolution) {
  
  # Build FTP directory path
  ftp_base <- glue::glue(config$base_path)
  
  # Handle different folder structures
  if (grepl("\\{country\\}", config$folder_structure)) {
    # For datasets where country is in the path (like water), list base directory
    ftp_url <- ftp_base
  } else {
    # For datasets where version/year come first (like population), build full path
    folder_path <- glue::glue(config$folder_structure, 
                             version = version, 
                             year = year,
                             resolution = resolution)
    ftp_url <- file.path(ftp_base, folder_path)
  }
  
  cat("Getting recursive file listing from FTP server:", ftp_url, "\n")
  cat("Looking for", resolution, "m files...\n")
  
  # Method 1: Get list of country directories
  tryCatch({
    result <- system2("curl", args = c(
      "--list-only",
      "--ftp-pasv",
      shQuote(ftp_url)
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
        paste0("'open ", ftp_url, "; ls; quit'")
      ), stdout = TRUE, stderr = FALSE)
      
      # Parse lftp output
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
      return(get_fallback_countries())
    })
  })
}

# Get countries from local file system
get_countries_from_local <- function(config, year, version, resolution) {
  
  base_path <- config$base_path
  
  if (!dir.exists(base_path)) {
    stop("Base path does not exist: ", base_path)
  }
  
  cat("Scanning local directory:", base_path, "\n")
  
  # Look for country directories (3-letter codes)
  all_dirs <- list.dirs(base_path, recursive = FALSE, full.names = FALSE)
  country_dirs <- all_dirs[nchar(all_dirs) == 3 & grepl("^[A-Z]{3}$", all_dirs)]
  
  # Verify that countries have the expected data structure
  valid_countries <- c()
  
  for (country in country_dirs) {
    # Build expected path using folder_structure
    country_path <- file.path(
      base_path, 
      glue::glue(config$folder_structure, 
                country = country, 
                year = year, 
                version = version,
                resolution = resolution)
    )
    
    # Build expected filename
    expected_file <- file.path(
      country_path,
      glue::glue(config$filename_pattern,
                country = tolower(country),
                year = year,
                version = version, 
                resolution = resolution)
    )
    
    if (file.exists(expected_file)) {
      valid_countries <- c(valid_countries, country)
    }
  }
  
  if (length(valid_countries) > 0) {
    cat("Found", length(valid_countries), "countries with valid data files\n")
    cat("First 10:", paste(head(valid_countries, 10), collapse = ", "), "\n")
    return(sort(valid_countries))
  } else {
    stop("No countries found with valid data files in expected structure")
  }
}

# Fallback country list for when FTP fails
get_fallback_countries <- function() {
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
}