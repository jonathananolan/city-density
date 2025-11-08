library(glue)

# Load required functions
source("R/functions/get_worldpop_global_data_ftp.R")

# Download a single dataset file for a country (or find if it already exists locally)
download_dataset_file <- function(
  country_iso3,
  dataset_type,
  year,
  version, 
  resolution,
  output_dir = "data/"
) {
  
  # Get dataset configuration
  dataset_configs <- list(
    population = list(
      source_type = "ftp",
      base_path = "ftp://ftp.worldpop.org/GIS/Population/Global_2015_2030",
      folder_structure = "{version}/{year}/{country}/v1/{resolution}m/constrained/",
      filename_pattern = "{country}_pop_{year}_CN_{resolution}m_{version}_v1.tif",
      subdirectory = "",
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
  
  config <- dataset_configs[[dataset_type]]
  if (is.null(config)) {
    stop("Unknown dataset_type: ", dataset_type)
  }
  
  # Build local file path
  country_lower <- tolower(country_iso3)
  filename <- glue(config$filename_pattern,
                   country = country_lower,
                   year = year,
                   version = version,
                   resolution = resolution)
  
  local_file <- file.path(output_dir, dataset_type, filename)
  
  # Check if file already exists locally
  if (file.exists(local_file) && file.size(local_file) > 1000) {
    cat("  ✓ File exists locally:", basename(local_file), "\n")
    return(local_file)
  }
  
  # File doesn't exist - download it
  cat("  → File not found locally - downloading\n")
  
  if (config$source_type == "ftp") {
    # Build FTP URL
    remote_path <- glue(config$folder_structure,
                       country = country_iso3,
                       year = year,
                       version = version,
                       resolution = resolution)
    
    ftp_url <- file.path(config$base_path, remote_path, filename)
    
    # Create output directory
    if (!dir.exists(dirname(local_file))) {
      dir.create(dirname(local_file), recursive = TRUE)
    }
    
    cat("  → Downloading from FTP:", ftp_url, "\n")
    
    # Download using curl
    download_success <- tryCatch({
      exit_code <- system2("curl", args = c(
        "--ftp-pasv",
        "--retry", "2", 
        "--retry-delay", "3",
        "--max-time", "300",
        "--connect-timeout", "30",
        "-L",
        "--create-dirs",
        "-o", shQuote(local_file),
        shQuote(ftp_url)
      ), stdout = FALSE, stderr = FALSE)
      
      exit_code == 0 && file.exists(local_file) && file.size(local_file) > 1000
    }, error = function(e) {
      FALSE
    })
    
    if (!download_success) {
      stop("Failed to download ", country_iso3, " ", dataset_type, " data")
    }
    
    file_size_mb <- round(file.size(local_file) / 1024 / 1024, 1)
    cat("  ✓ Downloaded", country_iso3, ":", file_size_mb, "MB\n")
    
    return(local_file)
    
  } else {
    stop("Unsupported source_type: ", config$source_type)
  }
}