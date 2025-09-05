library(terra)
library(dplyr)

# Install wpgpDownloadR if not available
if (!require(wpgpDownloadR, quietly = TRUE)) {
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("wpgp/wpgpDownloadR")
  library(wpgpDownloadR)
}


get_worldpop_files <- function(countries_list, year = 2025, output_dir = "data/worldpop/", resolution = 100, version = "R2025A", dataset_version = "v1") {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Downloading WorldPop Global2", version, year, "data at", resolution, "m resolution...\n")
  cat("Expected download size: 8-15 GB\n")
  cat("This may take 1-2 hours depending on connection speed\n\n")
  
  # FTP base URL pattern from your example
  # ftp://ftp.worldpop.org/GIS//Population/Global_2015_2030/R2025A/2025/ABW/v1/100m/constrained/abw_pop_2025_CN_100m_R2025A_v1.tif
  ftp_base <- "ftp://ftp.worldpop.org/GIS/Population/Global_2015_2030"
  
  # Robust FTP retry function
  retry_ftp_download <- function(url, destfile, max_retries = 3, base_wait = 15) {
    for (attempt in 1:max_retries) {
      wait_time <- base_wait * (1.5 ^ (attempt - 1))  # Exponential backoff
      
      if (attempt > 1) {
        cat("    Retry attempt", attempt, "of", max_retries, "(waiting", round(wait_time, 1), "seconds)...\n")
        Sys.sleep(wait_time)
      }
      
      result <- tryCatch({
        # Use curl for all attempts (simplify)
        exit_code <- system2("curl", args = c(
          "--ftp-pasv",           # Use passive FTP
          "--retry", "2",
          "--retry-delay", "3", 
          "--max-time", "300",    # 5 minute timeout
          "--connect-timeout", "30",
          "-L",                   # Follow redirects
          "--create-dirs",        # Create directories
          "-o", shQuote(destfile),
          shQuote(url)
        ), stdout = FALSE, stderr = FALSE)
        
        cat("    Exit code:", exit_code, "| File exists:", file.exists(destfile), "\n")
        
        # Check if download was successful (exit_code 0) and file exists
        if (exit_code == 0 && file.exists(destfile)) {
          file_size <- file.size(destfile)
          if (file_size > 1000) {  # At least 1KB
            cat("    Download successful, file size:", file_size, "bytes\n")
            return("success")
          } else {
            if (file.exists(destfile)) file.remove(destfile)  # Remove invalid file
            stop("Downloaded file too small (", file_size, " bytes)")
          }
        } else if (!file.exists(destfile)) {
          stop("File not created")
        } else {
          stop("Download failed with exit code ", exit_code)
        }
        
      }, error = function(e) {
        cat("    Attempt", attempt, "failed:", e$message, "\n")
        return("error")
      })
      
      if (result == "success") {
        cat("    Returning TRUE for successful download\n")
        return(TRUE)
      } else {
        cat("    Result was:", result, "\n")
      }
    }
    cat("    All attempts failed, returning FALSE\n")
    return(FALSE)
  }
  
  # Validate input
  if (length(countries_list) == 0) {
    stop("No countries provided")
  }
  
  cat("Found", length(countries_list), "countries to download\n")
  cat("First 10 countries:", paste(head(countries_list, 10), collapse = ", "), "...\n\n")
  
  downloaded_files <- character()
  failed_countries <- character()
  
  # Download each country individually
  for (i in seq_along(countries_list)) {
    country_iso3 <- countries_list[i]
    
    cat(sprintf("Downloading %d/%d: %s...\n", i, length(countries_list), country_iso3))
    
    # Build FTP URL following the pattern:
    # ftp://ftp.worldpop.org/GIS/Population/Global_2015_2030/R2025A/2025/ABW/v1/100m/constrained/abw_pop_2025_CN_100m_R2025A_v1.tif
    
    country_lower <- tolower(country_iso3)
    filename <- paste0(country_lower, "_pop_", year, "_CN_", resolution, "m_", version, "_", dataset_version, ".tif")
    
    ftp_url <- paste0(
      ftp_base, "/", 
      version, "/", 
      year, "/", 
      country_iso3, "/", 
      dataset_version, "/", 
      resolution, "m/constrained/", 
      filename
    )
    
    local_file <- file.path(output_dir, filename)
    
    # Skip if file already exists and is reasonable size
    if (file.exists(local_file) && file.size(local_file) > 1000) {
      cat("  ✓ File already exists:", filename, "\n")
      downloaded_files <- c(downloaded_files, local_file)
      next
    }
    
    cat("  FTP URL:", ftp_url, "\n")
    
    # Attempt download with retry logic
    download_result <- retry_ftp_download(ftp_url, local_file)
    
    # Ensure download_result is always logical
    download_success <- isTRUE(download_result)
    
    if (download_success) {
      file_size_mb <- round(file.size(local_file) / 1024 / 1024, 1)
      cat("  ✓ Downloaded", country_iso3, ":", file_size_mb, "MB\n")
      downloaded_files <- c(downloaded_files, local_file)
    } else {
      cat("  ✗ Failed to download", country_iso3, "after all attempts\n")
      failed_countries <- c(failed_countries, country_iso3)
    }
    
    # Brief pause between downloads
    Sys.sleep(2)
  }
  
  if (length(downloaded_files) == 0) {
    stop("No files were successfully downloaded")
  }
  
  cat("Successfully downloaded", length(downloaded_files), "country files\n")
  
  if (length(failed_countries) > 0) {
    cat("Failed countries:", paste(failed_countries, collapse = ", "), "\n")
  }
  
  cat("Downloaded files are ready for database loading\n")
  cat("Files location:", output_dir, "\n")
  
  # Return list of downloaded files instead of loading into memory
  # (Loading all countries into memory would be too much)
  return(list(
    files = downloaded_files,
    failed_countries = failed_countries,
    success_count = length(downloaded_files),
    total_countries = length(countries_list)
  ))
}

# Alternative function for manual download guidance
get_worldpop_download_info <- function() {
  cat("WorldPop Global Population Data Download Information\n")
  cat("==================================================\n\n")
  
  cat("Manual Download Options:\n")
  cat("1. HDX Humanitarian Data: https://data.humdata.org/organization/worldpop\n")
  cat("2. WorldPop Portal: https://www.portal.worldpop.org/\n") 
  cat("3. WorldPop Hub: https://hub.worldpop.org/\n\n")
  
  cat("For 2025 data, look for:\n")
  cat("- Global population counts (both sexes, all ages)\n")
  cat("- 100m resolution GeoTIFF format\n")
  cat("- WGS84 projection\n")
  cat("- Constrained (to settlements) version preferred\n\n")
  
  cat("Expected file size: 8-15 GB for global 100m resolution\n")
  cat("Consider downloading by continent if global file too large\n")
}