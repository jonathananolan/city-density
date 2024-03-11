upload_object <- function(file_location){
  
  put_object(file = file_location, 
             bucket = "city-density",multipart = T,show_progress = T,
             headers = list("Content-Type" = "text/html"),verbose = T)
}

#This function cuts off extremes of the viridis palette so it's easier to see where most of the data is. 
viridis_palette_limiter <- function(data,outlier_number){  # Sort data to find the cutoff points
  sorted_values <- sort(data)
  min_cutoff    <- min(sorted_values)
  high_cutoff   <- sorted_values[length(sorted_values) - outlier_number]  # 10th highest value
  highest_value <- max(sorted_values)
  first_distance <- high_cutoff-min_cutoff  
  second_distance <- highest_value - high_cutoff
  #The first distance is going to have about 50 colours... so what is the second?
  second_number_of_colours <- round(50*second_distance/first_distance)
  ## Make vector of colors for values smaller than 0 (20 colors)
  rc1 <- viridis::viridis(50, option = "D")
  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <- rep(viridis::viridis(1, begin = 1, option = "D"),second_number_of_colours)
  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)
  pal <- colorNumeric(palette = rampcols, domain = data)
  return(pal)
}

Sys.setenv("AWS_DEFAULT_REGION" = "us-east-1")
