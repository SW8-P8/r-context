library(httr)


write_df_csv <- function(df, location) {
  write.csv(cleaned_valid, location, row.names = FALSE)
}


upload_plot <- function(plot_obj, save_dir = "plots", filename = "plot.png", server_url = "http://localhost:3000/upload", width = 6, height = 5, dpi = 300) {
  # Create the save directory if it doesn't exist
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Full path to the local file
  file_path <- file.path(save_dir, filename)
  
  # Save the ggplot object to file
  ggsave(
    filename = file_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  
  # Upload the file
  response <- POST(
    url = server_url,
    body = list(plot = upload_file(file_path)),
    encode = "multipart"
  )
  
  # Handle response
  if (response$status_code == 200) {
    return(content(response)$url)
  } else {
    stop("Upload failed: ", content(response, as = "text"))
  }
}

