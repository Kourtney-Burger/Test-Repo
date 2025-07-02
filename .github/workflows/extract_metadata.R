# Install and load packages
# install.packages(c("yaml", "dplyr", "readr", "googlesheets4"))
library(yaml)
library(dplyr)
library(readr)
library(googlesheets4) # If you're using Google Sheets

# --- Configuration ---
markdown_files_path <- "projects" # Adjust to your markdown file location
spreadsheet_id <- "1gSVkSnhTiI1V6A9YDyAOo4-gHUTDp4REIw0qhP2NplI" # Replace with your Google Sheet ID
sheet_name <- "Sheet1" # The name of the sheet within your spreadsheet

# --- Function to extract metadata from a single Markdown file ---
extract_md_metadata <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  
  # Find YAML front matter delimiters
  front_matter_start <- which(content == "---")[1]
  front_matter_end <- which(content == "---")[2]
  
  if (!is.na(front_matter_start) && !is.na(front_matter_end) && front_matter_end > front_matter_start) {
    metadata_lines <- content[(front_matter_start + 1):(front_matter_end - 1)]
    metadata <- yaml.load(paste(metadata_lines, collapse = "\n"))
    
    # Add file path or name for identification
    metadata$file_name <- basename(file_path)
    return(as.data.frame(metadata))
  } else {
    warning(paste("No YAML front matter found in:", file_path))
    return(NULL)
  }
}

# --- Main script execution ---

# Get all markdown files
md_files <- list.files(markdown_files_path, pattern = "\\.md$", recursive = TRUE, full.names = TRUE)

if (length(md_files) > 0) {
  # Extract metadata from all files
  all_metadata <- lapply(md_files, extract_md_metadata)
  
  # Filter out NULL results (files without metadata) and combine
  all_metadata_df <- bind_rows(all_metadata[!sapply(all_metadata, is.null)])
  
  if (nrow(all_metadata_df) > 0) {
    # Reorder columns to ensure consistency (optional but good practice)
    # Define your expected column order here
    expected_cols <- c("project_name", "status", "start_date", "end_date", "team_lead", "budget", "file_name")
    
    # Add missing columns with NA and reorder
    for (col in expected_cols) {
      if (!(col %in% colnames(all_metadata_df))) {
        all_metadata_df[[col]] <- NA
      }
    }
    all_metadata_df <- all_metadata_df %>% select(all_of(expected_cols))
    
    # --- Write to Spreadsheet ---
    # For Google Sheets:
    # Authenticate (if not already done by the GitHub Action environment)
    # googlesheets4::sheets_auth(token = gargle::gargle_oauth_sitrep()$gargle_token) # Or your chosen auth method
    
    # Append data to the Google Sheet
    # The first time, you might need to create the sheet manually or use
    # sheets_write(data = all_metadata_df, ss = spreadsheet_id, sheet = sheet_name)
    # To append, use sheets_append:
    
    # First, read existing data to avoid duplicates for existing files
    existing_data <- tryCatch({
      read_sheet(ss = spreadsheet_id, sheet = sheet_name)
    }, error = function(e) {
      message("Could not read existing sheet, assuming it's new or empty.")
      data.frame() # Return empty data frame if sheet doesn't exist or can't be read
    })
    
    # Identify new or updated files
    if (nrow(existing_data) > 0) {
      # Files present in both new and existing data (updated)
      updated_files <- all_metadata_df %>% 
        semi_join(existing_data, by = "file_name")
      
      # Files only in new data (newly created)
      new_files <- all_metadata_df %>%
        anti_join(existing_data, by = "file_name")
      
      # Remove updated rows from existing data before appending to avoid duplicates
      data_to_write <- existing_data %>%
        anti_join(updated_files, by = "file_name") %>%
        bind_rows(all_metadata_df) # Add all current data
      
      # Overwrite the entire sheet to ensure consistency
      sheets_write(data = data_to_write, ss = spreadsheet_id, sheet = sheet_name)
      message("Spreadsheet updated with new and modified project metadata.")
      
    } else {
      # If no existing data, just write all current data
      googlesheets4::sheet_write(data = all_metadata_df, ss = spreadsheet_id, sheet = sheet_name)
      message("Spreadsheet created/updated with all project metadata.")
    }
    
    # For local CSV output (alternative to Google Sheets)
    # write_csv(all_metadata_df, "project_status.csv")
    # message("Metadata extracted and saved to project_status.csv")
    
  } else {
    message("No valid metadata found in markdown files.")
  }
} else {
  message("No markdown files found in the specified path.")
}
