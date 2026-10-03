# Script to update ipc/ipc.csv from the latest available IPC Excel file
# The Excel files are named sh_ipc_%m_%y.xls (e.g., sh_ipc_02_26.xls)

library(readxl)
suppressPackageStartupMessages(library(dplyr))

# Set working directory to the script location
# Works both in RStudio and when running from terminal
script_dir <- tryCatch({
  dirname(rstudioapi::getSourceEditorContext()$path)
}, error = function(e) {
  # When running from command line, use the script's directory
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    getwd()
  }
})

if (is.null(script_dir) || script_dir == "" || script_dir == ".") {
  script_dir <- getwd()
}
setwd(script_dir)

# ============================================================================
# PART 0: Download the latest source files from INDEC and the BCRA if available
# ============================================================================
# INDEC publishes monthly IPC files as sh_ipc_MM_YY.xls (MM = publication
# month, YY = year). The file published in month M contains the official IPC
# through month M-1. The BCRA publishes the REM tables as
# relevamiento-expectativas-mercado-tablas-YYYY-MM.xlsx (data month YYYY-MM).
# Both servers return an HTML page (not a 404) for files that do not exist yet,
# so we validate the file signature before keeping a download.

add_months <- function(date, n) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m")) + n
  while (m <= 0) { m <- m + 12; y <- y - 1 }
  while (m > 12) { m <- m - 12; y <- y + 1 }
  as.Date(sprintf("%04d-%02d-01", y, m))
}

download_latest_ipc <- function(ipc_dir = "ipc", months_back = 12) {
  base_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/"
  xls_magic <- as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1))

  parse_pub_date <- function(filename) {
    m <- regmatches(basename(filename),
                    regexec("sh_ipc_(\\d{2})_(\\d{2})\\.xls", basename(filename)))[[1]]
    if (length(m) == 3) {
      return(as.Date(sprintf("%04d-%02d-01", as.integer(m[3]) + 2000, as.integer(m[2]))))
    }
    as.Date(NA)
  }

  local_files <- list.files(ipc_dir, pattern = "^sh_ipc_\\d{2}_\\d{2}\\.xls$", full.names = TRUE)
  local_dates <- as.Date(sapply(local_files, parse_pub_date), origin = "1970-01-01")
  latest_local <- suppressWarnings(max(local_dates, na.rm = TRUE))
  if (!is.finite(latest_local)) latest_local <- as.Date("1900-01-01")

  this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  for (i in 0:months_back) {
    pub_date <- add_months(this_month, -i)
    if (pub_date <= latest_local) break

    fname <- sprintf("sh_ipc_%02d_%02d.xls",
                     as.integer(format(pub_date, "%m")),
                     as.integer(format(pub_date, "%y")))
    url <- paste0(base_url, fname)
    dest <- file.path(ipc_dir, fname)
    tmp <- tempfile(fileext = ".xls")

    valid <- tryCatch({
      suppressWarnings(download.file(url, tmp, mode = "wb", quiet = TRUE))
      file.exists(tmp) && file.size(tmp) > 100000 &&
        identical(readBin(tmp, what = "raw", n = 8), xls_magic)
    }, error = function(e) FALSE)

    if (valid) {
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
      cat("Downloaded latest IPC file from INDEC:", fname, "\n")
      return(dest)
    }
    unlink(tmp)
  }

  cat("No newer IPC file available from INDEC (latest local file:",
      format(latest_local, "%Y-%m"), ").\n")
  NULL
}

download_latest_rem <- function(ipc_dir = "ipc", months_back = 12) {
  base_url <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/informes/relevamiento-expectativas-mercado-tablas-"
  xlsx_magic <- as.raw(c(0x50, 0x4B, 0x03, 0x04))
  spanish_months <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
                      jul = 7, ago = 8, sep = 9, oct = 10, nov = 11, dic = 12)

  parse_rem_date <- function(filename) {
    b <- basename(filename)
    # New BCRA naming: relevamiento-expectativas-mercado-tablas-YYYY-MM.xlsx
    m_new <- regmatches(b, regexec("relevamiento-expectativas-mercado-tablas-(\\d{4})-(\\d{2})\\.xlsx", b))[[1]]
    if (length(m_new) == 3) {
      return(as.Date(sprintf("%04d-%02d-01", as.integer(m_new[2]), as.integer(m_new[3]))))
    }
    # Legacy naming: tablas-relevamiento-expectativas-mercado-MMM-YYYY.xlsx
    m_old <- regmatches(b, regexec("tablas-relevamiento-expectativas-mercado-([a-z]{3})-(\\d{4})\\.xlsx", b))[[1]]
    if (length(m_old) == 3) {
      return(as.Date(sprintf("%04d-%02d-01", as.integer(m_old[3]), spanish_months[m_old[2]])))
    }
    as.Date(NA)
  }

  local_files <- list.files(ipc_dir, pattern = "relevamiento-expectativas-mercado.*\\.xlsx$", full.names = TRUE)
  local_dates <- as.Date(sapply(local_files, parse_rem_date), origin = "1970-01-01")
  latest_local <- suppressWarnings(max(local_dates, na.rm = TRUE))
  if (!is.finite(latest_local)) latest_local <- as.Date("1900-01-01")

  this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  for (i in 0:months_back) {
    d <- add_months(this_month, -i)
    if (d <= latest_local) break

    fname <- sprintf("relevamiento-expectativas-mercado-tablas-%04d-%02d.xlsx",
                     as.integer(format(d, "%Y")), as.integer(format(d, "%m")))
    url <- paste0(base_url, sprintf("%04d-%02d.xlsx",
                                    as.integer(format(d, "%Y")), as.integer(format(d, "%m"))))
    dest <- file.path(ipc_dir, fname)
    tmp <- tempfile(fileext = ".xlsx")

    valid <- tryCatch({
      suppressWarnings(download.file(url, tmp, mode = "wb", quiet = TRUE))
      file.exists(tmp) && file.size(tmp) > 10000 &&
        identical(readBin(tmp, what = "raw", n = 4), xlsx_magic)
    }, error = function(e) FALSE)

    if (valid) {
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
      cat("Downloaded latest REM tables from BCRA:", fname, "\n")
      return(dest)
    }
    unlink(tmp)
  }

  cat("No newer REM tables available from BCRA (latest local file:",
      format(latest_local, "%Y-%m"), ").\n")
  NULL
}

download_latest_ipc("ipc")
download_latest_rem("ipc")

# Find the latest sh_ipc_*.xls file in the ipc folder
ipc_files <- list.files("ipc", pattern = "^sh_ipc_\\d{2}_\\d{2}\\.xls$", full.names = TRUE)

if (length(ipc_files) == 0) {
  stop("No IPC Excel files found in ipc/ folder")
}

# Parse dates from filenames and find the latest
parse_file_date <- function(filename) {
  # Extract month and year from filename (sh_ipc_MM_YY.xls)
  matches <- regmatches(basename(filename), regexec("sh_ipc_(\\d{2})_(\\d{2})\\.xls", basename(filename)))[[1]]
  if (length(matches) == 3) {
    month <- as.integer(matches[2])
    year <- as.integer(matches[3]) + 2000  # Convert YY to 20YY
    return(as.Date(sprintf("%04d-%02d-01", year, month)))
  }
  return(NA)
}

file_dates <- sapply(ipc_files, parse_file_date)
latest_file <- ipc_files[which.max(file_dates)]

cat("Reading latest IPC file:", latest_file, "\n")

# Read the Excel file
# Sheet: "Variación mensual IPC Nacional"
# Row 6: dates (Excel serial numbers)
# Row 10: IPC monthly values (Nivel general)
df <- suppressWarnings(suppressMessages(read_excel(latest_file, 
                                                   sheet = "Variación mensual IPC Nacional", 
                                                   col_names = FALSE)))

# Extract dates from row 6 (Excel serial dates)
dates_row <- suppressWarnings(as.numeric(df[6, ]))
dates_row <- dates_row[!is.na(dates_row)]
# Convert Excel serials to R dates and normalize to first day of month
dates <- as.Date(dates_row, origin = "1899-12-30")
dates <- as.Date(format(dates, "%Y-%m-01"))

# Extract IPC values from row 10 (Nivel general)
ipc_row <- suppressWarnings(as.numeric(df[10, ]))
ipc_values <- ipc_row[!is.na(ipc_row)]

# Create dataframe with new data
new_data <- data.frame(
  fecha = dates,
  ipc = ipc_values
)

# Read existing ipc.csv
ipc_csv_path <- "ipc/ipc.csv"
existing_data <- read.csv(ipc_csv_path, stringsAsFactors = FALSE)
existing_data$fecha <- as.Date(existing_data$fecha)
# Normalize existing csv dates to first-of-month to match Excel normalization
existing_data$fecha <- as.Date(format(existing_data$fecha, "%Y-%m-01"))

# Find dates that are in new_data but not in existing_data
new_dates <- new_data$fecha[!new_data$fecha %in% existing_data$fecha]

if (length(new_dates) == 0) {
  cat("No new dates to add. ipc.csv is already up to date.\n")
} else {
  cat("Found", length(new_dates), "new date(s) to add:\n")
  print(new_dates)
  
  # Filter new_data to only include new dates
  rows_to_add <- new_data[new_data$fecha %in% new_dates, ]
  
  # Calculate ipc_indice for new rows
  # ipc_indice is calculated as: previous_ipc_indice * (1 + ipc/100)
  last_indice <- tail(existing_data$ipc_indice, 1)
  
  rows_to_add <- rows_to_add %>%
    arrange(fecha) %>%
    mutate(ipc_indice = NA_real_)
  
  for (i in 1:nrow(rows_to_add)) {
    rows_to_add$ipc_indice[i] <- last_indice * (1 + rows_to_add$ipc[i] / 100)
    last_indice <- rows_to_add$ipc_indice[i]
  }
  
  # Append new rows to existing data
  updated_data <- rbind(existing_data, rows_to_add)
  updated_data <- updated_data %>% arrange(fecha)
  
  # Write updated data back to csv
  write.csv(updated_data, ipc_csv_path, row.names = FALSE)
  
  cat("Successfully updated", ipc_csv_path, "\n")
  cat("Added rows:\n")
  print(rows_to_add)
}

# ============================================================================
# PART 2: Update ipc_proy_rem.csv with REM projections
# ============================================================================

cat("\n--- Updating ipc_proy_rem.csv ---\n")

# Re-read ipc.csv (in case it was just updated)
ipc_data <- read.csv(ipc_csv_path, stringsAsFactors = FALSE)
ipc_data$fecha <- as.Date(ipc_data$fecha)
# Normalize to first-of-month so REM comparison uses month-level matching
ipc_data$fecha <- as.Date(format(ipc_data$fecha, "%Y-%m-01"))

# Copy ipc.csv to ipc_proy_rem.csv as the base
ipc_proy_rem_path <- "ipc/ipc_proy_rem.csv"

# Find the latest REM file. Two naming conventions are supported:
#  - New (BCRA):  relevamiento-expectativas-mercado-tablas-YYYY-MM.xlsx
#  - Legacy:      tablas-relevamiento-expectativas-mercado-MMM-YYYY.xlsx
# Month names in Spanish: ene, feb, mar, abr, may, jun, jul, ago, sep, oct, nov, dic
rem_files <- list.files("ipc", pattern = "relevamiento-expectativas-mercado.*\\.xlsx$", full.names = TRUE)

if (length(rem_files) == 0) {
  cat("No REM Excel files found in ipc/ folder. Skipping REM update.\n")
} else {
  # Parse dates from REM filenames (Spanish month names)
  spanish_months <- c("ene" = 1, "feb" = 2, "mar" = 3, "abr" = 4, "may" = 5, "jun" = 6,
                      "jul" = 7, "ago" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dic" = 12)

  parse_rem_date <- function(filename) {
    b <- basename(filename)
    # New BCRA naming: relevamiento-expectativas-mercado-tablas-YYYY-MM.xlsx
    m_new <- regmatches(b, regexec("relevamiento-expectativas-mercado-tablas-(\\d{4})-(\\d{2})\\.xlsx", b))[[1]]
    if (length(m_new) == 3) {
      return(as.Date(sprintf("%04d-%02d-01", as.integer(m_new[2]), as.integer(m_new[3]))))
    }
    # Legacy naming: tablas-relevamiento-expectativas-mercado-MMM-YYYY.xlsx
    m_old <- regmatches(b, regexec("tablas-relevamiento-expectativas-mercado-([a-z]{3})-(\\d{4})\\.xlsx", b))[[1]]
    if (length(m_old) == 3) {
      month <- spanish_months[m_old[2]]
      if (!is.na(month)) {
        return(as.Date(sprintf("%04d-%02d-01", as.integer(m_old[3]), month)))
      }
    }
    return(NA)
  }

  rem_dates <- as.Date(sapply(rem_files, parse_rem_date), origin = "1970-01-01")
  latest_rem_file <- rem_files[which.max(rem_dates)]
  
  cat("Reading latest REM file:", latest_rem_file, "\n")
  
  # Read the REM Excel file
  # First sheet, rows 6-12 contain 7 monthly projections
  # Column A: dates (Excel serial numbers)
  # Column C: IPC median values
  rem_df <- suppressWarnings(suppressMessages(read_excel(latest_rem_file, 
                                                          sheet = 1, 
                                                          col_names = FALSE)))
  
  # Extract 7 monthly values from rows 6-12 (1-indexed in R after reading)
  rem_dates_raw <- suppressWarnings(as.numeric(rem_df[6:12, 1][[1]]))
  rem_ipc_values <- suppressWarnings(as.numeric(rem_df[6:12, 3][[1]]))
  
  # Convert Excel dates to R dates (first day of month)
  rem_dates <- as.Date(rem_dates_raw, origin = "1899-12-30")
  # Normalize to first day of month
  rem_dates <- as.Date(format(rem_dates, "%Y-%m-01"))
  
  # Create REM dataframe
  rem_data <- data.frame(
    fecha = rem_dates,
    ipc = rem_ipc_values
  )
  
  cat("REM projections:\n")
  print(rem_data)
  
  # Find REM dates that are NOT in ipc.csv (keep real IPC values over projections)
  rem_new_dates <- rem_data$fecha[!rem_data$fecha %in% ipc_data$fecha]
  
  if (length(rem_new_dates) == 0) {
    cat("No new REM projections to add (all dates already have real IPC values).\n")
    # Just copy ipc.csv to ipc_proy_rem.csv
    write.csv(ipc_data, ipc_proy_rem_path, row.names = FALSE)
  } else {
    cat("Adding", length(rem_new_dates), "REM projection(s) for dates:\n")
    print(rem_new_dates)
    
    # Filter REM data to only new dates
    rem_rows_to_add <- rem_data[rem_data$fecha %in% rem_new_dates, ]
    
    # Calculate ipc_indice for REM projections
    # Start from the last available ipc_indice in ipc_data
    last_indice <- tail(ipc_data$ipc_indice, 1)
    
    rem_rows_to_add <- rem_rows_to_add %>%
      arrange(fecha) %>%
      mutate(ipc_indice = NA_real_)
    
    for (i in 1:nrow(rem_rows_to_add)) {
      rem_rows_to_add$ipc_indice[i] <- last_indice * (1 + rem_rows_to_add$ipc[i] / 100)
      last_indice <- rem_rows_to_add$ipc_indice[i]
    }
    
    # Combine ipc_data with REM projections
    proy_rem_data <- rbind(ipc_data, rem_rows_to_add)
    proy_rem_data <- proy_rem_data %>% arrange(fecha)
    
    # ========================================================================
    # Extend projections to end of year if REM data doesn't complete the year
    # Use the last available IPC rate as constant for remaining months
    # ========================================================================
    last_proj_date <- max(proy_rem_data$fecha)
    last_proj_month <- as.integer(format(last_proj_date, "%m"))
    last_proj_year <- as.integer(format(last_proj_date, "%Y"))
    
    if (last_proj_month < 12) {
      # Need to extend to December
      months_to_extend <- (last_proj_month + 1):12
      
      # Use the last IPC rate for extension
      last_ipc_rate <- tail(proy_rem_data$ipc, 1)
      last_indice <- tail(proy_rem_data$ipc_indice, 1)
      
      cat("\nExtending projections to December", last_proj_year, "using constant IPC rate of", last_ipc_rate, "%\n")
      
      extension_rows <- data.frame(
        fecha = as.Date(sprintf("%04d-%02d-01", last_proj_year, months_to_extend)),
        ipc = rep(last_ipc_rate, length(months_to_extend)),
        ipc_indice = NA_real_
      )
      
      for (i in 1:nrow(extension_rows)) {
        extension_rows$ipc_indice[i] <- last_indice * (1 + extension_rows$ipc[i] / 100)
        last_indice <- extension_rows$ipc_indice[i]
      }
      
      proy_rem_data <- rbind(proy_rem_data, extension_rows)
      proy_rem_data <- proy_rem_data %>% arrange(fecha)
      
      cat("Added extension rows:\n")
      print(extension_rows)
    }
    
    # Write to ipc_proy_rem.csv
    write.csv(proy_rem_data, ipc_proy_rem_path, row.names = FALSE)
    
    cat("Successfully updated", ipc_proy_rem_path, "\n")
    cat("Added REM projections:\n")
    print(rem_rows_to_add)
  }
}
