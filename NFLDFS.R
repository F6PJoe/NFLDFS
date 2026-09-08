# Clear console and environment
cat("\014")
rm(list = ls())

# Required packages
packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr",
  "jsonlite", "magrittr", "googlesheets4", "googledrive",
  "lubridate", "base64enc"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Decode Google Sheets credentials from env var and authenticate
json_key <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)

gs_url <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8/"

# Read last known update time from Google Sheet
last_updated_sheet <- tryCatch({
  val <- range_read(ss = gs_url, sheet = "NFL Update Time", range = "B2", col_names = FALSE)
  as.character(val[[1]][1])
}, error = function(e) {
  message("Could not read last update time from sheet: ", e$message)
  NULL
})

# Helper function to fetch and process slate data
get_processed_slate <- function(api_url) {
  response <- GET(api_url, add_headers(
    Authorization = "FantasySixPack",
    `Content-Type` = "application/json"
  ))
  data <- content(response, "parsed", simplifyVector = TRUE)
  slates <- data$slates

  current_updated <- slates$updated[1]

  text_cols <- names(slates)[sapply(slates, is.character)]

  slate_index <- NA
  for (col in text_cols) {
    idx <- which(grepl("MAIN", slates[[col]], ignore.case = TRUE))[1]
    if (!is.na(idx)) { slate_index <- idx; break }
  }

  if (is.na(slate_index)) {
    for (col in text_cols) {
      idx <- which(grepl("ALL DAY|ALL", slates[[col]], ignore.case = TRUE))[1]
      if (!is.na(idx)) { slate_index <- idx; break }
    }
  }

  if (is.na(slate_index)) {
    player_counts <- sapply(seq_along(data$slates$info), function(i) {
      info <- data$slates$info[[i]]
      if (is.data.frame(info)) nrow(info) else 0
    })
    slate_index <- which.max(player_counts)
    message("No MAIN/ALL DAY slate found. Using slate index ", slate_index,
            " with ", player_counts[slate_index], " players.")
  }

  # Guard: no slate data available yet
  raw <- data$slates$info[[slate_index]]
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    message("No slate data available yet for ", api_url, ". Skipping.")
    return(list(df = data.frame(), updated = current_updated))
  }

  df <- raw %>% rename(
    Opp    = opponent,
    Player = name,
    ID     = site_id,
    Pos    = position,
    Team   = team,
    Proj   = projection,
    Salary = salary,
    Beta   = beta_proj,
    Value  = value
  )

  df$Proj   <- round(as.numeric(df$Proj), 2)
  df$Salary <- as.numeric(df$Salary)
  df$Value  <- round(as.numeric(df$Value), 1)
  df <- df[!is.na(df$Proj) & df$Proj > 0, ]

  if (nrow(df) == 0) {
    warning("Slate index ", slate_index, " (", slates$slate[slate_index], ") has no players with valid projections yet. Returning empty data frame.")
    return(list(df = data.frame(), updated = current_updated))
  }

  # Handle multi-position players
  df$OptPos <- df$Pos
  dualPos <- grepl("/", df$Pos)
  df$Pos2 <- ""
  df$Pos2[dualPos] <- sub("/", "", str_extract(df$Pos[dualPos], "/[A-Z0-9]{1,2}$"))
  df$Pos[dualPos]  <- sub("/", "", str_extract(df$Pos[dualPos], "^[A-Z0-9]{1,2}/"))
  df$Pos1 <- df$Pos
  df$Pos  <- df$OptPos

  list(df = arrange(df, desc(Proj)), updated = current_updated)
}

# Fetch FD and DK data
fd_result <- get_processed_slate("https://bluecollardfs.com/api/nfl_fanduel")
dk_result <- get_processed_slate("https://bluecollardfs.com/api/nfl_draftkings")

fd <- fd_result$df
dk <- dk_result$df

# Use FD updated time as the source of truth
api_updated <- fd_result$updated
message("API last updated: ", api_updated)
message("Sheet last updated: ", last_updated_sheet)

if (is.null(api_updated) || is.na(api_updated)) {
  message("No API update time available. Skipping Google Sheets update.")
} else if (!is.null(last_updated_sheet) && api_updated == last_updated_sheet) {
  message("Data unchanged since last run. Skipping Google Sheets update.")
} else {
  message("New data detected. Writing to Google Sheets.")

  if (nrow(fd) > 0) {
    sheet_write(fd[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "FD NFL DFS", ss = gs_url)
  }
  if (nrow(dk) > 0) {
    sheet_write(dk[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "DK NFL DFS", ss = gs_url)
  }

  if (nrow(fd) > 0 || nrow(dk) > 0) {
    update_time    <- with_tz(Sys.time(), "America/New_York")
    formatted_date <- format(update_time, "%B %d, %Y")
    formatted_time <- format(update_time, "%I:%M %p ET")

    range_write(ss = gs_url, data = data.frame(Date = formatted_date), sheet = "NFL Update Time", range = "A2", col_names = FALSE)
    range_write(ss = gs_url, data = data.frame(Time = formatted_time), sheet = "NFL Update Time", range = "B2", col_names = FALSE)
  }
}
