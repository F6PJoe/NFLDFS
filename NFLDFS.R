# Clear console and environment
cat("\014")
rm(list = ls())

packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr",
  "jsonlite", "magrittr", "googlesheets4", "googledrive",
  "lubridate", "base64enc"
)
invisible(lapply(packages, library, character.only = TRUE))

json_key <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)

gs_url <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8"

get_processed_slate <- function(api_url, label) {
  api_key <- paste0("ApiKey ", Sys.getenv("BCDFS_API_KEY"))

  response <- tryCatch(
    GET(api_url, add_headers(Authorization = api_key, `Content-Type` = "application/json")),
    error = function(e) { message(label, " API request failed: ", e$message); return(NULL) }
  )
  if (is.null(response)) return(NULL)

  # Parse as text first then fromJSON to preserve array structure
  raw <- content(response, "text", encoding = "UTF-8")
  data <- tryCatch(
    fromJSON(raw, simplifyVector = FALSE),
    error = function(e) { message(label, " failed to parse response: ", e$message); return(NULL) }
  )
  if (is.null(data)) return(NULL)

  slates <- data$slates
  if (is.null(slates) || length(slates) == 0) {
    message(label, " — no slates available yet. Skipping.")
    return(NULL)
  }

  # Print all slate names for diagnostics
  slate_names <- sapply(slates, function(s) s$slate)
  message(label, " — slates available: ", paste(slate_names, collapse = " | "))

  # Find MAIN slate
  slate_index <- which(grepl("MAIN", slate_names, ignore.case = TRUE))[1]

  # Fallback to ALL DAY / ALL
  if (is.na(slate_index)) {
    slate_index <- which(grepl("ALL DAY|ALL", slate_names, ignore.case = TRUE))[1]
  }

  # Fallback to largest slate
  if (is.na(slate_index)) {
    player_counts <- sapply(slates, function(s) length(s$info))
    slate_index <- which.max(player_counts)
    message(label, " — no MAIN/ALL DAY slate found. Using largest: '",
            slate_names[slate_index], "' with ", player_counts[slate_index], " players.")
  }

  selected <- slates[[slate_index]]
  message(label, " — selected slate: '", selected$slate, "' updated: ", selected$updated)

  info <- selected$info
  if (is.null(info) || length(info) == 0) {
    message(label, " — slate exists but contains no player data. Skipping.")
    return(NULL)
  }

  # Convert list of players to data frame
  df <- bind_rows(lapply(info, as.data.frame, stringsAsFactors = FALSE))

  # Rename using actual API field names from docs
  df <- dplyr::rename(df,
    Opp    = opponent,
    Player = name,
    Pos    = position,
    Team   = team,
    Proj   = projection,
    Salary = salary,
    Value  = value
  )

  # Optional fields that may not always exist
  if ("site_id" %in% names(df))  df <- dplyr::rename(df, ID   = site_id)
  if ("beta_proj" %in% names(df)) df <- dplyr::rename(df, Beta = beta_proj)

  df$Proj   <- round(as.numeric(df$Proj), 2)
  df$Salary <- as.numeric(df$Salary)
  df$Value  <- round(as.numeric(df$Value), 1)
  df <- df[!is.na(df$Proj) & df$Proj > 0, ]

  if (nrow(df) == 0) {
    message(label, " — slate found but no players have valid projections yet. Skipping.")
    return(NULL)
  }

  # Handle multi-position players
  df$OptPos <- df$Pos
  dualPos   <- grepl("/", df$Pos)
  df$Pos2   <- ""
  df$Pos2[dualPos] <- sub("/", "", str_extract(df$Pos[dualPos], "/[A-Z0-9]{1,2}$"))
  df$Pos[dualPos]  <- sub("/", "", str_extract(df$Pos[dualPos], "^[A-Z0-9]{1,2}/"))
  df$Pos1 <- df$Pos
  df$Pos  <- df$OptPos
  df <- arrange(df, desc(Proj))

  message(label, " — slate loaded with ", nrow(df), " players.")
  return(df)
}

write_placeholder <- function(sheet_name, site_label) {
  range_clear(ss = gs_url, sheet = sheet_name, range = "A2:Z1000")
  range_write(
    ss        = gs_url,
    data      = data.frame(Message = paste0(site_label, " Projections for today's games will be coming soon")),
    sheet     = sheet_name,
    range     = "A2",
    col_names = FALSE
  )
  message(site_label, " — placeholder message written to ", sheet_name, ".")
}

# --- FanDuel ---
fd <- get_processed_slate("https://bluecollardfs.com/api/nfl_fanduel", "FanDuel")
if (!is.null(fd)) {
  sheet_write(fd[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "FD NFL DFS", ss = gs_url)
  message("FanDuel data written to Google Sheets.")
} else {
  write_placeholder("FD NFL DFS", "FanDuel")
}

# --- DraftKings ---
dk <- get_processed_slate("https://bluecollardfs.com/api/nfl_draftkings", "DraftKings")
if (!is.null(dk)) {
  sheet_write(dk[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "DK NFL DFS", ss = gs_url)
  message("DraftKings data written to Google Sheets.")
} else {
  write_placeholder("DK NFL DFS", "DraftKings")
}

# --- Timestamp ---
update_time    <- with_tz(Sys.time(), "America/New_York")
formatted_date <- format(update_time, "%B %d, %Y")
formatted_time <- format(update_time, "%I:%M %p ET")
range_write(ss = gs_url, data = data.frame(Date = formatted_date), sheet = "NFL Update Time", range = "A2", col_names = FALSE)
range_write(ss = gs_url, data = data.frame(Time = formatted_time), sheet = "NFL Update Time", range = "B2", col_names = FALSE)
message("Timestamp updated: ", formatted_date, " ", formatted_time)

if (!interactive()) quit(status = 0)
