# =========================
# render_and_upload.R  (Service Account → Shared drive, ID-only)
# =========================
suppressPackageStartupMessages({
  library(quarto)
  library(googledrive)
  library(jsonlite)
})

# --- Auth with Service Account ---
SA_PATH <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = "service-account.json")
if (!file.exists(SA_PATH)) stop(paste0("Service account key not found at: ", SA_PATH))
.sa <- tryCatch(jsonlite::fromJSON(SA_PATH), error = function(e) NULL)
if (is.null(.sa) || .sa$type != "service_account") stop("GOOGLE_APPLICATION_CREDENTIALS is not a service-account key.")

# Drive scopes: 'drive' + 'drive.file' for Shared drive uploads
drive_auth(path = SA_PATH, scopes = c(
  "https://www.googleapis.com/auth/drive",
  "https://www.googleapis.com/auth/drive.file"
))

# --- Target folder MUST be in a Shared drive ---
folder_id <- Sys.getenv("GOOGLE_FOLDER_ID")
if (!nzchar(folder_id)) {
  stop("GOOGLE_FOLDER_ID is empty. Put the Shared drive folder ID in .Renviron.")
}

# Verify the SA can see exactly that folder
fld <- tryCatch(drive_get(as_id(folder_id)), error = function(e) NULL)
if (is.null(fld) || nrow(fld) != 1) {
  stop("Cannot access folder by GOOGLE_FOLDER_ID. Is the SA a member of the Shared drive?")
}

parent <- as_id(folder_id)  # <- CRITICAL: use the ID directly (no name lookups)

# --- Render the report ---
quarto::quarto_render("report.qmd")  # makes report.html (and report.pdf if LaTeX available)

# --- Upload using the folder ID directly ---
ts <- format(Sys.time(), "%Y-%m-%d_%H%M")
html_name <- paste0("macro_report_", ts, ".html")
pdf_name  <- paste0("macro_report_", ts, ".pdf")

drive_upload(media = "report.html", path = parent, name = html_name, overwrite = TRUE)

if (file.exists("report.pdf")) {
  drive_upload(media = "report.pdf", path = parent, name = pdf_name, overwrite = TRUE)
}

cat("Report uploaded to Shared drive folder ID:", folder_id, "\n")
