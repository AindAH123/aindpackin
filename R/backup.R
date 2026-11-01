
#' Export the Snippet Library
#'
#' Creates a backup of all saved snippets and folders.
#'
#' @param path Destination path for the backup file.
#' @param file Optional path to the snippet storage file.
#' @return Invisibly returns the backup path.
#' @export
export_snippets <- function(path, file = NULL) {

  if (file.exists(path)) {
    stop("A backup already exists at this location.")
  }

  backup <- list(
    format = "aindpackin",
    version = 1L,
    snippets = list_snippets(file),
    folders = list_folders(file)
  )

  saveRDS(backup, path)

  invisible(path)
}


#' Import a Snippet Library
#'
#' Restores snippets and folders from an aindpackin backup.
#'
#' @param path Path to the backup file.
#' @param file Optional path to the snippet storage file.
#' @param overwrite Allow replacement of existing data.
#' @return Invisibly returns the storage path.
#' @export
import_snippets <- function(path, file = NULL,
                            overwrite = FALSE) {

  if (!file.exists(path)) {
    stop("Backup file not found.")
  }

  backup <- readRDS(path)

  if (!is.list(backup) ||
      !identical(backup$format, "aindpackin") ||
      !identical(backup$version, 1L) ||
      !is.list(backup$snippets) ||
      !is.character(backup$folders)) {
    stop("Invalid aindpackin backup.")
  }

  if (is.null(file)) {
    data_dir <- tools::R_user_dir("aindpackin", "data")
    file <- file.path(data_dir, "snippets.rds")
    folder_file <- file.path(data_dir, "folders.rds")
  } else {
    folder_file <- paste0(file, ".folders.rds")
  }

  if (!overwrite &&
      (file.exists(file) || file.exists(folder_file))) {
    stop("Existing library detected. Use overwrite = TRUE to replace it.")
  }

  dir.create(dirname(file),
             recursive = TRUE,
             showWarnings = FALSE)

  saveRDS(backup$snippets, file)
  saveRDS(backup$folders, folder_file)

  invisible(file)
}
