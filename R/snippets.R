
#' Save a Code Snippet
#'
#' Saves reusable R code under a specified name
#' and folder for future use.
#'
#' @param name Name of the snippet.
#' @param code R code to save as text.
#' @param folder Folder in which to save the snippet.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the saved snippet key.
#'
#' @export

save_snippet <- function(name, code, folder = "General",
                         file = NULL) {

  if (is.null(file)) {
    file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "snippets.rds"
    )
  }

  dir.create(dirname(file),
             recursive = TRUE,
             showWarnings = FALSE)

  if (file.exists(file)) {
    snippets <- readRDS(file)
  } else {
    snippets <- list()
  }

  key <- paste(folder, name, sep = "::")

  if (key %in% names(snippets)) {
    stop("A snippet with this name already exists.")
  }

  snippets[[key]] <- list(
    name = name,
    code = code,
    folder = folder
  )

  saveRDS(snippets, file)

  invisible(key)
}

#' List Saved Code Snippets
#'
#' Retrieves all saved code snippets, including their
#' names, code, and assigned folders.
#'
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return A named list of saved snippets.
#'
#' @export


list_snippets <- function(file = NULL) {

  if (is.null(file)) {
    file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "snippets.rds"
    )
  }

  if (!file.exists(file)) {
    return(list())
  }

  readRDS(file)
}

#' Update a Saved Code Snippet
#'
#' Modifies the code of an existing snippet while
#' preserving its name and assigned folder.
#'
#' @param name Name of the existing snippet.
#' @param code New R code to save as text.
#' @param folder Folder containing the snippet.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the updated snippet key.
#'
#' @export


update_snippet <- function(name, code, folder = "General",
                           file = NULL) {

  snippets <- list_snippets(file)

  key <- paste(folder, name, sep = "::")

  if (!key %in% names(snippets)) {
    stop("Snippet not found.")
  }

  snippets[[key]]$code <- code

  if (is.null(file)) {
    file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "snippets.rds"
    )
  }

  saveRDS(snippets, file)

  invisible(key)
}

#' Delete a Saved Code Snippet
#'
#' Permanently removes a saved code snippet
#' from the specified folder.
#'
#' @param name Name of the snippet to delete.
#' @param folder Folder containing the snippet.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the deleted snippet key.
#'
#' @export

delete_snippet <- function(name, folder = "General",
                           file = NULL) {

  snippets <- list_snippets(file)

  key <- paste(folder, name, sep = "::")

  if (!key %in% names(snippets)) {
    stop("Snippet not found.")
  }

  snippets[[key]] <- NULL

  if (is.null(file)) {
    file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "snippets.rds"
    )
  }

  saveRDS(snippets, file)

  invisible(key)
}

#' List Saved Folders
#'
#' Retrieves all folders used to organize code
#' snippets, including independently created
#' folders that do not contain any snippets.
#'
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return A character vector of folder names.
#'
#' @export

list_folders <- function(file = NULL) {

  snippets <- list_snippets(file)


  folders <- character(0)

  for (snippet in snippets) {

    parts <- strsplit(
      snippet$folder, "/", fixed = TRUE
    )[[1]]

    for (i in seq_along(parts)) {
      path <- paste(parts[1:i], collapse = "/")
      folders <- c(folders, path)
    }
  }


  folder_file <- if (is.null(file)) {
    file.path(
      tools::R_user_dir("aindpackin", "data"),
      "folders.rds"
    )
  } else {
    paste0(file, ".folders.rds")
  }

  saved_folders <- if (file.exists(folder_file)) {
    readRDS(folder_file)
  } else {
    character(0)
  }

  sort(unique(c(folders, saved_folders)))

}

#' Move a Code Snippet Between Folders
#'
#' Moves an existing saved code snippet from one
#' folder to another without changing its code.
#'
#' @param name Name of the snippet to move.
#' @param from_folder Current folder containing the snippet.
#' @param to_folder Destination folder.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the new snippet key.
#'
#' @export

move_snippet <- function(name, from_folder, to_folder,
                         file = NULL) {

  snippets <- list_snippets(file)

  old_key <- paste(from_folder, name, sep = "::")
  new_key <- paste(to_folder, name, sep = "::")

  if (!old_key %in% names(snippets)) {
    stop("Snippet not found.")
  }

  if (old_key == new_key) {
    return(invisible(new_key))
  }

  if (new_key %in% names(snippets)) {
    stop("A snippet with this name already exists.")
  }

  snippets[[new_key]] <- snippets[[old_key]]
  snippets[[new_key]]$folder <- to_folder
  snippets[[old_key]] <- NULL

  if (is.null(file)) {
    file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "snippets.rds"
    )
  }

  saveRDS(snippets, file)

  invisible(new_key)
}

#' Create a New Folder
#'
#' Creates a folder for organizing saved code snippets.
#' Folders can exist even when they contain no snippets.
#'
#' @param name Name of the folder to create.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the created folder name.
#'
#' @export

create_folder <- function(name, file = NULL) {

  if (!is.character(name) ||
      length(name) != 1 ||
      is.na(name) ||
      !nzchar(trimws(name))) {
    stop("Enter a valid folder name.")
  }

  name <- trimws(name)

  if (grepl("/", name, fixed = TRUE)) {
    stop("Folder names cannot contain '/'.")
  }

  if (is.null(file)) {
    folder_file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "folders.rds"
    )
  } else {
    folder_file <- paste0(file, ".folders.rds")
  }

  folders <- if (file.exists(folder_file)) {
    readRDS(folder_file)
  } else {
    character(0)
  }

  existing <- sub("/.*$", "", list_folders(file))

  if (name %in% c(folders, existing)) {
    stop("Folder already exists.")
  }

  dir.create(dirname(folder_file),
             recursive = TRUE,
             showWarnings = FALSE)

  folders <- sort(unique(c(folders, name)))

  saveRDS(folders, folder_file)

  invisible(name)
}

#' Delete an Empty Folder
#'
#' Deletes a saved folder if it contains no snippets.
#' Folders containing snippets cannot be deleted
#' until those snippets are moved or removed.
#'
#' @param name Name of the folder to delete.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the deleted folder name.
#'
#' @export

delete_folder <- function(name, file = NULL) {

  name <- trimws(name)

  if (!nzchar(name)) {
    stop("Enter a folder name.")
  }

  # Find the saved folders file
  if (is.null(file)) {
    folder_file <- file.path(
      tools::R_user_dir("aindpackin", "data"),
      "folders.rds"
    )
  } else {
    folder_file <- paste0(file, ".folders.rds")
  }

  if (!file.exists(folder_file)) {
    stop("Folder not found.")
  }

  folders <- readRDS(folder_file)

  # Check whether the folder contains snippets
  snippets <- list_snippets(file)

  has_snippets <- any(vapply(snippets, function(item) {
    item$folder == name ||
      startsWith(item$folder, paste0(name, "/"))
  }, logical(1)))

  if (has_snippets) {
    stop("Folder contains snippets. Move or delete them first.")
  }

  if (!(name %in% folders)) {
    stop("Folder not found.")
  }

  # Remove the folder
  folders <- folders[folders != name]

  saveRDS(folders, folder_file)

  invisible(name)
}

#' Rename a Folder
#'
#' Renames an existing folder and automatically updates
#' the folder assignments of all snippets inside it.
#' Preserves the snippets and their saved code.
#'
#' @param old_name Current name of the folder.
#' @param new_name New name for the folder.
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default storage location is used.
#'
#' @return Invisibly returns the new folder name.
#'
#' @export

rename_folder <- function(old_name, new_name, file = NULL) {

  old_name <- trimws(old_name)
  new_name <- trimws(new_name)

  if (!nzchar(new_name) ||
      grepl("/", new_name, fixed = TRUE)) {
    stop("Enter a valid folder name.")
  }

  existing <- unique(sub("/.*$", "", list_folders(file)))

  if (!(old_name %in% existing)) {
    stop("Original folder not found.")
  }

  if (new_name %in% existing) {
    stop("A folder with that name already exists.")
  }

  # Update snippets in the folder
  snippets <- list_snippets(file)

  if (length(snippets) > 0) {

    for (i in seq_along(snippets)) {

      folder <- snippets[[i]]$folder

      if (folder == old_name ||
          startsWith(folder, paste0(old_name, "/"))) {

        suffix <- substring(folder, nchar(old_name) + 1L)

        snippets[[i]]$folder <- paste0(new_name, suffix)
      }
    }

    names(snippets) <- vapply(snippets, function(item) {
      paste(item$folder, item$name, sep = "::")
    }, character(1))

    if (anyDuplicated(names(snippets))) {
      stop("Renaming would create duplicate snippets.")
    }
  }

  # Locate saved folder data
  if (is.null(file)) {
    data_dir <- tools::R_user_dir("aindpackin", "data")
    folder_file <- file.path(data_dir, "folders.rds")
    snippet_file <- file.path(data_dir, "snippets.rds")
  } else {
    folder_file <- paste0(file, ".folders.rds")
    snippet_file <- file
  }

  folders <- if (file.exists(folder_file)) {
    readRDS(folder_file)
  } else {
    character(0)
  }

  folders[folders == old_name] <- new_name

  if (!(new_name %in% folders)) {
    folders <- c(folders, new_name)
  }

  dir.create(dirname(folder_file),
             recursive = TRUE,
             showWarnings = FALSE)

  saveRDS(snippets, snippet_file)
  saveRDS(sort(unique(folders)), folder_file)

  invisible(new_name)
}

