
#' Open the aindpackin Snippet Manager
#'
#' Launches an interactive RStudio window for managing
#' reusable code snippets. Users can create, edit, delete,
#' search, and organize snippets into folders.
#'
#' @param file Optional path to an RDS storage file.
#'   If NULL, the default user storage location is used.
#'
#' @return Invisibly returns the selected snippet key,
#'   or NULL if the window is cancelled.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' open_vault()
#' }


open_vault <- function(file = NULL) {

  if (!rstudioapi::isAvailable()) {
    stop("This feature requires RStudio.")
  }

  editor <- rstudioapi::getSourceEditorContext()

  # Build the grouped dropdown
  make_choices <- function(snippets) {

    if (length(snippets) == 0) {
      return(character(0))
    }

    keys <- names(snippets)

    folders <- vapply(
      snippets,
      function(x) x$folder,
      character(1)
    )

    groups <- split(seq_along(keys), folders)

    lapply(groups, function(i) {
      stats::setNames(
        keys[i],
        vapply(snippets[i], function(x) x$name,
               character(1))
      )
    })
  }

  # Popup layout
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("aindpackin"),

    miniUI::miniContentPanel(


      shiny::tags$h4("Snippet Management"),

      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;",

        shiny::actionButton("add", "+ New Snippet"),
        shiny::actionButton("edit", "Edit Snippet"),
        shiny::actionButton("delete", "Delete Snippet")
      ),

      shiny::tags$h4("Folder Management"),

      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;",

        shiny::actionButton("add_folder", "+ New Folder"),
        shiny::actionButton("edit_folder_btn", "Edit Folder"),
        shiny::actionButton("remove_folder_btn", "Delete Folder")
      ),



      shiny::textInput(
        "search",
        "Search Snippets",
        placeholder = "Search by name, folder, or code"
      ),

      shiny::selectInput(
        "browse_folder",
        "Folder",
        choices = sort(unique(
          sub("/.*$", "", list_folders(file))
        ))
      ),



      shiny::hr(),

      shiny::selectInput(
        "snippet",
        "Saved Snippets",
        choices = make_choices(list_snippets(file))
      ),





      shiny::tags$h4("Code Preview"),

      shiny::verbatimTextOutput("preview")
    )
  )

  server <- function(input, output, session) {

    snippets <- shiny::reactiveVal(
      list_snippets(file)
    )





    # Search saved snippets
    shiny::observeEvent(
      list(input$search, input$browse_folder), {

      current <- snippets()

      # Only show snippets from the selected folder
      folder <- input$browse_folder

      if (is.null(folder) || !nzchar(folder)) {
        current <- list()
      } else {

        matches_folder <- vapply(current, function(item) {
          item$folder == folder ||
            startsWith(item$folder, paste0(folder, "/"))
        }, logical(1))

        current <- current[matches_folder]
      }

      query <- tolower(trimws(input$search))

      if (nzchar(query)) {

        matches <- vapply(current, function(item) {

          text <- tolower(c(
            item$name,
            item$folder,
            item$code
          ))

          any(grepl(query, text, fixed = TRUE))

        }, logical(1))

        current <- current[matches]
      }

      selected <- if (length(current) > 0) {
        names(current)[1]
      } else {
        character(0)
      }

      shiny::updateSelectInput(
        session,
        "snippet",
        choices = make_choices(current),
        selected = selected
      )

    }, ignoreInit = FALSE)


    # Show selected code
    output$preview <- shiny::renderText({

      selected <- input$snippet
      current <- snippets()

      if (is.null(selected) ||
          !selected %in% names(current)) {
        return("No snippet selected.")
      }

      current[[selected]]$code
    })

    # Save the new folder
    shiny::observeEvent(input$save_folder, {

      name <- trimws(input$folder_name)

      success <- tryCatch({

        create_folder(name, file = file)
        TRUE

      }, error = function(e) {

        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )

        FALSE
      })

      if (success) {

        shiny::removeModal()

        shiny::updateSelectInput(
          session,
          "browse_folder",
          choices = sort(unique(
            sub("/.*$", "", list_folders(file))
          )),
          selected = name
        )

        shiny::showNotification(
          "Folder created!",
          type = "message"
        )
      }
    })

    # Open Delete Folder confirmation
    shiny::observeEvent(input$remove_folder_btn, {

      folder <- input$browse_folder

      if (is.null(folder) || !nzchar(folder)) {
        shiny::showNotification(
          "Select a folder first.",
          type = "warning"
        )
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = "Delete Folder",

        paste("Are you sure you want to delete", folder, "?"),

        shiny::tags$p(
          "Only empty folders can be deleted."
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "confirm_remove_folder",
            "Delete Folder"
          )
        )
      ))
    })


    # Confirm and delete the folder
    shiny::observeEvent(input$confirm_remove_folder, {

      folder <- input$browse_folder

      success <- tryCatch({

        delete_folder(folder, file = file)
        TRUE

      }, error = function(e) {

        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )
        FALSE
      })

      if (success) {

        remaining <- sort(unique(
          sub("/.*$", "", list_folders(file))
        ))

        shiny::updateSelectInput(
          session,
          "browse_folder",
          choices = remaining,
          selected = if (length(remaining) > 0) {
            remaining[1]
          } else {
            character(0)
          }
        )

        shiny::removeModal()

        shiny::showNotification(
          "Folder deleted!",
          type = "message"
        )
      }
    })

    # Open the Edit Folder window
    shiny::observeEvent(input$edit_folder_btn, {

      folder <- input$browse_folder

      if (is.null(folder) || !nzchar(folder)) {
        shiny::showNotification(
          "Select a folder first.",
          type = "warning"
        )
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = "Rename Folder",

        shiny::textInput(
          "rename_folder_name",
          "New Folder Name",
          value = folder
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "confirm_rename_folder",
            "Save Changes"
          )
        )
      ))
    })

    # Save the renamed folder
    shiny::observeEvent(input$confirm_rename_folder, {

      old_name <- input$browse_folder
      new_name <- trimws(input$rename_folder_name)

      if (identical(old_name, new_name)) {
        shiny::removeModal()
        return()
      }

      success <- tryCatch({
        rename_folder(old_name, new_name, file = file)
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )
        FALSE
      })

      if (success) {

        folders <- sort(unique(
          sub("/.*$", "", list_folders(file))
        ))

        shiny::updateSelectInput(
          session,
          "browse_folder",
          choices = folders,
          selected = new_name
        )

        snippets(list_snippets(file))

        shiny::updateSelectInput(
          session,
          "snippet",
          choices = make_choices(snippets())
        )

        shiny::removeModal()

        shiny::showNotification(
          "Folder renamed!",
          type = "message"
        )
      }
    })

    # Open the new-snippet form


    # Open the new-folder window

    shiny::observeEvent(input$add_folder, {


      shiny::showModal(shiny::modalDialog(
        title = "Create New Folder",

        shiny::textInput(
          "folder_name",
          "Folder Name"
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "save_folder", "Create Folder"
          )
        )
      ))
    })

    shiny::observeEvent(input$add, {

      shiny::showModal(shiny::modalDialog(
        title = "Create New Snippet",

        shiny::textInput(
          "new_name", "Snippet Name"
        ),


        shiny::selectizeInput(
          "new_folder",
          "Folder",
          choices = sort(unique(c(
            "General",
            sub("/.*$", "", list_folders(file))
          ))),
          selected = "General",
          options = list(create = TRUE)
        ),


        shiny::textAreaInput(
          "new_code", "Code", rows = 7
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "save_new", "Save Snippet"
          )
        )
      ))
    })

    # Save the new snippet
    shiny::observeEvent(input$save_new, {

      name <- trimws(input$new_name)
      folder <- trimws(input$new_folder)

      subfolder <- input$new_subfolder

      if (!is.null(subfolder) && nzchar(trimws(subfolder))) {
        folder <- paste(folder, trimws(subfolder), sep = "/")
      }

      code <- input$new_code

      success <- tryCatch({

        if (!nzchar(name) ||
            !nzchar(folder) ||
            !nzchar(trimws(code))) {
          stop("All fields are required.")
        }

        save_snippet(
          name = name,
          code = code,
          folder = folder,
          file = file
        )

        TRUE

      }, error = function(e) {
        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )
        FALSE
      })

      if (success) {

        snippets(list_snippets(file))


        # Refresh only snippets from the saved folder
        selected_folder <- sub("/.*$", "", folder)

        current <- snippets()

        matches <- vapply(current, function(item) {
          item$folder == selected_folder ||
            startsWith(item$folder, paste0(selected_folder, "/"))
        }, logical(1))

        visible <- current[matches]

        # Keep the search filter active
        query <- tolower(trimws(input$search))

        if (nzchar(query)) {
          matches_search <- vapply(visible, function(item) {
            any(grepl(
              query,
              tolower(c(item$name, item$folder, item$code)),
              fixed = TRUE
            ))
          }, logical(1))

          visible <- visible[matches_search]
        }

        # Select the folder where the snippet was saved
        shiny::updateSelectInput(
          session,
          "browse_folder",
          choices = sort(unique(
            sub("/.*$", "", list_folders(file))
          )),
          selected = selected_folder
        )

        # Show only matching snippets
        new_key <- paste(folder, name, sep = "::")

        shiny::updateSelectInput(
          session,
          "snippet",
          choices = make_choices(visible),
          selected = if (new_key %in% names(visible)) {
            new_key
          } else if (length(visible) > 0) {
            names(visible)[1]
          } else {
            character(0)
          }
        )


        shiny::removeModal()

        shiny::showNotification(
          "Snippet saved!",
          type = "message"
        )
      }
    })

    # Open the edit window
    shiny::observeEvent(input$edit, {

      selected <- input$snippet
      current <- snippets()

      if (is.null(selected) ||
          !selected %in% names(current)) {
        shiny::showNotification("Select a snippet first.",
                                type = "warning")
        return()
      }

      item <- current[[selected]]

      shiny::showModal(shiny::modalDialog(
        title = paste("Edit:", item$name),

        shiny::textAreaInput(
          "edit_code",
          "Edit Code",
          value = item$code,
          rows = 8
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("save_edit", "Save Changes")
        )
      ))
    })

    # Save the changes
    shiny::observeEvent(input$save_edit, {

      selected <- input$snippet
      current <- snippets()

      if (is.null(selected) ||
          !selected %in% names(current)) {
        return()
      }

      if (!nzchar(trimws(input$edit_code))) {
        shiny::showNotification("Code cannot be empty.",
                                type = "error")
        return()
      }

      item <- current[[selected]]

      success <- tryCatch({
        update_snippet(
          name = item$name,
          code = input$edit_code,
          folder = item$folder,
          file = file
        )
        TRUE
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e),
                                type = "error")
        FALSE
      })

      if (success) {
        snippets(list_snippets(file))
        shiny::removeModal()
        shiny::showNotification("Snippet updated!")
      }
    })

    # Ask for confirmation before deleting
    shiny::observeEvent(input$delete, {

      selected <- input$snippet
      current <- snippets()

      if (is.null(selected) ||
          !selected %in% names(current)) {
        shiny::showNotification(
          "Select a snippet first.",
          type = "warning"
        )
        return()
      }

      item <- current[[selected]]

      shiny::showModal(shiny::modalDialog(
        title = "Delete Snippet",

        paste(
          "Are you sure you want to delete",
          item$name, "?"
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "confirm_delete", "Delete"
          )
        )
      ))
    })

    # Delete the selected snippet
    shiny::observeEvent(input$confirm_delete, {

      selected <- input$snippet
      current <- snippets()

      if (is.null(selected) ||
          !selected %in% names(current)) {
        return()
      }

      item <- current[[selected]]

      success <- tryCatch({

        delete_snippet(
          name = item$name,
          folder = item$folder,
          file = file
        )

        TRUE

      }, error = function(e) {
        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )
        FALSE
      })

      if (success) {

        remaining <- list_snippets(file)
        snippets(remaining)


        # Keep only snippets from the selected folder
        folder <- input$browse_folder

        visible <- remaining[vapply(remaining, function(item) {
          item$folder == folder ||
            startsWith(item$folder, paste0(folder, "/"))
        }, logical(1))]

        # Preserve the search filter
        query <- if (is.null(input$search)) {
          ""
        } else {
          tolower(trimws(input$search))
        }

        if (nzchar(query)) {
          visible <- visible[vapply(visible, function(item) {
            any(grepl(
              query,
              tolower(c(item$name, item$folder, item$code)),
              fixed = TRUE
            ))
          }, logical(1))]
        }

        next_selected <- if (length(visible) > 0) {
          names(visible)[1]
        } else {
          character(0)
        }

        shiny::updateSelectInput(
          session,
          "snippet",
          choices = make_choices(visible),
          selected = next_selected
        )


        shiny::removeModal()

        shiny::showNotification(
          "Snippet deleted!",
          type = "message"
        )
      }
    })

    shiny::observeEvent(input$done, {
      shiny::req(input$snippet)
      shiny::stopApp(input$snippet)
    })


    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })

  }

  selected <- shiny::runGadget(
    ui,
    server,
    viewer = shiny::dialogViewer(
      "aindpackin",
      width = 700,
      height = 550
    ),
    stopOnCancel = FALSE
  )

  if (is.null(selected)) {
    return(invisible(NULL))
  }

  saved <- list_snippets(file)

  if (!selected %in% names(saved)) {
    stop("Selected snippet was not found.")
  }

  rstudioapi::insertText(
    text = saved[[selected]]$code,
    id = editor$id
  )

  invisible(selected)
}


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
