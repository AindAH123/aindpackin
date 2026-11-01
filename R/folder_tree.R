
# Give every saved snippet a unique button ID
.snippet_link_id <- function(key) {
  numbers <- as.integer(charToRaw(enc2utf8(key)))
  paste0("snippet_", paste(numbers, collapse = "_"))
}


# Build expandable folders and subfolders
.make_folder_tree <- function(snippets) {

  if (length(snippets) == 0) {
    return(shiny::tags$p("No matching snippets."))
  }

  folders <- vapply(
    snippets,
    function(item) item$folder,
    character(1)
  )

  # Recursively build each folder level
  build_level <- function(path = "") {

    prefix <- if (nzchar(path)) {
      paste0(path, "/")
    } else {
      ""
    }

    # Find folders beneath the current path
    matching <- folders[
      startsWith(folders, prefix)
    ]

    remaining <- substring(
      matching,
      nchar(prefix) + 1L
    )

    remaining <- remaining[nzchar(remaining)]

    child_names <- if (length(remaining) > 0) {
      sort(unique(vapply(
        strsplit(remaining, "/", fixed = TRUE),
        function(x) x[1],
        character(1)
      )))
    } else {
      character(0)
    }

    # Create expandable folder elements
    folder_nodes <- lapply(child_names, function(child) {

      child_path <- if (nzchar(path)) {
        paste(path, child, sep = "/")
      } else {
        child
      }

      shiny::tags$details(
        style = "margin: 8px 0 8px 12px;",

        shiny::tags$summary(
          style = "cursor: pointer; font-weight: bold;",
          paste0("Folder: ", child)
        ),

        build_level(child_path)
      )
    })

    # Find snippets directly inside this folder
    keys <- names(snippets)[folders == path]

    snippet_nodes <- lapply(keys, function(key) {

      shiny::tags$div(
        style = "margin: 7px 0 7px 25px;",

        shiny::actionLink(
          inputId = .snippet_link_id(key),
          label = snippets[[key]]$name
        )
      )
    })

    shiny::tagList(c(folder_nodes, snippet_nodes))
  }

  build_level()
}
