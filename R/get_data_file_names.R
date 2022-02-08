#' Get Data File Names (from Dataverse)
#'
#' Retrieve data file names in the Dataverse dataset.
#'
#' @import dataverse
#' @export
#' @examples
#' \dontrun{get_data_file_names()}

get_data_file_names <- function() {

  doi <- dataset_doi

  file_names <- dataverse::dataset_files(
    dataset = doi,
    version = ":latest",
    key = Sys.getenv("DATAVERSE_KEY"),
    server = Sys.getenv("DATAVERSE_SERVER")
  )

  names <- c()

  if (length(file_names) > 0)
  {
    for(i in 1:length(file_names))
    {
      names <- c(names, file_names[[i]]$dataFile$filename)
    }

  } else
  {
    names <- file_names
  }

  names
}

