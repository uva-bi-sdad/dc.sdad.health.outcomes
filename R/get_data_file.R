#' Get Data File (from Dataverse)
#'
#' Leaving outdir blank will provide the data directly.
#' If outdir is not blank, the data file will be saved to the (already created) directory specified.
#'
#' @param file_name name of file to get from the dataverse dataset.
#' @param outdir target directory if downloading the file.
#' @import dataverse
#' @import readr
#' @import jsonlite
#' @export
#' @examples
#' \dontrun{get_data_file("file_name.csv.xz", "./outdir")}

get_data_file <- function(file_name, outdir = "") {

  srv <- Sys.getenv("DATAVERSE_SERVER")
  doi <- dataset_doi

  data_file_name <- file_name
  meta_file_name <- sub(".csv.xz", "_metadata.json", file_name, fixed = TRUE)

  f <- dataverse::get_file_by_name(
    filename = data_file_name,
    dataset = doi,
    server = srv
  )

  d <- dataverse::dataset_metadata(
    dataset = doi,
    version = ":latest",
    block = "citation",
    server = srv
  )

  if (outdir == "") {
    tmp_file <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".xz")
    writeBin(f, tmp_file)
    pkg <- list(data = readr::read_csv(xzfile(tmp_file)),
                metadata = jsonlite::toJSON(d))
    return(pkg)
  } else {
    data_file_path <- paste0(outdir, "/", data_file_name)
    writeBin(f, data_file_path)
    lines <- readr::read_lines(df <- xzfile(data_file_path))
    readr::write_lines(lines, gsub("\\.xz$", "", data_file_path))
    jsonlite::write_json(d, paste0(outdir, "/", meta_file_name), pretty = TRUE)
  }
}

