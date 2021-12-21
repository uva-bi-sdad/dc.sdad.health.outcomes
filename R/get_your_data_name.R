#' Get YOUR DATA NAME  (Ex: Virginia SOL Scores).
#' Leaving outdir blank will provide the data directly.
#' If outdir is not blank, the data file will be saved to the directory specified.
#' @param datasetID doi of dataverse dataset.
#' @param file_name name of file to get from the dataverse dataset.
#' @param outdir target directory if downloading the file.
#' @import dataverse
#' @import readr
#' @export
#' @examples
#' get_YOUR_DATA_NAME("doi:xxx", "va_hdct_vdoe_2019_2021_3rd_grade_median_read_score.csv.xz", "outdir")

get_YOUR_DATA_NAME <- function(datasetID, file_name, outdir = "") {

  srv <- Sys.getenv("DATAVERSE_SERVER")
  doi <- datasetID    #readLines(here::here("data/dataset_doi"))[1]

  data_file_name <- file_name
  meta_file_name <- sub(".csv.xz", "_metadata.json", file_name)

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
    readr::write_lines(lines, gsub(".xz$", "", data_file_path))
    jsonlite::write_json(d, paste0(outdir, "/", meta_file_name), pretty = TRUE)
  }
}

