#' Get Virginia SOL Scores.
#' Leaving outdir blank will provide the data directly.
#' If outdir is not blank, the data file will be saved to the directory specified.
#' @test name of SOL test to download
#' @param outdir target directory if downloading the file.
#' @import dataverse
#' @import readr
#' @export
#' @examples

get_YOUR_DATA_NAME <- function(test = "3rd grade reading", outdir = "") {
  srv <- "dataverse.lib.virginia.edu"
  doi <- readLines(here::here("data/dataset_doi"))[1]

  data_file_name <- "va_hdct_vdoe_2019_2021_3rd_grade_median_read_score.csv.xz"
  meta_file_name <- "va_hdct_vdoe_2019_2021_3rd_grade_median_read_score_metadata.json"



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
    writeBin(f, tmp_file, data_file_name)
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

