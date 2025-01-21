#' Parse GSE Metadata
#'
#' Parse metadata from a GSE soft file.
#'
#' @param file_path The path to the GSE soft file.
#' @return A data frame containing the parsed GSE metadata.
#' @export
ParseGSEMetadata <- function(file_path) {
  file_content <- readLines(file_path)
  metadata <- list()
  
  for (line in file_content) {
    if (grepl("^!", line)) {
      parts <- strsplit(line, " = ", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        key <- sub("^!", "", parts[1])
        value <- parts[2]
        metadata[[key]] <- value
      }
    }
  }
  
  return(as.data.frame(t(metadata), stringsAsFactors = FALSE))
}

#' Parse GSM Metadata
#'
#' Parse metadata from a GSM soft file.
#'
#' @param file_path The path to the GSM soft file.
#' @return A data frame containing the parsed GSM metadata.
#' @export
ParseGSMMeta <- function(file_path) {
  file_content <- readLines(file_path)
  metadata <- list()
  
  for (line in file_content) {
    if (grepl("^!", line)) {
      parts <- strsplit(line, " = ", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        key <- sub("^!", "", parts[1])
        value <- parts[2]
        metadata[[key]] <- value
      }
    }
  }
  
  return(as.data.frame(t(metadata), stringsAsFactors = FALSE))
}
