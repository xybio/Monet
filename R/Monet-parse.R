# Parse the metadata of a single GSE file
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

# Parse the metadata of a single GSM file
ParseGSMMetadata <- function(file_path) {
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
