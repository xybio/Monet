#' Print Monet Object
#'
#' @param obj A Monet object.
#' @export
ViewMonet <- function(obj) {
  cat("Monet Object\n")
  cat("Number of GSM Samples:", length(obj$assays), "\n")
  if (!is.null(obj$meta.data$gse.index)) {
    cat("GSE Meta Data:\n")
    print(head(obj$meta.data$gse.index))
  }
  if (!is.null(obj$meta.data$gsm.index)) {
    cat("GSM Meta Data:\n")
    print(head(obj$meta.data$gsm.index))
  }
}