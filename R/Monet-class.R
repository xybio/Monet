#' Monet Object Constructor
#'
#' Create a Monet object for GEO data management.
#'
#' @param assays A list of assays (expression data layers).
#' @param meta.data A list of metadata layers (gse.index, gsm.index, pmc.index).
#' @param active.assay The active assay for operations.
#' @param misc Additional information.
#' @return A Monet object.
#' @export
CreateMonetObject <- function(assays = list(), meta.data = list(), active.assay = NULL, misc = list()) {
  monet_object <- list(
    assays = assays,
    meta.data = list(
      gse.index = meta.data$gse.index %||% data.frame(),
      gsm.index = meta.data$gsm.index %||% data.frame(),
      gsem.map = meta.data$gsem.map %||% data.frame(),
      pmc.index = meta.data$pmc.index %||% data.frame()
    ),
    active.assay = active.assay,
    misc = misc
  )
  
  class(monet_object) <- "Monet"
  return(monet_object)
}
