#' Add GSE Data to Monet Object
#'
#' Add or update GSE metadata in the Monet object.
#'
#' @param monet_object A Monet object.
#' @param gse_data A data frame containing GSE metadata to add.
#' @return The updated Monet object.
#' @export
AddGSEData <- function(monet_object, gse_data) {
  monet_object$meta.data$gse.index <- rbind(
    monet_object$meta.data$gse.index,
    gse_data
  )
  return(monet_object)
}

#' Get GSE Metadata
#'
#' Retrieve GSE metadata from the Monet object.
#'
#' @param monet_object A Monet object.
#' @return A data frame containing GSE metadata.
#' @export
GetGSEMetadata <- function(monet_object) {
  monet_object$meta.data$gse.index
}

#' Add GSM Data to Monet Object
#'
#' Add or update GSM metadata in the Monet object.
#'
#' @param monet_object A Monet object.
#' @param gsm_data A data frame containing GSM metadata to add.
#' @return The updated Monet object.
#' @export
AddGSMData <- function(monet_object, gsm_data) {
  monet_object$meta.data$gsm.index <- rbind(
    monet_object$meta.data$gsm.index,
    gsm_data
  )
  return(monet_object)
}

#' Get GSM Metadata
#'
#' Retrieve GSM metadata from the Monet object.
#'
#' @param monet_object A Monet object.
#' @param gsm_id Optional GSM ID for specific query. If NULL, return all GSM metadata.
#' @return A data frame containing GSM metadata.
#' @export
GetGSMMetadata <- function(monet_object, gsm_id = NULL) {
  if (is.null(gsm_id)) {
    return(monet_object$meta.data$gsm.index)
  }
  subset(monet_object$meta.data$gsm.index, GSM_ID == gsm_id)
}

#' Add PMC Data to Monet Object
#'
#' Add or update PMC metadata in the Monet object.
#'
#' @param monet_object A Monet object.
#' @param pmc_data A data frame containing PMC metadata to add.
#' @return The updated Monet object.
#' @export
AddPMCData <- function(monet_object, pmc_data) {
  monet_object$meta.data$pmc.index <- rbind(
    monet_object$meta.data$pmc.index,
    pmc_data
  )
  return(monet_object)
}

#' Get PMC Metadata
#'
#' Retrieve PMC metadata from the Monet object.
#'
#' @param monet_object A Monet object.
#' @param pmc_id Optional PMC ID for specific query. If NULL, return all PMC metadata.
#' @return A data frame containing PMC metadata.
#' @export
GetPMCMetadata <- function(monet_object, pmc_id = NULL) {
  if (is.null(pmc_id)) {
    return(monet_object$meta.data$pmc.index)
  }
  subset(monet_object$meta.data$pmc.index, PMC_ID == pmc_id)
}
