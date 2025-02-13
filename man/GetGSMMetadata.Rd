% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Monet-metadata.R
\name{GetGSMMetadata}
\alias{GetGSMMetadata}
\title{Get GSM Metadata}
\usage{
GetGSMMetadata(monet_object, gsm_id = NULL)
}
\arguments{
\item{monet_object}{A Monet object.}

\item{gsm_id}{Optional GSM ID for specific query. If NULL, return all GSM metadata.}
}
\value{
A data frame containing GSM metadata.
}
\description{
Retrieve GSM metadata from the Monet object.
}
