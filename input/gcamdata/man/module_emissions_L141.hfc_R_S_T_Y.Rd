% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zemissions_L141.hfc_R_S_T_Y.R
\name{module_emissions_L141.hfc_R_S_T_Y}
\alias{module_emissions_L141.hfc_R_S_T_Y}
\title{module_emissions_L141.hfc_R_S_T_Y}
\usage{
module_emissions_L141.hfc_R_S_T_Y(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L141.hfc_R_S_T_Yh}, \code{L141.hfc_ef_R_cooling_Yh}. The corresponding file in the
original data system was \code{L141.hfc_R_S_T_Y.R} (emissions level1).
}
\description{
Calculate HFC emissions from EDGAR, by residential and commercial cooling shares,
adjusted to match the Guus Velders HFC inventory.
}
\details{
Describe in detail what this chunk does.
}
\author{
RMH Aug 2017
}
