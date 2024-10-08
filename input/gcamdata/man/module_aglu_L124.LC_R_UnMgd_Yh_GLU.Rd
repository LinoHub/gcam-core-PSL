% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaglu_L124.LC_R_UnMgd_Yh_GLU.R
\name{module_aglu_L124.LC_R_UnMgd_Yh_GLU}
\alias{module_aglu_L124.LC_R_UnMgd_Yh_GLU}
\title{module_aglu_L124.LC_R_UnMgd_Yh_GLU}
\usage{
module_aglu_L124.LC_R_UnMgd_Yh_GLU(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L124.LC_bm2_R_Shrub_Yh_GLU_adj}, \code{L124.LC_bm2_R_Grass_Yh_GLU_adj}, \code{L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj}, \code{L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj}. The corresponding file in the
original data system was \code{LB124.LC_R_UnMgd_Yh_GLU.R} (aglu level1).
}
\description{
Adjust land cover data for unmanaged land types (shrub, grass, unmgd pasture, unmgd forest) in L120.LC_bm2_R_LT_Yh_GLU to deduct
the extra cropland needs described in L122.LC_bm2_R_ExtraCropLand_Yh_GLU proportionally according to the relative share of each unmanaged
land type.
}
\details{
Initial unmanaged land cover estimates from L120.LC_bm2_R_LT_Yh_GLU for each unmanaged land type (shrub, grass,
unmgd pasture, unmgd forest) are aggregated to produce the total amount of unmanaged land in each region-GLU-year. This
is then used with the extra cropland calculated in L122.LC_bm2_R_ExtraCropLand_Yh_GLU to create an adjustment factor
for each region-GLU-year. This adjustment factor is applied to each unmanaged land type in each region-GLU-year so that
the extra cropland is deducted from each unmanaged land type according to its relative share.

Initial unmanaged pasture area in each region-glu-year is calculated as total pasture are in L120.LC_bm2_R_LT_Yh_GLU
minus managed pasture area in L123.LC_bm2_R_MgdPast_Yh_GLU.
Initial unmanaged forest area in each region-glu-year is calculated as total forest are in L120.LC_bm2_R_LT_Yh_GLU
minus managed forest area in L123.LC_bm2_R_MgdFor_Yh_GLU.
}
\author{
ACS August 2017
}
