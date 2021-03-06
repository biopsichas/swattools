% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{monthly_hru}
\alias{monthly_hru}
\title{Monthly output.hru data}
\format{
A data frame with 21 variables:
\describe{
\item{date}{Date}
\item{Year}{Year}
\item{Month}{Month}
\item{Day}{Day}
\item{LULC}{Four letter character code for the cover/plant on the HRU. (code from crop.dat file).}
\item{HRU}{Hydrologic response unit number.}
\item{GIS}{GIS code reprinted from watershed configuration file (.fig).}
\item{SUB}{Topographically-defined subbasin to which the HRU belongs.}
\item{MON}{Month}
\item{AREAkm2}{Drainage area of the HRU (km2 ).}
\item{WYLDmm}{Water yield (mm H2O). Total amount of water leaving the HRU and entering main channel during the time step. (WYLD = SURQ + LATQ + GWQ – TLOSS – pond abstractions).}
\item{ORGNkg_ha}{Organic N yield (kg N/ha). Organic nitrogen transported out of the HRU and into the reach during the time step.}
\item{ORGPkg_ha}{Organic P yield (kg P/ha). Organic phosphorus transported with sediment into the reach during the time step.}
\item{SEDPkg_ha}{Sediment P yield (kg P/ha). Mineral phosphorus sorbed to sediment transported into the reach during the time step.}
\item{NSURQkg_ha}{NO3 in surface runoff (kg N/ha). Nitrate transported with surface runoff into the reach during the time step.}
\item{NLATQkg_ha}{NO3 in lateral flow (kg N/ha). Nitrate transported by lateral flow into the reach during the time step}
\item{NO3GWkg_ha}{NO3 transported into main channel in the groundwater loading from the HRU (kg N/ha).}
\item{SOLPkg_ha}{Soluble P yield (kg P/ha). Soluble mineral forms of phosphorus transported by surface runoff into the reach during the time step.}
\item{P_GWkg_ha}{Soluble phosphorus transported by groundwater flow into main channel during the time step (kg P/ha).}
\item{YLDt_ha}{Harvested yield (metric tons/ha). The model partitions yield from the total biomass on a daily basis (and reports it). However, the actual yield is not known until it is harvested. The harvested yield is reported as dry weight.}
\item{TNO3kg_ha}{NO3 transported into main channel in the tile drain loading from the HRU (kg N/ha).}
}

For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
}
\usage{
monthly_hru
}
\description{
Data sample imported from monthly SWAT output.hru file with read_swat_output function from data-raw/monthly/output.hru.
}
\keyword{datasets}
