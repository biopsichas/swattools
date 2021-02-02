#'Monthly output.sub data
#'
#'Data sample imported from monthly SWAT output.sub file with read_swat_output function from data-raw/monthly/output.sub.
#'
#' @format A data frame with 31 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{SUB}{File type}
#' \item{GIS}{Subbasin number}
#' \item{MON}{Month}
#' \item{AREAkm2}{Area of the subbasin (km2).}
#' \item{PRECIPmm}{Total amount of precipitation falling on the subbasin during time step (mm H2O)."}
#' \item{SNOMELTmm}{Total amount of snow melt on the subbasin during time step (mm H2O)."}
#' \item{PETmm}{Potential evapotranspiration from the subbasin during the time step (mm H2O)."}
#' \item{ETmm}{Actual evapotranspiration from the subbasin during the time step (mm).}
#' \item{SWmm}{Soil water content (mm). Amount of water in the soil profile at the end of the time period.}
#' \item{PERCmm}{Water that percolates past the root zone during the time step (mm).}
#' \item{SURQmm}{Surface runoff contribution to streamflow during time step (mm H2O).}
#' \item{GW_Qmm}{Groundwater contribution to streamflow (mm). Water from the shallow aquifer that returns to the reach during the time step.}
#' \item{WYLDmm}{Water yield (mm H2O). The net amount of water that leaves the subbasin and contributes to streamflow in the reach during the time step. (WYLD = SURQ + LATQ + GWQ – TLOSS – pond abstractions).}
#' \item{SYLDt/ha}{Sediment yield (metric tons/ha). Sediment from the subbasin that is transported into the reach during the time step.}
#' \item{ORGNkg/ha}{Organic N yield (kg N/ha). Organic nitrogen transported out of the subbasin and into the reach during the time step.}
#' \item{ORGPkg/ha}{Organic P yield (kg P/ha). Organic phosphorus transported with sediment into the reach during the time step.}
#' \item{NSURQkg/ha}{NO3 in surface runoff (kg N/ha). Nitrate transported by the surface runoff into the reach during the time step.}
#' \item{SOLPkg/ha}{Soluble P yield (kg P/ha). Phosphorus that is transported by surface runoff into the reach during the time step.}
#' \item{SEDPkg/ha}{Mineral P yield (kg P/ha). Mineral phosphorus attached to sediment that is transported by surface runoff into the reach during the time step.}
#' \item{LATQmm}{Lateral flow contribution to streamflow in watershed for the day, month or year (mm).}
#' \item{LATNO3kg/ha}{NO3-N in lateral flow (kg N/ha).}
#' \item{GWNO3kg/ha}{NO3-N in ground flow (kg N/ha).}
#' \item{CHOLAmic/L}{Concentration of chlorophyll-a for the day, month or year (mic/L).}
#' \item{CBODUmg/L}{Carbonaceous biochemical oxygen demand of material transported during time step (mg O2/L).}
#' \item{DOXQmg/L}{Amount of dissolved oxygen transported during time step (mg/L).}
#' \item{TNO3kg/ha}{NO3-N in tile drain flow (kg N/ha).}
#' \item{RCH}{Reach number for the subbasin.}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"monthly_sub"

#'Daily output.sub data
#'
#'Data sample imported from daily SWAT output.sub file with read_swat_output function from data-raw/daily/output.sub.
#'
#' @format A data frame with 31 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{SUB}{File type}
#' \item{GIS}{Subbasin number}
#' \item{MON}{Day in a year}
#' \item{AREAkm2}{Area of the subbasin (km2).}
#' \item{PRECIPmm}{Total amount of precipitation falling on the subbasin during time step (mm H2O)."}
#' \item{SNOMELTmm}{Total amount of snow melt on the subbasin during time step (mm H2O)."}
#' \item{PETmm}{Potential evapotranspiration from the subbasin during the time step (mm H2O)."}
#' \item{ETmm}{Actual evapotranspiration from the subbasin during the time step (mm).}
#' \item{SWmm}{Soil water content (mm). Amount of water in the soil profile at the end of the time period.}
#' \item{PERCmm}{Water that percolates past the root zone during the time step (mm).}
#' \item{SURQmm}{Surface runoff contribution to streamflow during time step (mm H2O).}
#' \item{GW_Qmm}{Groundwater contribution to streamflow (mm). Water from the shallow aquifer that returns to the reach during the time step.}
#' \item{WYLDmm}{Water yield (mm H2O). The net amount of water that leaves the subbasin and contributes to streamflow in the reach during the time step. (WYLD = SURQ + LATQ + GWQ – TLOSS – pond abstractions).}
#' \item{SYLDt/ha}{Sediment yield (metric tons/ha). Sediment from the subbasin that is transported into the reach during the time step.}
#' \item{ORGNkg/ha}{Organic N yield (kg N/ha). Organic nitrogen transported out of the subbasin and into the reach during the time step.}
#' \item{ORGPkg/ha}{Organic P yield (kg P/ha). Organic phosphorus transported with sediment into the reach during the time step.}
#' \item{NSURQkg/ha}{NO3 in surface runoff (kg N/ha). Nitrate transported by the surface runoff into the reach during the time step.}
#' \item{SOLPkg/ha}{Soluble P yield (kg P/ha). Phosphorus that is transported by surface runoff into the reach during the time step.}
#' \item{SEDPkg/ha}{Mineral P yield (kg P/ha). Mineral phosphorus attached to sediment that is transported by surface runoff into the reach during the time step.}
#' \item{LATQmm}{Lateral flow contribution to streamflow in watershed for the day, month or year (mm).}
#' \item{LATNO3kg/ha}{NO3-N in lateral flow (kg N/ha).}
#' \item{GWNO3kg/ha}{NO3-N in ground flow (kg N/ha).}
#' \item{CHOLAmic/L}{Concentration of chlorophyll-a for the day, month or year (mic/L).}
#' \item{CBODUmg/L}{Carbonaceous biochemical oxygen demand of material transported during time step (mg O2/L).}
#' \item{DOXQmg/L}{Amount of dissolved oxygen transported during time step (mg/L).}
#' \item{TNO3kg/ha}{NO3-N in tile drain flow (kg N/ha).}
#' \item{RCH}{Reach number for the subbasin.}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"daily_sub"

#'Yearly output.sub data
#'
#'Data sample imported from yearly SWAT output.sub file with read_swat_output function from data-raw/yearly/output.sub.
#'
#' @format A data frame with 31 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{SUB}{File type}
#' \item{GIS}{Subbasin number}
#' \item{MON}{Year}
#' \item{AREAkm2}{Area of the subbasin (km2).}
#' \item{PRECIPmm}{Total amount of precipitation falling on the subbasin during time step (mm H2O)."}
#' \item{SNOMELTmm}{Total amount of snow melt on the subbasin during time step (mm H2O)."}
#' \item{PETmm}{Potential evapotranspiration from the subbasin during the time step (mm H2O)."}
#' \item{ETmm}{Actual evapotranspiration from the subbasin during the time step (mm).}
#' \item{SWmm}{Soil water content (mm). Amount of water in the soil profile at the end of the time period.}
#' \item{PERCmm}{Water that percolates past the root zone during the time step (mm).}
#' \item{SURQmm}{Surface runoff contribution to streamflow during time step (mm H2O).}
#' \item{GW_Qmm}{Groundwater contribution to streamflow (mm). Water from the shallow aquifer that returns to the reach during the time step.}
#' \item{WYLDmm}{Water yield (mm H2O). The net amount of water that leaves the subbasin and contributes to streamflow in the reach during the time step. (WYLD = SURQ + LATQ + GWQ – TLOSS – pond abstractions).}
#' \item{SYLDt/ha}{Sediment yield (metric tons/ha). Sediment from the subbasin that is transported into the reach during the time step.}
#' \item{ORGNkg/ha}{Organic N yield (kg N/ha). Organic nitrogen transported out of the subbasin and into the reach during the time step.}
#' \item{ORGPkg/ha}{Organic P yield (kg P/ha). Organic phosphorus transported with sediment into the reach during the time step.}
#' \item{NSURQkg/ha}{NO3 in surface runoff (kg N/ha). Nitrate transported by the surface runoff into the reach during the time step.}
#' \item{SOLPkg/ha}{Soluble P yield (kg P/ha). Phosphorus that is transported by surface runoff into the reach during the time step.}
#' \item{SEDPkg/ha}{Mineral P yield (kg P/ha). Mineral phosphorus attached to sediment that is transported by surface runoff into the reach during the time step.}
#' \item{LATQmm}{Lateral flow contribution to streamflow in watershed for the day, month or year (mm).}
#' \item{LATNO3kg/ha}{NO3-N in lateral flow (kg N/ha).}
#' \item{GWNO3kg/ha}{NO3-N in ground flow (kg N/ha).}
#' \item{CHOLAmic/L}{Concentration of chlorophyll-a for the day, month or year (mic/L).}
#' \item{CBODUmg/L}{Carbonaceous biochemical oxygen demand of material transported during time step (mg O2/L).}
#' \item{DOXQmg/L}{Amount of dissolved oxygen transported during time step (mg/L).}
#' \item{TNO3kg/ha}{NO3-N in tile drain flow (kg N/ha).}
#' \item{RCH}{Reach number for the subbasin.}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"yearly_sub"

#'Monthly output.hru data
#'
#'Data sample imported from monthly SWAT output.hru file with read_swat_output function from data-raw/monthly/output.hru.
#'
#' @format A data frame with 21 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{LULC}{Four letter character code for the cover/plant on the HRU. (code from crop.dat file).}
#' \item{HRU}{Hydrologic response unit number.}
#' \item{GIS}{GIS code reprinted from watershed configuration file (.fig).}
#' \item{SUB}{Topographically-defined subbasin to which the HRU belongs.}
#' \item{MON}{Month}
#' \item{AREAkm2}{Drainage area of the HRU (km2 ).}
#' \item{WYLDmm}{Water yield (mm H2O). Total amount of water leaving the HRU and entering main channel during the time step. (WYLD = SURQ + LATQ + GWQ – TLOSS – pond abstractions).}
#' \item{ORGNkg_ha}{Organic N yield (kg N/ha). Organic nitrogen transported out of the HRU and into the reach during the time step.}
#' \item{ORGPkg_ha}{Organic P yield (kg P/ha). Organic phosphorus transported with sediment into the reach during the time step.}
#' \item{SEDPkg_ha}{Sediment P yield (kg P/ha). Mineral phosphorus sorbed to sediment transported into the reach during the time step.}
#' \item{NSURQkg_ha}{NO3 in surface runoff (kg N/ha). Nitrate transported with surface runoff into the reach during the time step.}
#' \item{NLATQkg_ha}{NO3 in lateral flow (kg N/ha). Nitrate transported by lateral flow into the reach during the time step}
#' \item{NO3GWkg_ha}{NO3 transported into main channel in the groundwater loading from the HRU (kg N/ha).}
#' \item{SOLPkg_ha}{Soluble P yield (kg P/ha). Soluble mineral forms of phosphorus transported by surface runoff into the reach during the time step.}
#' \item{P_GWkg_ha}{Soluble phosphorus transported by groundwater flow into main channel during the time step (kg P/ha).}
#' \item{YLDt_ha}{Harvested yield (metric tons/ha). The model partitions yield from the total biomass on a daily basis (and reports it). However, the actual yield is not known until it is harvested. The harvested yield is reported as dry weight.}
#' \item{TNO3kg_ha}{NO3 transported into main channel in the tile drain loading from the HRU (kg N/ha).}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"monthly_hru"

#'Yearly output.hru data
#'
#'Data sample imported from yearly SWAT output.hru file with read_swat_output function from data-raw/yearly/output.hru.
#'
#' @format A data frame with 21 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{LULC}{Four letter character code for the cover/plant on the HRU. (code from crop.dat file).}
#' \item{HRU}{Hydrologic response unit number.}
#' \item{GIS}{GIS code reprinted from watershed configuration file (.fig).}
#' \item{SUB}{Topographically-defined subbasin to which the HRU belongs.}
#' \item{MON}{Year}
#' \item{AREAkm2}{Drainage area of the HRU (km2 ).}
#' \item{WYLDmm}{Water yield (mm H2O). Total amount of water leaving the HRU and entering main channel during the time step. (WYLD = SURQ + LATQ + GWQ – TLOSS – pond abstractions).}
#' \item{ORGNkg_ha}{Organic N yield (kg N/ha). Organic nitrogen transported out of the HRU and into the reach during the time step.}
#' \item{ORGPkg_ha}{Organic P yield (kg P/ha). Organic phosphorus transported with sediment into the reach during the time step.}
#' \item{SEDPkg_ha}{Sediment P yield (kg P/ha). Mineral phosphorus sorbed to sediment transported into the reach during the time step.}
#' \item{NSURQkg_ha}{NO3 in surface runoff (kg N/ha). Nitrate transported with surface runoff into the reach during the time step.}
#' \item{NLATQkg_ha}{NO3 in lateral flow (kg N/ha). Nitrate transported by lateral flow into the reach during the time step}
#' \item{NO3GWkg_ha}{NO3 transported into main channel in the groundwater loading from the HRU (kg N/ha).}
#' \item{SOLPkg_ha}{Soluble P yield (kg P/ha). Soluble mineral forms of phosphorus transported by surface runoff into the reach during the time step.}
#' \item{P_GWkg_ha}{Soluble phosphorus transported by groundwater flow into main channel during the time step (kg P/ha).}
#' \item{YLDt_ha}{Harvested yield (metric tons/ha). The model partitions yield from the total biomass on a daily basis (and reports it). However, the actual yield is not known until it is harvested. The harvested yield is reported as dry weight.}
#' \item{TNO3kg_ha}{NO3 transported into main channel in the tile drain loading from the HRU (kg N/ha).}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"yearly_hru"


#'Monthly output.rch data
#'
#'Data sample imported from monthly SWAT output.rch file with read_swat_output function from data-raw/monthly/output.rch.
#'
#' @format A data frame with 55 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{FILE}{File type}
#' \item{RCH}{Reach number.}
#' \item{MON}{Month}
#' \item{AREAkm2}{Area drained by reach (km2).}
#' \item{FLOW_INcms}{Average daily streamflow into reach during time step (m3/s).}
#' \item{FLOW_OUTcms}{Average daily streamflow out of reach during time step (m3/s).}
#' \item{EVAPcms}{Average daily rate of water loss from reach by evaporation during time step (m3/s).}
#' \item{TLOSScms}{Average daily rate of water loss from reach by transmission through the streambed during time step (m3/s).}
#' \item{SED_INtons}{Sediment transported with water into reach during time step (metric tons).}
#' \item{SED_OUTtons}{Sediment transported with water out of reach during time step (metric tons).}
#' \item{SEDCONCmg_kg}{Concentration of sediment in reach during time step (mg/L).}
#' \item{ORGN_INkg}{Organic nitrogen transported with water into reach during time step (kg N).}
#' \item{ORGN_OUTkg}{Organic nitrogen transported with water out of reach during time step (kg N).}
#' \item{ORGP_INkg}{Organic phosphorus transported with water into reach during time step (kg P).}
#' \item{ORGP_OUTkg}{Organic phosphorus transported with water out of reach during time step (kg P).}
#' \item{NO3_INkg}{Nitrate transported with water into reach during time step (kg N).}
#' \item{NO3_OUTkg}{Nitrate transported with water out of reach during time step (kg N).}
#' \item{NH4_INkg}{Ammonium transported with water into reach during time step (kg N).}
#' \item{NH4_OUTkg}{Ammonium transported with water out of reach during time step (kg N).}
#' \item{NO2_INkg}{Nitrite transported with water into reach during time step (kg N).}
#' \item{NO2_OUTkg}{Nitrite transported with water out of reach during time step (kg N).}
#' \item{MINP_INkg}{Mineral phosphorus transported with water into reach during time step (kg P).}
#' \item{MINP_OUTkg}{Mineral phosphorus transported with water out of reach during time step (kg P).}
#' \item{CHLA_INkg}{Algal biomass transported with water into reach during time step (kg chl-a).}
#' \item{CHLA_OUTkg}{Algal biomass transported with water out of reach during time step (kg chl-a).}
#' \item{CBOD_INkg}{Carbonaceous biochemical oxygen demand of material transported into reach during time step (kg O2).}
#' \item{CBOD_OUTkg}{Carbonaceous biochemical oxygen demand of material transported out of reach during time step (kg O2).}
#' \item{DISOX_INkg}{Amount of dissolved oxygen transported into reach during time step (kg O2).}
#' \item{DISOX_OUTkg}{Amount of dissolved oxygen transported out of reach during time step (kg O2).}
#' \item{SOLPST_INmg}{Soluble pesticide transported with water into reach during time step (mg active ingredient).}
#' \item{SOLPST_OUTmg}{Soluble pesticide transported with water out of reach during time step (mg active ingredient).}
#' \item{SORPST_INmg}{Pesticide sorbed to sediment transported with water into reach during time step (mg active ingredient).}
#' \item{SORPST_OUTmg}{Pesticide sorbed to sediment transported with water out of reach during time step (mg active ingredient).}
#' \item{REACTPSTmg}{Loss of pesticide from water by reaction during time step (mg active ingredient).}
#' \item{VOLPSTmg}{Loss of pesticide from water by volatilization during time step (mg active ingredient).}
#' \item{SETTLPSTmg}{Transfer of pesticide from water to river bed sediment by settling during time step (mg active ingredient).}
#' \item{RESUSP_PSTmg}{Transfer of pesticide from river bed sediment to water by resuspension during time step (mg active ingredient).}
#' \item{DIFFUSEPSTmg}{Transfer of pesticide from water to river bed sediment by diffusion during time step (mg active ingredient).}
#' \item{REACBEDPSTmg}{Loss of pesticide from river bed sediment by reaction during time step (mg active ingredient).}
#' \item{BURYPSTmg}{Loss of pesticide from river bed sediment by burial during time step (mg active ingredient).}
#' \item{BED_PSTmg}{Pesticide in river bed sediment during time step (mg active ingredient).}
#' \item{BACTP_OUTct}{Number of persistent bacteria transported out of reach during time step (# cfu/100 mL).}
#' \item{BACTLP_OUTct}{Number of less persistent bacteria transported out of reach during time step (# cfu/100 mL).}
#' \item{CMETAL_1kg}{Conservative metal #1 transported out of reach (kg).}
#' \item{CMETAL_2kg}{Conservative metal #2 transported out of reach (kg).}
#' \item{CMETAL_3kg}{Conservative metal #3 transported out of reach (kg).}
#' \item{TOTNkg}{Total nitrogen transported with water out of reach during time step (kg N).}
#' \item{TOTPkg}{Total phosphorus transported with water out of reach during time step (kg N).}
#' \item{NO3_mg_l}{Nitrate concentration in reach N mg/L}
#' \item{WTMPdegc}{Water temperature degrees C.}
#' \item{RCH2}{Reach number dublicate.}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"monthly_rch"

#'Yearly output.rch data
#'
#'Data sample imported from yearly SWAT output.rch file with read_swat_output function from data-raw/yearly/output.rch.
#'
#' @format A data frame with 55 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{FILE}{File type}
#' \item{RCH}{Reach number.}
#' \item{MON}{Year}
#' \item{AREAkm2}{Area drained by reach (km2).}
#' \item{FLOW_INcms}{Average daily streamflow into reach during time step (m3/s).}
#' \item{FLOW_OUTcms}{Average daily streamflow out of reach during time step (m3/s).}
#' \item{EVAPcms}{Average daily rate of water loss from reach by evaporation during time step (m3/s).}
#' \item{TLOSScms}{Average daily rate of water loss from reach by transmission through the streambed during time step (m3/s).}
#' \item{SED_INtons}{Sediment transported with water into reach during time step (metric tons).}
#' \item{SED_OUTtons}{Sediment transported with water out of reach during time step (metric tons).}
#' \item{SEDCONCmg_kg}{Concentration of sediment in reach during time step (mg/L).}
#' \item{ORGN_INkg}{Organic nitrogen transported with water into reach during time step (kg N).}
#' \item{ORGN_OUTkg}{Organic nitrogen transported with water out of reach during time step (kg N).}
#' \item{ORGP_INkg}{Organic phosphorus transported with water into reach during time step (kg P).}
#' \item{ORGP_OUTkg}{Organic phosphorus transported with water out of reach during time step (kg P).}
#' \item{NO3_INkg}{Nitrate transported with water into reach during time step (kg N).}
#' \item{NO3_OUTkg}{Nitrate transported with water out of reach during time step (kg N).}
#' \item{NH4_INkg}{Ammonium transported with water into reach during time step (kg N).}
#' \item{NH4_OUTkg}{Ammonium transported with water out of reach during time step (kg N).}
#' \item{NO2_INkg}{Nitrite transported with water into reach during time step (kg N).}
#' \item{NO2_OUTkg}{Nitrite transported with water out of reach during time step (kg N).}
#' \item{MINP_INkg}{Mineral phosphorus transported with water into reach during time step (kg P).}
#' \item{MINP_OUTkg}{Mineral phosphorus transported with water out of reach during time step (kg P).}
#' \item{CHLA_INkg}{Algal biomass transported with water into reach during time step (kg chl-a).}
#' \item{CHLA_OUTkg}{Algal biomass transported with water out of reach during time step (kg chl-a).}
#' \item{CBOD_INkg}{Carbonaceous biochemical oxygen demand of material transported into reach during time step (kg O2).}
#' \item{CBOD_OUTkg}{Carbonaceous biochemical oxygen demand of material transported out of reach during time step (kg O2).}
#' \item{DISOX_INkg}{Amount of dissolved oxygen transported into reach during time step (kg O2).}
#' \item{DISOX_OUTkg}{Amount of dissolved oxygen transported out of reach during time step (kg O2).}
#' \item{SOLPST_INmg}{Soluble pesticide transported with water into reach during time step (mg active ingredient).}
#' \item{SOLPST_OUTmg}{Soluble pesticide transported with water out of reach during time step (mg active ingredient).}
#' \item{SORPST_INmg}{Pesticide sorbed to sediment transported with water into reach during time step (mg active ingredient).}
#' \item{SORPST_OUTmg}{Pesticide sorbed to sediment transported with water out of reach during time step (mg active ingredient).}
#' \item{REACTPSTmg}{Loss of pesticide from water by reaction during time step (mg active ingredient).}
#' \item{VOLPSTmg}{Loss of pesticide from water by volatilization during time step (mg active ingredient).}
#' \item{SETTLPSTmg}{Transfer of pesticide from water to river bed sediment by settling during time step (mg active ingredient).}
#' \item{RESUSP_PSTmg}{Transfer of pesticide from river bed sediment to water by resuspension during time step (mg active ingredient).}
#' \item{DIFFUSEPSTmg}{Transfer of pesticide from water to river bed sediment by diffusion during time step (mg active ingredient).}
#' \item{REACBEDPSTmg}{Loss of pesticide from river bed sediment by reaction during time step (mg active ingredient).}
#' \item{BURYPSTmg}{Loss of pesticide from river bed sediment by burial during time step (mg active ingredient).}
#' \item{BED_PSTmg}{Pesticide in river bed sediment during time step (mg active ingredient).}
#' \item{BACTP_OUTct}{Number of persistent bacteria transported out of reach during time step (# cfu/100 mL).}
#' \item{BACTLP_OUTct}{Number of less persistent bacteria transported out of reach during time step (# cfu/100 mL).}
#' \item{CMETAL_1kg}{Conservative metal #1 transported out of reach (kg).}
#' \item{CMETAL_2kg}{Conservative metal #2 transported out of reach (kg).}
#' \item{CMETAL_3kg}{Conservative metal #3 transported out of reach (kg).}
#' \item{TOTNkg}{Total nitrogen transported with water out of reach during time step (kg N).}
#' \item{TOTPkg}{Total phosphorus transported with water out of reach during time step (kg N).}
#' \item{NO3_mg_l}{Nitrate concentration in reach N mg/L}
#' \item{WTMPdegc}{Water temperature degrees C.}
#' \item{RCH2}{Reach number dublicate.}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"yearly_rch"

#'Daily output.rch data
#'
#'Data sample imported from daily SWAT output.rch file with read_swat_output function from data-raw/daily/output.rch.
#'
#' @format A data frame with 55 variables:
#' \describe{
#' \item{date}{Date}
#' \item{Year}{Year}
#' \item{Month}{Month}
#' \item{Day}{Day}
#' \item{FILE}{File type}
#' \item{RCH}{Reach number.}
#' \item{MON}{Day}
#' \item{AREAkm2}{Area drained by reach (km2).}
#' \item{FLOW_INcms}{Average daily streamflow into reach during time step (m3/s).}
#' \item{FLOW_OUTcms}{Average daily streamflow out of reach during time step (m3/s).}
#' \item{EVAPcms}{Average daily rate of water loss from reach by evaporation during time step (m3/s).}
#' \item{TLOSScms}{Average daily rate of water loss from reach by transmission through the streambed during time step (m3/s).}
#' \item{SED_INtons}{Sediment transported with water into reach during time step (metric tons).}
#' \item{SED_OUTtons}{Sediment transported with water out of reach during time step (metric tons).}
#' \item{SEDCONCmg_kg}{Concentration of sediment in reach during time step (mg/L).}
#' \item{ORGN_INkg}{Organic nitrogen transported with water into reach during time step (kg N).}
#' \item{ORGN_OUTkg}{Organic nitrogen transported with water out of reach during time step (kg N).}
#' \item{ORGP_INkg}{Organic phosphorus transported with water into reach during time step (kg P).}
#' \item{ORGP_OUTkg}{Organic phosphorus transported with water out of reach during time step (kg P).}
#' \item{NO3_INkg}{Nitrate transported with water into reach during time step (kg N).}
#' \item{NO3_OUTkg}{Nitrate transported with water out of reach during time step (kg N).}
#' \item{NH4_INkg}{Ammonium transported with water into reach during time step (kg N).}
#' \item{NH4_OUTkg}{Ammonium transported with water out of reach during time step (kg N).}
#' \item{NO2_INkg}{Nitrite transported with water into reach during time step (kg N).}
#' \item{NO2_OUTkg}{Nitrite transported with water out of reach during time step (kg N).}
#' \item{MINP_INkg}{Mineral phosphorus transported with water into reach during time step (kg P).}
#' \item{MINP_OUTkg}{Mineral phosphorus transported with water out of reach during time step (kg P).}
#' \item{CHLA_INkg}{Algal biomass transported with water into reach during time step (kg chl-a).}
#' \item{CHLA_OUTkg}{Algal biomass transported with water out of reach during time step (kg chl-a).}
#' \item{CBOD_INkg}{Carbonaceous biochemical oxygen demand of material transported into reach during time step (kg O2).}
#' \item{CBOD_OUTkg}{Carbonaceous biochemical oxygen demand of material transported out of reach during time step (kg O2).}
#' \item{DISOX_INkg}{Amount of dissolved oxygen transported into reach during time step (kg O2).}
#' \item{DISOX_OUTkg}{Amount of dissolved oxygen transported out of reach during time step (kg O2).}
#' \item{SOLPST_INmg}{Soluble pesticide transported with water into reach during time step (mg active ingredient).}
#' \item{SOLPST_OUTmg}{Soluble pesticide transported with water out of reach during time step (mg active ingredient).}
#' \item{SORPST_INmg}{Pesticide sorbed to sediment transported with water into reach during time step (mg active ingredient).}
#' \item{SORPST_OUTmg}{Pesticide sorbed to sediment transported with water out of reach during time step (mg active ingredient).}
#' \item{REACTPSTmg}{Loss of pesticide from water by reaction during time step (mg active ingredient).}
#' \item{VOLPSTmg}{Loss of pesticide from water by volatilization during time step (mg active ingredient).}
#' \item{SETTLPSTmg}{Transfer of pesticide from water to river bed sediment by settling during time step (mg active ingredient).}
#' \item{RESUSP_PSTmg}{Transfer of pesticide from river bed sediment to water by resuspension during time step (mg active ingredient).}
#' \item{DIFFUSEPSTmg}{Transfer of pesticide from water to river bed sediment by diffusion during time step (mg active ingredient).}
#' \item{REACBEDPSTmg}{Loss of pesticide from river bed sediment by reaction during time step (mg active ingredient).}
#' \item{BURYPSTmg}{Loss of pesticide from river bed sediment by burial during time step (mg active ingredient).}
#' \item{BED_PSTmg}{Pesticide in river bed sediment during time step (mg active ingredient).}
#' \item{BACTP_OUTct}{Number of persistent bacteria transported out of reach during time step (# cfu/100 mL).}
#' \item{BACTLP_OUTct}{Number of less persistent bacteria transported out of reach during time step (# cfu/100 mL).}
#' \item{CMETAL_1kg}{Conservative metal #1 transported out of reach (kg).}
#' \item{CMETAL_2kg}{Conservative metal #2 transported out of reach (kg).}
#' \item{CMETAL_3kg}{Conservative metal #3 transported out of reach (kg).}
#' \item{TOTNkg}{Total nitrogen transported with water out of reach during time step (kg N).}
#' \item{TOTPkg}{Total phosphorus transported with water out of reach during time step (kg N).}
#' \item{NO3_mg_l}{Nitrate concentration in reach N mg/L}
#' \item{WTMPdegc}{Water temperature degrees C.}
#' \item{RCH2}{Reach number dublicate.}
#' }
#'
#' For further details, see \url{https://swat.tamu.edu/media/116601/ch32_output.pdf}
#'
"daily_rch"

#'Rivers data
#'
#'Data of river network used in modeling.
#'
#' @format A sf data.frame with 2 variables:
#' \describe{
#' \item{CID}{Catchment ID}
#' \item{geometry}{Geometry of objects}
#' }
#'
"rivers"

#'Basin data
#'
#'Data of basins used in modeling.
#'
#' @format A sf data.frame with 2 variables:
#' \describe{
#' \item{Setup_name}{SWAT setup name}
#' \item{Reach}{Reach number correcting to reach or subbasin numbers in SWAT ouput files.}
#' \item{CID}{Catchment ID. It is the same as in rivers CID.}
#' \item{Subbasin}{Subbasins aggregating SWAT setups.}
#' \item{Area}{Area in km2}
#' \item{CIDto}{CID of subbasin where water flows out of this subbasin}
#' \item{geometry}{Geometry of objects}
#' }
#'
"basins"

