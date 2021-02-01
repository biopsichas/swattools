#' Function to clean imported SWAT model output files
#'
#'This function removes unnecessary columns from imported SWAT output files.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @return Data.frame of cleaned outoup files
#' @importFrom dplyr select %>%
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' clean_swat_output(daily_sub)

clean_swat_output <- function(df){
  if ("FILE" %in% colnames(df)){
    selected_cols <- c("date", "RCH", "AREAkm2", "FLOW_OUTcms", "EVAPcms", "SED_OUTtons", "ORGN_OUTkg", "ORGP_OUTkg",
                       "NO3_OUTkg", "NH4_OUTkg", "NO2_OUTkg", "MINP_OUTkg", "TOTNkg", "TOTPkg")
  } else if ("LULC" %in% colnames(df)){
    selected_cols <- c("date", "LULC", "HRU", "GIS", "SUB", "AREAkm2", "WYLDmm", "ORGNkg_ha", "ORGPkg_ha", "SEDPkg_ha",
                       "NSURQkg_ha", "NLATQkg_ha", "NO3GWkg_ha", "SOLPkg_ha", "P_GWkg_ha", "YLDt_ha", "TNO3kg_ha")
  } else if ("SUB" %in% colnames(df)){
    selected_cols <- c("date", "RCH", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm", "PERCmm", "SURQmm",
                       "LATQmm", "GW_Qmm","WYLDmm", "SYLDt/ha", "ORGNkg/ha", "ORGPkg/ha", "NSURQkg/ha", "SOLPkg/ha",
                       "SEDPkg/ha", "LATNO3kg/ha", "GWNO3kg/ha", "TNO3kg/ha")
  } else {
    stop ("File type in unknown!!!")
  }

  return(select(df, all_of(selected_cols)))
}


#' Function to recalculate imported SWAT model output files to lower time scale
#'
#'This function recalculated imported SWAT output files to lower time scale.
#'Daily to monthly and yearly or monthly to yearly
#'
#' @param df Data.frame of imported and cleaned SWAT output.*** files
#' @return Data.frame of downgraded timescale
#' @importFrom dplyr select %>%
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' clean_swat_output(daily_sub)

time_scale_downgrade <- function(df, ){
  if ("FILE" %in% colnames(df)){
    selected_cols <- c("date", "RCH", "AREAkm2", "FLOW_OUTcms", "EVAPcms", "SED_OUTtons", "ORGN_OUTkg", "ORGP_OUTkg",
                       "NO3_OUTkg", "NH4_OUTkg", "NO2_OUTkg", "MINP_OUTkg", "TOTNkg", "TOTPkg")
  } else if ("LULC" %in% colnames(df)){
    selected_cols <- c("date", "LULC", "HRU", "GIS", "SUB", "AREAkm2", "WYLDmm", "ORGNkg_ha", "ORGPkg_ha", "SEDPkg_ha",
                       "NSURQkg_ha", "NLATQkg_ha", "NO3GWkg_ha", "SOLPkg_ha", "P_GWkg_ha", "YLDt_ha", "TNO3kg_ha")
  } else if ("SUB" %in% colnames(df)){
    selected_cols <- c("date", "RCH", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm", "PERCmm", "SURQmm",
                       "LATQmm", "GW_Qmm","WYLDmm", "SYLDt/ha", "ORGNkg/ha", "ORGPkg/ha", "NSURQkg/ha", "SOLPkg/ha",
                       "SEDPkg/ha", "LATNO3kg/ha", "GWNO3kg/ha", "TNO3kg/ha")
  } else {
    stop ("File type in unknown!!!")
  }

  return(select(df, all_of(selected_cols)))
}

