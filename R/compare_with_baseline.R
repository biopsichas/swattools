#' Function to get scenario difference from baseline values
#'
#'This function calculates percentage difference comparing scenario
#'with its baseline. It could be used with raw or averaged data.
#'
#' @param df Data.frame of imported output.*** SWAT file with extract_watersheds_output function
#' @param baseline_period Vector of two values. First with year of beginning
#' of baseline period and second with end of it. Those two values should have
#' been used in naming scenario folder. Example (c("2000", "2019"))
#' @return Data.frame with all numeric columns recalculated as percentage from baseline.
#' @importFrom dplyr arrange bind_rows %>%
#' @export

get_diff_from_baseline <- function(df, baseline_period = NA){

  ##Getting all different outputs available and separating them into baselines
  ## scenarios and non baseline scenarios
  scenarios <- unique(df$SCENARIO)
  if (!is.na(baseline_period)){
    base <- paste0("_baseline_", baseline_period[1], "_", baseline_period[2], "$")
    baseline_scenarios <- scenarios[grep(base, scenarios)]
    measures_scenarios <- scenarios[-grep(base, scenarios)]
  } else {
    baseline_scenarios <- scenarios[grep("_baseline$", scenarios)]
    measures_scenarios <- scenarios[-grep("_baseline$", scenarios)]
  }

  ##Identifying inputs to the function
  if ("RCH" %in% names(df)){
    drops_columns <- c("date", "RCH", "AREAkm2", "SUBBASIN", "SETUP", "SC_FOLDER", "SCENARIO")
  } else if ("LULC" %in% names(df)){
    drops_columns <- c("date", "LULC", "HRU", "GIS", "SUB", "AREAkm2", "SUBBASIN", "SETUP", "SC_FOLDER", "SCENARIO")
  }

  if (!is.na(baseline_period)){
    drops_columns <- c(drops_columns, "PERIOD")
  } else if ("RCH" %in% names(df)){
    df <- df %>%
      arrange(date, RCH)
  } else if ("LULC" %in% names(df)){
    df <- df %>%
      arrange(date, HRU)
  }

  ##Looping over non baseline scenarios and calculating %
  #difference with baseline.
  df_save <- NULL
  for (measures_scenario in measures_scenarios){
    if (!is.na(baseline_period)){
      baseline_scenario <- paste0(gsub(".measure_.*","\\1", measures_scenario),
                                  "_baseline_", baseline_period[1], "_", baseline_period[2])
    } else {
      baseline_scenario <- paste0(gsub(".measure_.*","\\1", measures_scenario),
                                  "_baseline")
    }
    ##Calculating percantages
    if (baseline_scenario %in% df$SCENARIO){
      df_ms <- df[df$SCENARIO == measures_scenario,]
      df_bs <- df[df$SCENARIO == baseline_scenario,]
      df_result <- data.frame(df_ms[,(names(df_ms) %in% drops_columns)],
                              round(100 * ((df_ms[,!(names(df_ms) %in% drops_columns)] -
                                              df_bs[,!(names(df_bs) %in% drops_columns)]) /
                                             df_bs[,!(names(df_bs) %in% drops_columns)]),2))
      if(length(df_save) != 0){
        df_save <- bind_rows(df_save, df_result)
      } else {
        df_save <- df_result
      }
    } else {
      message(paste(baseline_scenario, "is not available!!!"))
    }
  }
  return(df_save)
}
