#' Function to get scenario difference from baseline values
#'
#'This function calculates percentage difference comparing scenario
#'with its baseline. It could be used with raw or averaged data.
#'
#' @param df Data.frame of imported output.*** SWAT file with extract_watersheds_output function
#' @param baseline_period Vector of two values. First with year of beginning
#' @param type_of_diff String from one letter. 'r' stands for relative difference in percentages
#' #' and 'a' for absolute in absolute values.
#' of baseline period and second with end of it. Those two values should have
#' been used in naming scenario folder. Example (c("2000", "2019"))
#' @param type_of_baseline String from one letter. 'f' stands for fixed period difference and "c"
#' for changing. It means "f" would calculate all the difference with one period (example 2000-2019) baseline
#' and "c" with each period baselines (example 2000-2019, 2040-2059, 2080-2099)
#' @return Data.frame with all numeric columns recalculated as percentage from baseline.
#' @importFrom dplyr arrange bind_rows %>%
#' @export
#' @examples
#' ##get_diff_from_baseline(df, NULL, "r", "c")

get_diff_from_baseline <- function(df, baseline_period = NULL, type_of_diff = "r", type_of_baseline = "f"){

  ##Getting all different outputs available and separating them into baselines
  ## scenarios and non baseline scenarios
  scenarios <- unique(df$SCENARIO)
  if (length(baseline_period) != 0){
    base <- paste0("_baseline_", baseline_period[1], "_", baseline_period[2], "$")
    baseline_scenarios <- scenarios[grep(base, scenarios)]
    measures_scenarios <- scenarios[-grep(base, scenarios)]
  } else {
    baseline_scenarios <- scenarios[grep("_baseline", scenarios)]
    measures_scenarios <- scenarios[-grep("_baseline", scenarios)]
  }

  ##Identifying inputs to the function
  if ("LULC" %in% names(df)){
    drops_columns <- c("date", "LULC", "HRU", "GIS", "SUB", "AREAkm2", "SUBBASIN", "SETUP", "SC_FOLDER", "SCENARIO")
  } else {
    drops_columns <- c("date", "RCH", "AREAkm2", "SUBBASIN", "SETUP", "SC_FOLDER", "SCENARIO")
  }

  if (length(baseline_period) != 0){
    drops_columns <- c(drops_columns, "PERIOD")
  } else if ("PERIOD" %in% names(df)){
    drops_columns <- c(drops_columns[-1], "PERIOD")
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
    if (type_of_baseline == "f"){
      ##This is for measures scenarios
      if (grepl("measure", measures_scenario)){
        if (length(baseline_period) != 0){
          baseline_scenario <- paste0(gsub(".measure_.*","\\1", measures_scenario),
                                      "_baseline_", baseline_period[1], "_", baseline_period[2])
        } else {
          ##This part for scenarios without period averaging
          baseline_scenario <- paste0(gsub(".measure_.*","\\1", measures_scenario),
                                      "_baseline")
        }
        ##This part for baseline scenarios, but not in main baseline period
      } else if (grepl("baseline", measures_scenario)){
        if (length(baseline_period) != 0){
          baseline_scenario <- paste0(gsub(".baseline_.*","\\1", measures_scenario),
                                      "_baseline_", baseline_period[1], "_", baseline_period[2])
        } else {
          ##This part for scenarios without period averaging
          baseline_scenario <- paste0(gsub(".baseline_.*","\\1", measures_scenario),
                                      "_baseline")
        }
      }
      ##Part for changing baseline periods
    } else if(type_of_baseline == "c"){
      baseline_scenario <- gsub("measure_[1-9][0-9]","baseline", measures_scenario)
    }
    ##Calculating percentages
    if (baseline_scenario %in% df$SCENARIO){
      df_ms <- df[df$SCENARIO == measures_scenario,]
      df_bs <- df[df$SCENARIO == baseline_scenario,]
      ##Setting if we calculate relative of absolute difference
      if (type_of_diff == "r"){
        df_result <- data.frame(df_ms[,(names(df_ms) %in% drops_columns)],
                                round(100 * ((df_ms[,!(names(df_ms) %in% drops_columns)] -
                                                df_bs[,!(names(df_bs) %in% drops_columns)]) /
                                               df_bs[,!(names(df_bs) %in% drops_columns)]),2))
      } else if(type_of_diff == "a"){
        df_result <- data.frame(df_ms[,(names(df_ms) %in% drops_columns)],
                                (df_ms[,!(names(df_ms) %in% drops_columns)] -
                                   df_bs[,!(names(df_bs) %in% drops_columns)]))
      } else {
        stop("Type of difference is not deffined in the right way. It should be either 'r' for relative or 'a'
              for absolute difference between measure and baseline scenarios.!!!")
      }
      if(length(df_save) != 0){
        df_save <- bind_rows(df_save, df_result)
      } else {
        df_save <- df_result
      }
    } else {
      message(paste(baseline_scenario, "is not available!!!"))
    }
  }
  ##Removing any dots in column names if they are there.
  if(any(grepl(".", colnames(df_save)))){
    names(df_save) <- gsub(x = names(df_save), pattern = "\\.", replacement = "/")
  }
  return(df_save)
}
