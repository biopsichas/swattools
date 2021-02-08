#' Function to average output data over some period
#'
#'This function averages output data imported from SWAT output.rch
#'output.sub and output.hru files. Data should have been imported
#'by using extract_watersheds_output function.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @param starting_year Number representing starting year of period (example 2000)
#' @param ending_year Number representing ending year of period (example 2019)
#' @return Data.frame of averaged data over period
#' @importFrom dplyr filter select %>% group_by summarise_all mutate
#' @export

get_period_means <- function(df, starting_year, ending_year){

  #Filtering data frame for the selected years
  df <- df %>% filter(date >= as.Date(ISOdate(starting_year, 1, 1)) &
                        date <= as.Date(ISOdate(ending_year, 12, 31))) %>%
    select(-date)

  ##Identifying inputs to the function
  if ("RCH" %in% names(df)){
    df <- df %>%
      group_by(SUBBASIN, SETUP, SC_FOLDER, SCENARIO, RCH) %>%
      summarise_all(funs(mean)) %>%
      mutate(PERIOD = paste0(as.character(starting_year), "_",
                             as.character(ending_year)))
  } else if ("LULC" %in% names(df)){
    df <- df %>%
      group_by(SUBBASIN, SETUP, SC_FOLDER, SCENARIO, LULC, HRU, GIS, SUB) %>%
      summarise_all(list(~mean)) %>%
      mutate(PERIOD = paste0(as.character(starting_year), "_",
                             as.character(ending_year)))
  } else{
    stop("Input dataframe could not be identified as coming from 'rch', 'sub' or
    'hru' output file!!!")
  }
  df <- within(df,  SCENARIO <- paste(SCENARIO, PERIOD, sep="_"))
  return (df)
}

#' Function to sum up output data over setups
#'
#'This function averages output data imported from SWAT output.rch
#' and output.sub files. Data should have been imported by using
#' extract_watersheds_output function.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @return Data.frame of summed up data over setups
#' @importFrom dplyr select %>% group_by top_n mutate_at mutate_if ends_with vars funs distinct
#' @export

collapse_results_to_setups <- function(df){

  ##Identifying inputs to the function
  if ("FLOW_OUTcms" %in% names(df)){
    df <- df %>%
      group_by(SCENARIO) %>%
      top_n(1, AREAkm2)
  } else if("WYLDmm" %in% names(df)){
    df <- df %>%
      mutate_at(vars(ends_with('mm')), funs(.*AREAkm2*1000)) %>%
      mutate_at(vars(ends_with("kg/ha")), funs(.*AREAkm2*0.1)) %>%
      mutate_at(vars(ends_with("t/ha")), funs(.*AREAkm2*100))

    names(df) <- gsub(x = names(df), pattern = "mm", replacement = "cms/y")
    names(df) <- gsub(x = names(df), pattern = "kg/ha", replacement = "t/y")
    names(df) <- gsub(x = names(df), pattern = "t/ha", replacement = "t/y")

    df <- df %>%
      select(-RCH)
    if ("date" %in% names(df)){
      df <- df %>%
        group_by(SCENARIO, date) %>%
        mutate(across(is.numeric, sum)) %>%
        distinct()
    } else if ("PERIOD" %in% names(df)){
      df <- df %>%
        group_by(SCENARIO, PERIOD) %>%
        mutate(across(is.numeric, sum)) %>%
        distinct()
    }
  } else {
    stop("Dataframe doesn't hold data from 'rch' or 'sub' output files!!!")
  }
  return(df)
}

#' Function to get averaged by periods and collapsed data by setups
#'
#'This function averages output data by periods and collapse it by setups.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @param period_list List of periods used in averaging data  (example
#' period_list <- list (base = c(2000, 2019), mid = c(2040, 2059), end = c(2080, 2099)))
#' @return Data.frame of averaged data over periods and collapsed to setups
#' (it means removing reach information).
#' @importFrom dplyr bind_rows arrange
#' @export

get_averaged_collapsed_data <- function(df, period_list){
  df_save <- NULL
  for (period in period_list){
    df_averaged <- get_period_means(df, period[1], period[2])
    df_collapsed <- collapse_results_to_setups(df_averaged)
    if(length(df_save) != 0){
      df_save <- bind_rows(df_save, df_collapsed)
    } else {
      df_save <- df_collapsed
    }
  }
  df_save <- df_save %>%
    arrange(SCENARIO)
  return(df_save)
}


