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
#' @importFrom dplyr select %>% group_by top_n mutate_at mutate across ends_with vars funs distinct
#' @export

get_collapsed_results_to_setups <- function(df){

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
    df_collapsed <- get_collapsed_results_to_setups(df_averaged)
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

#' Function to get sums of 'sub' data over scenarios
#'
#'This function averages output data by periods and collapse it by setups,
#'and sums it over scenarios for all setups.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @param period_list List of periods used in averaging data  (example
#' period_list <- list (base = c(2000, 2019), mid = c(2040, 2059), end = c(2080, 2099)))
#' @return Data.frame of summed up over scenarios.
#' @importFrom dplyr filter ungroup select_if mutate across distinct select
#' @importFrom tidyselect everything
#' @export

get_scenario_sub_sums <- function(df, period_list){
  ##Getting averaged and collapsed data to the periods and setups
  df <- get_averaged_collapsed_data(df, period_list)
  SC_FOLDER_names <- unique(df$SC_FOLDER)
  PERIOD_names <- unique(df$PERIOD)
  ##Providing vector of scenarios
  scenarios <- as.vector(outer(SC_FOLDER_names, PERIOD_names, paste, sep="_"))
  df_result <- NULL
  for (scenario in scenarios){
    ##Identifying scenario and summing up its values.
    df_selected <- df %>%
      filter(grepl(scenario, SCENARIO)) %>%
      ungroup() %>%
      select_if(~is.numeric(.x)) %>%
      mutate(across(is.numeric, sum)) %>%
      distinct() %>%
      mutate(SCENARIO = scenario)
    ##Saving result
    if (length(df_result) != 0){
      df_result <- bind_rows(df_result, df_selected)
    } else {
      df_result <- df_selected
    }
  }
  df_result <- df_result %>%
    select(SCENARIO, AREAkm2, everything())

  return(df_result)
}


#' Function to get effectiveness of measures over scenarios in kg per ha of implemented measures
#'
#'This function allows comparing scenarios taking into account areas of implemented measures.
#'Output of function is how much each parameters is changed my implementation of 1 ha of measure
#'in scenario for reach or setup or whole country.
#'
#' @param df Data.frame of imported output.*** SWAT file processed with
#'get_diff_from_baseline function by option of 'a' (absolute difference).
#' @return Data.frame of results showing effectiveness of implementing 1 ha of measure.
#' @importFrom dplyr group_by select summarise_all rename mutate left_join mutate_at vars
#' @importFrom tidyr separate
#' @export
#' @examples
#' ## result <- get_measure_effectiveness_kgha(df)

get_measure_effectiveness_kgha <- function(df){
  ##Identifying if data in df are averaged over setups if not.
  if ("RCH" %in% names(df)){
    m_area <- app_areas %>%
      group_by(Subbasin, Setup_name, Measure, RCH)
  } else {
    m_area <- app_areas %>%
      select(-RCH) %>%
      group_by(Subbasin, Setup_name, Measure)
  }
  ##Preparing application areas of measures data
  m_area <- m_area %>%
    summarise_all(sum) %>%
    rename(m_AREAkm2 = AREAkm2)

  ##Adding two missing columns
  df <- df %>%
    separate(SC_FOLDER, into = c("CLIMATE", "MEASURE"), sep = "_mea") %>%
    mutate(MEASURE = paste0("mea", MEASURE))

  ##Adding application arreas of measures data
  if ("RCH" %in% names(df)){
    df <- df %>%
      left_join(m_area, by = c("SUBBASIN" = "Subbasin", "SETUP" = "Setup_name", "MEASURE" = "Measure", "RCH"))
  } else {
    df <- df %>%
      left_join(m_area, by = c("SUBBASIN" = "Subbasin", "SETUP" = "Setup_name", "MEASURE" = "Measure"))
  }

  ##Columns not to use in numerical operations.
  drops_columns <- c("SUBBASIN", "SETUP", "SC_FOLDER", "CLIMATE", "MEASURE", "SCENARIO", "AREAkm2",
                     "PERIOD", "RCH", "date", "LULC", "HRU", "GIS", "SUB", "m_AREAkm2")

  ##Calculating how much is reduced or increased of particular parameter by implementing one ha of measures.
  df <- data.frame(df[,(names(df) %in% drops_columns)],
                   round((df[,!(names(df) %in% drops_columns)] / df[,"m_AREAkm2"]) * 10, 1))

  ##Remove dot, which might appear from division.
  if(any(grepl(".", colnames(df)))){
    names(df) <- gsub(x = names(df), pattern = "\\.", replacement = "/")
  }

  ##Renaming columns.
  df <- df %>%
    mutate_at(vars(matches("cms/y$")),  list(~./1000))
  names(df) <- gsub(x = names(df), pattern = "cms/y", replacement = "cms/ha")
  names(df) <- gsub(x = names(df), pattern = "t/y", replacement = "kg/ha")

  ##Resetting Inf, NAs values to 0.
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-0

  ##Return
  return(df)
}

#' Function to get effectiveness of measures over scenarios in euros per 1kg or 1cms of parameters for implemented measures
#'
#'This function allows comparing scenarios taking into account areas and costs of implemented measures.
#'Output of function is how much  it cost to decrease (negative values) or increase (positive values)
#'each in parameters by one unit (either 1 kg or 1 cubic meter of water) in scenario for reach or
#'setup or whole country.
#'
#' @param df Data.frame of imported output.*** SWAT file processed with
#'get_diff_from_baseline function by option of 'a' (absolute difference).
#' @param cost_list A list of measure names and costs. Example, cost_list <- list(measure_30 = 248,
#' measure_33 = 4, measure_34 = 2, measure_37 = 1, measure_39 = 22, measure_40 = 58)
#' @return Data.frame of results showing cost-effectiveness of implementing measure in euros
#' per one unit (1 kg or 1 cms) of reduction (negative values) or increase (positive values).
#' @importFrom dplyr left_join mutate_at vars
#' @importFrom tidyr gather
#' @export
#' @examples
#' ## result <- get_costs_per_reduction_1unit(df, cost_list)

get_costs_per_reduction_1unit <- function(df, cost_list){
  df_mcost <- data.frame(t(sapply(cost_list,c))) %>%
    gather("MEASURE", "COSTe_ha")
  df <- get_measure_effectiveness_kgha(df) %>%
    left_join(df_mcost, by = "MEASURE")

  ##Columns not to use in numerical operations.
  drops_columns <- c("SUBBASIN", "SETUP", "SC_FOLDER", "CLIMATE", "MEASURE", "SCENARIO", "AREAkm2",
                     "PERIOD", "RCH", "date", "LULC", "HRU", "GIS", "SUB", "m_AREAkm2", "COSTe_ha")

  ##Calculating how much is reduced or increased of particular parameter by implementing one ha of measures.
  df <- data.frame(df[,(names(df) %in% drops_columns)],
                   round((df[,"COSTe_ha"] / df[,!(names(df) %in% drops_columns)]), 3))

  ##Remove dot, which might appear from division.
  if(any(grepl(".", colnames(df)))){
    names(df) <- gsub(x = names(df), pattern = "\\.", replacement = "/")
  }

  ##Renaming columns.
  df <- df %>%
    mutate_at(vars(matches("cms/y$")),  list(~./1000))
  names(df) <- gsub(x = names(df), pattern = "kg/ha", replacement = "e/1kg")
  names(df) <- gsub(x = names(df), pattern = "cms/ha", replacement = "e/1cms")

  ##Resetting Inf, NAs values to 0.
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-0

  return(df)
}
