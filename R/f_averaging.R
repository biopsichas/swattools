#' Function to average output data over some period
#'
#'This function averages output data imported from SWAT output.rch
#'output.sub and output.hru files. Data should have been imported
#'by using extract_watersheds_output function.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @param starting_year Number representing starting year of period (example 2000)
#' @param ending_year Number representing ending year of period (example 2019)
#' @param data_type One letter sting "y" for yearly or "m" for monthly
#' @return Data.frame of averaged data over period
#' @importFrom dplyr filter select %>% group_by summarise_all mutate
#' @importFrom lubridate month
#' @export

get_period_means <- function(df, starting_year, ending_year, data_type = "y"){

  #Filtering data frame for the selected years
  df <- df %>% filter(date >= as.Date(ISOdate(starting_year, 1, 1)) &
                        date <= as.Date(ISOdate(ending_year, 12, 31))) %>%
    mutate(MONTH = month(date)) %>%
    select(-date)

  ##Identifying inputs to the function
  if ("RCH" %in% names(df)){
    df <- df %>%
      group_by(SUBBASIN, SETUP, SC_FOLDER, SCENARIO, RCH, MONTH) %>%
      summarise_all(mean) %>%
      mutate(PERIOD = paste0(as.character(starting_year), "_",
                             as.character(ending_year)))
  } else if ("LULC" %in% names(df)){
    df <- df %>%
      group_by(SUBBASIN, SETUP, SC_FOLDER, SCENARIO, LULC, HRU, GIS, SUB, MONTH) %>%
      summarise_all(mean) %>%
      mutate(PERIOD = paste0(as.character(starting_year), "_",
                             as.character(ending_year)))
  } else{
    stop("Input dataframe could not be identified as coming from 'rch', 'sub' or
    'hru' output file!!!")
  }
  df <- within(df,  SCENARIO <- paste(SCENARIO, PERIOD, sep="_"))
  if(data_type == "y"){
    df <- df %>%
      select(-MONTH)
  }
  return (df)
}


#' Function to sum up output data over setups
#'
#'This function averages output data imported from SWAT output.rch
#' and output.sub files. Data should have been imported by using
#' extract_watersheds_output function.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @param rch TRUE or FALSE to leave reach numbers for yearly data
#' @return Data.frame of summed up data over setups
#' @importFrom dplyr select %>% group_by top_n mutate_at mutate across ends_with vars funs distinct
#' @export

get_collapsed_results_to_setups <- function(df, rch = FALSE){
  ##Identifying inputs to the function
  ##rch
  if ("FLOW_OUTcms" %in% names(df) & "MONTH" %in% names(df)){
    df <- df %>%
      group_by(SCENARIO, MONTH) %>%
      top_n(1, AREAkm2)
  } else if ("FLOW_OUTcms" %in% names(df)){
    if(rch == FALSE){
      df <- df %>%
        group_by(SCENARIO) %>%
        top_n(1, AREAkm2)
    }
    ##sub
  } else if("WYLDmm" %in% names(df)){
    df <- df %>%
      mutate(across(ends_with('mm'), ~.*AREAkm2*1000)) %>%
      mutate(across(ends_with("kg/ha"), ~.*AREAkm2*0.1)) %>%
      mutate(across(ends_with("t/ha"), ~.*AREAkm2*100))

    names(df) <- gsub(x = names(df), pattern = "mm", replacement = "cms/y")
    names(df) <- gsub(x = names(df), pattern = "kg/ha", replacement = "t/y")
    names(df) <- gsub(x = names(df), pattern = "t/ha", replacement = "t/y")

    if(rch == FALSE){
      df <- df %>%
        ungroup() %>%
        select(-RCH)
      ##raw
      if ("date" %in% names(df)){
        df <- df %>%
          group_by(SCENARIO, date) %>%
          mutate(across(where(is.numeric), sum)) %>%
          distinct()
        ##averaged for periods
      } else if ("PERIOD" %in% names(df) & "MONTH" %in% names(df)){
        df <- df %>%
          group_by(SCENARIO, PERIOD, MONTH) %>%
          mutate(across(where(is.numeric), sum)) %>%
          distinct()
      } else if("PERIOD" %in% names(df)){
        df <- df %>%
          group_by(SCENARIO, PERIOD) %>%
          mutate(across(where(is.numeric), sum)) %>%
          distinct()
      }
      ##Only for yearly to leave RCH for maps.
    } else if (rch == TRUE) {
      df <- df %>%
        ungroup() %>%
        mutate(RCH = as.character(RCH)) %>%
        group_by(SCENARIO, PERIOD, RCH) %>%
        mutate(across(where(is.numeric), sum)) %>%
        distinct()
    }
  } else {
    stop("Dataframe doesn't hold data from 'rch' or 'sub' output files!!!")
  }
  return(df %>% ungroup())
}

#' Function to get averaged by periods and collapsed data by setups
#'
#'This function averages output data by periods and collapse it by setups.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @param period_list List of periods used in averaging data  (example
#' period_list <- list (base = c(2000, 2019), mid = c(2040, 2059), end = c(2080, 2099)))
#' @param data_type One letter sting "y" for yearly or "m" for monthly.
#' @param rch TRUE or FALSE to leave reach numbers for yearly data.
#' @return Data.frame of averaged data over periods and collapsed to setups
#' (it means removing reach information).
#' @importFrom dplyr bind_rows arrange
#' @export

get_averaged_collapsed_data <- function(df, period_list, data_type = "y", rch = FALSE){
  df_save <- NULL
  for (period in period_list){
    df_averaged <- get_period_means(df, period[1], period[2], data_type)
    df_collapsed <- get_collapsed_results_to_setups(df_averaged, rch)
    if(length(df_save) != 0){
      df_save <- bind_rows(df_save, df_collapsed)
    } else {
      df_save <- df_collapsed
    }
  }
  if(data_type == "y"){
    df_save <- df_save %>%
      arrange(SCENARIO)
  } else if (data_type == "m"){
    df_save <- df_save %>%
      arrange(SCENARIO, MONTH)
  }
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
#' @param data_type One letter sting "y" for yearly or "m" for monthly
#' @return Data.frame of summed up over scenarios.
#' @importFrom dplyr filter ungroup mutate across distinct select
#' @importFrom tidyselect everything
#' @export

get_scenario_sub_sums <- function(df, period_list, data_type = "y"){
  ##Getting averaged and collapsed data to the periods and setups
  df <- get_averaged_collapsed_data(df, period_list, data_type) %>%
    ungroup() %>%
    select(SC_FOLDER, PERIOD, which(sapply(.,class)=="numeric"))
  if (data_type == "y"){
    df_r <- df %>%
      group_by(SC_FOLDER, PERIOD) %>%
      mutate(across(is.numeric, sum)) %>%
      distinct() %>%
      mutate(SCENARIO = paste0(SC_FOLDER, "_", PERIOD))
    df_r <- df_r %>%
      ungroup() %>%
      select(SCENARIO, AREAkm2, everything(), -c(SC_FOLDER, PERIOD))
  } else if (data_type == "m"){
    df_r <- df %>%
      group_by(SC_FOLDER, PERIOD, MONTH) %>%
      mutate(MONTH = as.character(MONTH)) %>%
      mutate(across(is.numeric, sum)) %>%
      distinct() %>%
      mutate(SCENARIO = paste0(SC_FOLDER, "_", PERIOD))
    df_r <- df_r %>%
      ungroup() %>%
      select(SCENARIO, MONTH, AREAkm2, everything(), -c(SC_FOLDER, PERIOD))
  }
  return(df_r)
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

#' Function to sum parameters to NT, PT, ST and Q
#'
#'This function sums output data to main parameters. NT - total nitrogen,
#'PT - total phosphorus, ST - total sediment and Q - water discharge.
#'
#' @param df Data.frame of imported output.*** SWAT file
#' @return Data.frame of summed parameters
#' @export

get_main_parameters <- function(df){
  ##Columns not to use in numerical operations.
  drops_columns <- c("SUBBASIN", "SETUP", "SC_FOLDER", "CLIMATE", "MEASURE", "SCENARIO", "AREAkm2",
                     "PERIOD", "RCH", "date", "LULC", "HRU", "GIS", "SUB", "m_AREAkm2", "COSTe_ha")
  ##Calculating main parameters
  df <- data.frame(df[,(names(df) %in% drops_columns)],
                   NT = unlist((df[,grep("^ORGN", colnames(df))] +
                                  df[,grep("^NSURQ", colnames(df))] +
                                  df[,grep("^LATNO3", colnames(df))] +
                                  df[,grep("^GWNO3", colnames(df))] +
                                  df[,grep("^TNO3", colnames(df))])),
                   PT = unlist((df[,grep("^ORGP", colnames(df))] +
                                  df[,grep("^SOLP", colnames(df))] +
                                  df[,grep("^SEDP", colnames(df))])),
                   ST = unlist(df[,grep("^SYLD", colnames(df))]),
                   Q = unlist(df[,grep("^WYLD", colnames(df))]))
  return(df)
}
