#' Function plot heatmaps to compare results from scenarios
#'
#'This function allows plotting heatmaps for the scenario results extracted and averaged
#'from output.rch and output.sub SWAT output files.
#'
#' @param df Data.frame of data extracted from output.rch and output.sub files and processed with get_diff_from_baseline function.
#' Beside other columns has to have SCENARIO column, which join all information about scenario.
#' @param filter_scenario_by_vector Vector, which allows to filter scenarios. Filtering could be done by AND providing
#' vector members seperated by comma, or by OR seperating one vector member in many '|'. For example
#' c("rcp45", "2080_2099", "Minija|Merkys|Dauguva") this vector filter by AND and by OR.
#' @return heatmap plot
#' @importFrom dplyr select filter %>%
#' @importFrom tidyr gather
#' @importFrom scales rescale
#' @importFrom ggplot2 ggplot geom_tile geom_text scale_fill_gradientn theme aes element_text
#' @importFrom stringr str_detect
#' @importFrom tidyselect matches
#' @export
#' @examples
#' ##plot_scenarios_heatmap(df, c("measure_30", "rcp45", "2080_2099", "Daugyvene"))

plot_scenarios_heatmap <- function(df, filter_scenario_by_vector = NA){
  if (!is.na(filter_scenario_by_vector)){
    for (pattern in filter_scenario_by_vector){
      df <- df %>%
        filter(str_detect(SCENARIO, pattern))
    }
  }
  df <- df %>%
    select(SCENARIO, matches("cms$|kg$|tons$|mm$|ha$|cms/y$|t/y$")) %>%
    gather(PARAMETER, DIFFERENCE, -SCENARIO)

  heatmap_plot <- ggplot(df, aes(x = PARAMETER, y = SCENARIO)) +
    geom_tile(aes(fill= DIFFERENCE)) +
    geom_text(aes(label = round(DIFFERENCE, 0))) +
    scale_fill_gradientn(colours = c("darkslateblue", "deepskyblue1", "darkolivegreen1",
                                     "grey95",
                                     "gold", "darkorange", "red"),
                         values = rescale(c(-100, -50, -25,
                                            0,
                                            25, 50, 100)),
                         breaks = seq(-100, 100, 20),
                         limits = c(-100, 100),
                         guide = "colorbar") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(heatmap_plot)
}

#' Function plot bar plots to compare results from scenarios
#'
#'This function allows plotting bar plots for the scenario results extracted and averaged
#'from SWAT output files.
#'
#' @param df Data.frame of data extracted from output.rch and output.sub files and processed with get_diff_from_baseline function.
#' Beside other columns has to have SCENARIO column, which join all information about scenario.
#' @param variable_name Sting with the name of column to plot (example "NSURQt/y")
#' @param filter_vec Vector, which allows to filter scenarios. Filtering could be done by AND providing
#' vector members seperated by comma, or by OR seperating one vector member in many '|'. For example
#' c("measure_40", "rcp45|rcp85", "2080_2099") this vector filter by AND and by OR.
#' @return Bar plot
#' @importFrom dplyr select filter rename %>%
#' @importFrom ggplot2 ggplot geom_bar theme_gray aes element_blank
#' @export
#' @examples
#' ##plot_scenarios_bars(df, "NSURQt/y", c("measure_40", "rcp45|rcp85", "2080_2099"))

plot_scenarios_bars <- function(df, variable_name, filter_vec = c("_")){
  ##Getting scenario and parameter
  df <- df %>%
    select(SCENARIO, !!variable_name) %>%
    rename(`Difference %` = 2)
  ##Filtering for vector
  for (vc in filter_vec){
    df <- df %>%
      filter(grepl(vc, SCENARIO)) %>%
      mutate(RCM = sub("_.*", "", SCENARIO))
  }
  ##Plotting
  plot <- ggplot(df, aes(`Difference %`, SCENARIO, fill = RCM)) +
    geom_bar(stat='identity') +
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  return(plot)
}

#' Function to plot on map modeling results of scenario on basins polygons
#'
#'This function put out modeling results on basins polygons used
#'in modeling. However only one scenario could be plotted. Also data input
#'should be summed up/averaged to one value for reach or setup/
#'
#' @param df Data.frame of imported output.*** SWAT file. Data
#' should be summed up/averaged to only one per scenario per reach or setup.
#' @param pattern String neccessary to filter out one particural scenario.
#' @param parameter String showing, which parameters of df to put on a map.
#' @importFrom dplyr filter left_join
#' @importFrom stringr str_detect
#' @importFrom ggplot2 ggplot scale_fill_viridis_c xlab ylab labs theme_minimal aes_string geom_sf
#' @return Map for modeling results on basins polygons
#' @export
#' @examples
#' ##plot_scenario_result_map(df, "RACMO22E_rcp45_measure_33_2040_2059", "NT")

plot_scenario_result_map <- function(df, pattern, parameter){
  ##Filter right scenario
  df <- df %>%
    filter(str_detect(SCENARIO, pattern))
  ##Identify if results should be plotted on reaches of setups
  if ("RCH" %in% names(df)){
    b_df <- basins %>%
      left_join(df, by = c("Subbasin" = "SUBBASIN", "Setup_name" = "SETUP", "Reach" = "RCH"))
  } else {
    b_df <- basins %>%
      left_join(df, by = c("Subbasin" = "SUBBASIN", "Setup_name" = "SETUP"))
  }
  ##Creating map
  plot <- ggplot(data = b_df) +
    geom_sf(aes_string(fill = parameter)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(fill = "Values") +
    theme_minimal()

  return(plot)
}

