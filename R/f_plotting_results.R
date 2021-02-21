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


#' Function to plot grid of bars plots comparing scenarios of measures during different periods.
#'
#'This function put out modeling results on a grid of bars plots.
#'This grid plot of plots allows to see effectiveness of measures during different time periods.
#'Also allows to compare results of different RCM.
#'
#' @param df Data.frame of imported output.*** SWAT file. Data
#' should be summed to one per scenario and period and showing relative change to
#' baseline.
#' @param param String showing, which parameters of df to put on a plot.
#' @param rcp String showing, which climate change scenario to use (currently only "rcp45" or "rcp85" available)
#' @importFrom cowplot plot_grid get_legend ggdraw draw_label
#' @importFrom ggplot2 theme margin
#' @return Grid of bar plots
#' @export
#' @examples
#' ##plot_sc_grid_bars(df, "NT", "rcp45")

plot_sc_grid_bars <- function(df, param, rcp){
  ##Setting up bar plots for measures and periods
  p1a <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_30_2000_2019"))
  p1b <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_30_2040_2059"))
  p1c <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_30_2080_2099"))
  p2a <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_33_2000_2019"))
  p2b <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_33_2040_2059"))
  p2c <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_33_2080_2099"))
  p3a <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_37_2000_2019"))
  p3b <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_37_2040_2059"))
  p3c <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_37_2080_2099"))
  p4a <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_39_2000_2019"))
  p4b <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_39_2040_2059"))
  p4c <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_39_2080_2099"))
  p5a <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_40_2000_2019"))
  p5b <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_40_2040_2059"))
  p5c <- plot_scenarios_bars(df, param, paste0(rcp ,"_measure_40_2080_2099"))

  ##Making grid plot
  prow <- plot_grid(
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    p1a + theme(legend.position="none"),
    p1b + theme(legend.position="none"),
    p1c + theme(legend.position="none"),
    NULL,
    p2a + theme(legend.position="none"),
    p2b + theme(legend.position="none"),
    p2c + theme(legend.position="none"),
    NULL,
    p3a + theme(legend.position="none"),
    p3b + theme(legend.position="none"),
    p3c + theme(legend.position="none"),
    NULL,
    p4a + theme(legend.position="none"),
    p4b + theme(legend.position="none"),
    p4c + theme(legend.position="none"),
    NULL,
    p5a + theme(legend.position="none"),
    p5b + theme(legend.position="none"),
    p5c + theme(legend.position="none"),
    ##Putting up labels
    labels = c("", "2000 - 2019", "2040 - 2059", "2080 - 2099",
               # "Baseline", "", "", "",
               "Grassland \nconversion", "", "", "",
               "Reduction \nof \nfertilization", "", "", "",
               "No-till \nfarming", "", "", "",
               "Winter \ncover \ncrops", "", "", "",
               "Stubble \nfields in \nwinter", "", "", ""),
    ncol = 4,
    nrow = 6,
    label_x = .5,
    hjust = 0,
    label_size=8,
    rel_heights = c(0.2, 1, 1, 1, 1, 1),
    rel_widths = c(0.9, 1, 1, 1),
    align="hv"
  )
  # extract the legend from one of the plots
  legend <- get_legend(
    # create some space to the left of the legend
    p1a + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  ##Title
  title <- ggdraw() + draw_label(paste(param, "in", rcp, "scenario"), fontface='bold')
  ##Legend
  final_plot <- plot_grid(prow, legend, rel_widths = c(3, .5))
  ##Final
  final_plot <- plot_grid(title, final_plot, ncol=1, rel_heights=c(0.1, 1))
  return(final_plot)
}


#' Function to plot grid of bars plots comparing not baseline periods of baseline scenario..
#'
#'This function put out modeling results on a grid of bars plots.
#'This grid plot of plots allows to see baseline scenario results during not baseline periods.
#'Also allows to compare results of different RCM.
#'
#' @param df Data.frame of imported output.*** SWAT file. Data
#' should be summed to one per scenario and period and showing relative change to
#' baseline.
#' @param param String showing, which parameters of df to put on a plot.
#' @param rcp String showing, which climate change scenario to use (currently only "rcp45" or "rcp85" available)
#' @importFrom cowplot plot_grid get_legend ggdraw draw_label
#' @importFrom ggplot2 theme margin
#' @return Grid of bar plots
#' @export
#' @examples
#' ##plot_baseline_grid_bars(df, "NT", "rcp45")

plot_baseline_grid_bars <- function(df, param, rcp){
  ##Setting up bar plots for measures and periods
  p1a <- plot_scenarios_bars(df, param, paste0(rcp ,"_baseline_2040_2059"))
  p1b <- plot_scenarios_bars(df, param, paste0(rcp ,"_baseline_2080_2099"))

  ##Making grid plot
  prow <- plot_grid(
    p1a + theme(legend.position="none"),
    p1b + theme(legend.position="none"),
    ##Putting up labels
    labels = c("2040 - 2059", "2080 - 2099"),
    ncol = 1,
    nrow = 2,
    label_x = .5,
    hjust = 0,
    label_size=8,
    rel_heights = c(1, 1),
    rel_widths = c(1),
    align="hv"
  )
  # extract the legend from one of the plots
  legend <- get_legend(
    # create some space to the left of the legend
    p1a + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  ##Title
  title <- ggdraw() + draw_label(paste(param, "in", rcp, "scenario"), fontface='bold')
  ##Legend
  final_plot <- plot_grid(prow, legend, rel_widths = c(3, .5))
  ##Final
  final_plot <- plot_grid(title, final_plot, ncol=1, rel_heights=c(0.1, 1))
  return(final_plot)
}

