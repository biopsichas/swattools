#' Function plot heatmaps to compare results from scenarios
#'
#'This function allows plotting heatmaps for the scenario results extracted and averaged
#'from output.rch and output.sub SWAT output files.
#'
#' @param df Data.frame of data extracted from output.rch and output.sub files. Beside other columns
#' has to have SCENARIO column, which join all information about scenario.
#' @param filter_scenario_by_vector Vector, which allows to filter scenarios. Filtering could be done by AND providing
#' vector members seperated by comma, or by OR seperating one vector member in many '|'. For example
#' c("rcp45", "2080_2099", "Minija|Merkys|Dauguva") this vector filter by AND and by OR.
#' @return Data.frame imported from SWAT output files
#' @importFrom dplyr select filter %>%
#' @importFrom tidyr gather
#' @importFrom scales rescale
#' @importFrom ggplot2 ggplot geom_tile geom_text scale_fill_gradientn theme aes element_text
#' @importFrom stringr str_detect
#' @importFrom tidyselect matches
#' @export

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
