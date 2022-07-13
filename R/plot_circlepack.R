#' @title Barplot based on ggplot2
#' @param object microbiome_dataset
#' @param label_level label levels
#' @param weight intensity or log_intensity
#' @param ... other params
#' @return ggplot2 class object
#' @export

plot_circlepack <-
  function(object,
           label_level = "Class",
           weight = c("intensity", "log_intensity"),
           ...) {
    UseMethod("plot_circlepack")
  }

#' @method plot_circlepack microbiome_dataset
#' @rdname plot_circlepack
#' @export
#' library(microbiomeplot)
#' data("global_patterns")
#' global_patterns %>%
#'   activate_microbiome_dataset(what = "variable_info") %>%
#'   filter(Kingdom == "Archaea") %>%
#'   plot_circlepack(label_level = "Class")

plot_circlepack.microbiome_dataset <-
  function(object,
           label_level = "Kingdowm",
           weight = c("log_intensity", "intensity"),
           ...) {
    label_level <-
      stringr::str_to_title(label_level)
    weight <- match.arg(weight)
    
    graph <-
      object %>%
      microbiomedataset::activate_microbiome_dataset(what = "variable_info") %>%
      # filter(Kingdom == "Archaea") %>%
      microbiomedataset::convert2tbl_graph()
    
    graph <-
      graph %>%
      tidygraph::activate(what = "nodes") %>%
      dplyr::filter(intensity > 0 & node != "Root") %>%
      dplyr::mutate(log_intensity = log(intensity + 1, 2)) %>%
      dplyr::filter(intensity > 0) %>%
      dplyr::mutate(
        taxa_rank =
          case_when(
            stringr::str_detect(node, "k__") ~ "Kingdom",
            stringr::str_detect(node, "p__") ~ "Phylum",
            stringr::str_detect(node, "c__") ~ "Class",
            stringr::str_detect(node, "o__") ~ "Order",
            stringr::str_detect(node, "f__") ~ "Family",
            stringr::str_detect(node, "g__") ~ "Genus",
            stringr::str_detect(node, "s__") ~ "Species"
          )
      ) %>%
      dplyr::mutate(taxa_rank = factor(
        taxa_rank,
        levels = c(
          "Kingdom",
          "Phylum",
          "Class",
          "Order",
          "Family",
          "Genus",
          "Species"
        )
      ))
    
    graph <-
      graph %>%
      tidygraph::activate(what = "nodes") %>%
      dplyr::mutate(label = dplyr::case_when(as.character(taxa_rank) == label_level ~ node)) %>%
      dplyr::mutate(label = stringr::str_replace(label, "[a-z]{1}__", ""))
    
    if (weight == "intensity") {
      plot <-
        ggraph::ggraph(graph, layout = 'circlepack', weight = intensity)
    } else{
      plot <-
        ggraph::ggraph(graph, layout = 'circlepack', weight = log_intensity)
    }
    
    plot <-
      plot +
      ggraph::geom_node_circle(aes(fill = taxa_rank),
                               size = 0.25, n = 50) +
      ggraph::geom_node_label(aes(label = label), size = 4, repel = TRUE) +
      scale_fill_manual(
        values = c(
          "Kingdom" = viridis::viridis(10)[1],
          "Phylum" = viridis::viridis(10)[2],
          "Class" = viridis::viridis(10)[3],
          "Order" = viridis::viridis(10)[4],
          "Family" = viridis::viridis(10)[5],
          "Genus" = viridis::viridis(10)[6],
          "Species" = viridis::viridis(10)[7]
        )
      ) +
      coord_fixed() +
      ggraph::theme_graph()
    return(plot)
  }
