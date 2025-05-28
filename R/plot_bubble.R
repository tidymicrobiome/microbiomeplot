#plotting a bubble plot



#' @title Bubble for Microbiome Composition
#' @description Bubble plots are another way to visualize compositional microbial community data 
#' (other than a stacked bar chart). In a bubble plot the relative abundance of a species in a 
#' sample is scaled to match the size of the corresponding point.
#'
#' @param object  microbiomedataset object containing taxonomic abundance data.
#' @param top_n nteger specifying the number of top taxa to display individually.
#'   Taxa beyond this number are grouped as "Other". Default is 10.
#' @param theme nteger specifying the number of top taxa to display individually.
#'   Taxa beyond this number are grouped as "Other". Default is 10.
#' @param fill fill for bubble plot
#' @param color_palette the color palatte for bubble to be colored by
#' @param x sample_id
#' @param extract_intensity_by the parameter by which is intensity is extracted by
#' @param what what kind of intensity
#' @param na.rm remove NAs
#' @param relative Relative intensity
#' @param title_x title of x-axis
#' @param title_y title of y-axis
#' @param ... 
#'
#' @return a ggplot2 object
#' @examples
#' plot_bubble(global_patterns)
plot_bubble <- function(object, fill = c("Kingdom",
                                                "Phylum",
                                                "Class",
                                                "Order",
                                                "Family",
                                                "Genus",
                                                "Species"), top_n = 10, theme = "void", bubble_scale = 1, color_palette = "Set3",  x = "sample_id",
                        extract_intensity_by = "sample_id", what = c("sum_intensity",
                                                       "mean_intensity",
                                                       "median_intensity"),na.rm = TRUE, relative = TRUE, title_x = "sample_id", title_y = "Relative Abundance",...){
  UseMethod("plot_bubble")
}


#' @method plot_bubble microbiome_dataset
#' @rdname plot_bubble
#' @importFrom microbiomedataset extract_intensity check_microbiome_dataset_class
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' plot_bubble(global_patterns)
plot_bubble.microbiome_dataset <- function(object, fill = c("Kingdom",
                                                                 "Phylum",
                                                                 "Class",
                                                                 "Order",
                                                                 "Family",
                                                                 "Genus",
                                                                 "Species"), top_n = 10, theme = "void", bubble_scale = 1, color_palette = "Set3", x = "sample_id",
                                           extract_intensity_by = "sample_id", what = c("sum_intensity",
                                                                          "mean_intensity",
                                                                          "median_intensity"), na.rm = TRUE, relative = TRUE, title_x = "sample_id", title_y = "Relative Abundance",...){
  
  
  fill = match.arg(fill)
  what = match.arg(what)
  
if (microbiomedataset::check_microbiome_dataset_class(object)){
  
  intensity <-
    extract_intensity(
      object = object,
      sample_wise = extract_intensity_by,
      taxonomic_rank = fill,
      data_type = "longer",
      what = what,
      na.rm = na.rm,
      relative = relative
    )
  
  colnames(intensity)[1] <- "sample_id"
  
  intensity <-
    intensity %>%
    dplyr::group_by(sample_id) %>%
    dplyr::slice_max(order_by = value, n = top_n) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    as.data.frame()

  
  plot = ggplot(intensity, aes(x = !!sym(x), y = value, fill = !!sym(fill))) + geom_point(aes(size = value, fill = !!sym(fill)), alpha = 0.75, shape = 21) + theme(legend.key=element_blank(), 
                                                                                                                                                      axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1), 
                                                                                                                                              axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
                                                                                                                                                      legend.text = element_text(size = 10, face ="bold", colour ="black"), 
                                                                                                                                                      legend.title = element_text(size = 12, face = "bold"),                                                                                                                                                 panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
                                                                                                                                                      legend.position = "right") + labs(x = title_x, y = title_y)
  
  return(plot)
}
}

  
  
