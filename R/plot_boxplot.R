#plotting boxplot for microbiome data - for a single vairable 
#' @title Boxplot for OTU abundance 
#' @description
#' boxplots/violn plots for single OTUs against a column in sample_info
#' @param d microbiomedataset object
#' @param x the Metadata variable to map to the horizontal axis.
#' @param y OTU to map on the vertical axis
#' @param line The variable to map on lines
#' @param violin Use violin version of the boxplot
#' @param na.rm Remove NAs
#' @param show.points Include data points in the figure
#'
#' @return a ggplot2 object
#' @export
plot_boxplot <-
  function(d, x, y, line=NULL, violin=FALSE, na.rm=FALSE, show.points=TRUE) {
    UseMethod("plot_boxplot")}

#' @method plot_boxplot microbiome_dataset
#' @rdname plot_boxplot
#' @import ggplot2
#' @importFrom microbiomedataset extract_expression_data extract_sample_info extract_variable_info
#' @return a ggplot2 object
#' @export
plot_boxplot.microbiome_dataset <- function(object, x, y, line=NULL, violin=FALSE, na.rm=FALSE, show.points=TRUE){
  
  change <- xvar <- yvar <- linevar <- colorvar <- NULL
  
  expression_data <- microbiomedataset::extract_expression_data(object)
  sample_info <- microbiomedataset::extract_sample_info(object)
  variable_info <- microbiomedataset::extract_variable_info(object)
  
  sample_info$xvar <- sample_info[[x]]
  if (!is.factor(sample_info[[x]])) {
    sample_info$xvar <- factor(as.character(sample_info$xvar))
  }
  
  if (y %in% rownames(expression_data)) {
    sample_info$yvar <- as.vector(unlist(expression_data[y, ]))
  } else {
    sample_info$yvar <- as.vector(unlist(sample_info[, y]))
  }
  
  if (na.rm) {
    sample_info <- subset(sample_info, !is.na(xvar))
    sample_info <- subset(sample_info, !is.na(yvar))
  }
  
  if (nrow(sample_info) == 0) {
    warning("No sufficient data for plotting available. 
            Returning an empty plot.")
    return( ggplot2::ggplot())
  }
  
  sample_info$xvar <- factor(sample_info$xvar)
  
  # Visualize example data with a boxplot
  p <- ggplot2::ggplot(sample_info, aes(x=xvar, y=yvar))
  
  if (show.points) {
    p <- p +  ggplot2::geom_point(size=2,
                        position=position_jitter(width=0.3), alpha=0.5)
  }
  
  # Box or Violin plot ?
  if (!violin) {
    p <- p +  ggplot2::geom_boxplot(fill=NA)
  } else {
    p <- p +  ggplot2::geom_violin(fill=NA)
  }
  
  # Add also subjects as lines and points
  if (!is.null(line)) {
    sample_info$linevar <- factor(sample_info[[line]])
    
    # Calculate change directionality
    sample_info2 <- suppressWarnings(sample_info %>%
                              arrange(linevar, xvar) %>%
                              group_by(linevar) %>% 
                              summarise(change=diff(yvar)))
    
    sample_info$change <- sample_info2$change[match(sample_info$linevar, sample_info2$linevar)]
  
    
    sample_info$change <- sign(sample_info$change)
    p <- p + ggplot2::geom_line(data=sample_info,
                       aes(group=linevar, color=change), size=1) +
      ggplot2::scale_colour_gradient2(low="blue", mid="black", high="red", 
                             midpoint=0, na.value="grey50", guide="none")
  }
  
  p <- p +  ggplot2::xlab(x) +  ggplot2::ylab(y)
  
  return(p)
}