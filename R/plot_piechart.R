#setwd(r4projects::get_project_wd())
# plot_piechart(global_patterns, tax_level = "Genus")

#' Create a Pie Chart of Taxonomic Composition
#' 
#' @title Create a Pie Chart of Taxonomic Composition
#' @description Generates a pie chart visualizing the global taxonomic composition
#'   across all samples in a microbiome dataset. The function aggregates abundance 
#'   data at the specified taxonomic level and displays the top contributors.
#'
#' @param object A microbiomedataset object containing taxonomic abundance data.
#' @param tax_level The taxonomic level to aggregate and display. Must be one of:
#'   "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", or "Species".
#' @param top_n Integer specifying the number of top taxa to display individually.
#'   Taxa beyond this number are grouped as "Other". Default is 10.
#' @param theme nteger specifying the number of top taxa to display individually.
#'   Taxa beyond this number are grouped as "Other". Default is 10.
#' @param table Logical indicating whether to include a data table alongside 
#'   the pie chart (TRUE) or return only the pie chart (FALSE). Default is TRUE.
#' @param ... Additional parameters passed to internal functions.
#'
#' @return If table=TRUE, returns a grid.arrange object containing both the 
#'   pie chart and a table of percentages. If table=FALSE, returns a ggplot2 object.
#' @examples
#' \dontrun{
#' # Create a pie chart of the top 5 phyla
#' plot_piechart(microbiome_data, tax_level = "Phylum", top_n = 5)
#' 
#' # Create a pie chart with a different theme and no table
#' plot_piechart(microbiome_data, tax_level = "Genus", theme = "classic", table = FALSE)
#' }
#' @seealso \code{\link{extract_expression_data}}, \code{\link{extract_variable_info}}
#' @export

plot_piechart <- function(object, tax_level = c("Kingdom",
                                                "Phylum",
                                                "Class",
                                                "Order",
                                                "Family",
                                                "Genus",
                                                "Species"), top_n = 10, theme = "void", table = TRUE, ...){
  UseMethod("plot_piechart")
}

#' @method plot_piechart microbiome_dataset
#' @rdname plot_piechart
#' @import ggplot2
#' @importFrom dplyr group_by summarise ungroup arrange mutate across
#' @importFrom tidyr drop_na
#' @importFrom rlang sym
#' @importFrom gridExtra tableGrob grid.arrange ttheme_minimal
#' @export

plot_piechart.microbiome_dataset <- function(object, tax_level = c("Kingdom",
                                                                   "Phylum",
                                                                   "Class",
                                                                   "Order",
                                                                   "Family",
                                                                   "Genus",
                                                                   "Species"), top_n = 10, theme = "void",  table = TRUE, ...){
  
  tax_level <- match.arg(tax_level)
  
  #extract the expression data and variable info table for the taxonomy groups
  expression_data <- microbiomedataset::extract_expression_data(object)
  variable_info <- microbiomedataset::extract_variable_info(object)
  
  #merge the variable info table and the expression data
  merged_data <- cbind(expression_data, variable_info)
  
  #identify the sample columns 
  sample_columns <- setdiff(colnames(merged_data), colnames(variable_info))
  
  aggregated_data <- merged_data %>% group_by(!!sym(tax_level)) %>% summarise(across(all_of(sample_columns), sum, na.rm = TRUE)) %>% ungroup()
  total_abundance <- rowSums(aggregated_data[, sample_columns, drop = FALSE])
  
  aggregated_data$total_abundance <- total_abundance
  
  # Sort and select the top N groups
  top_groups <- aggregated_data %>%
    arrange(desc(total_abundance)) 
  
  #slice the top_groups based on the top_n parameter
  top_groups <- top_groups[1:top_n, ]
  top_groups <- top_groups[, c(tax_level, "total_abundance")] 
  
  # Combine the rest into "Other"
  other_abundance <- sum(aggregated_data$total_abundance) - sum(top_groups$total_abundance)
  
  #Group the remaning groups in other
  new_row <- setNames(data.frame("Other", other_abundance), c(tax_level, "total_abundance"))
  top_groups <- rbind(top_groups, new_row)
  
  #adding percentage to the top_groups 
  top_groups <- top_groups %>%
    mutate(percentage = round(total_abundance / sum(total_abundance) * 100))
  
  top_groups <- top_groups %>% drop_na()
  
  #plotting the piechart
  theme_function <- switch(
    theme,
    "minimal" = ggplot2::theme_minimal,
    "classic" = ggplot2::theme_classic,
    "bw" = ggplot2::theme_bw,
    "light" = ggplot2::theme_light,
    "dark" = ggplot2::theme_dark,
    "void" = ggplot2::theme_void,
    stop("Invalid theme name. Choose from: 'minimal', 
         'classic', 'bw', 'light', 'dark', 'void ")
  )
  
  # Create a pie chart using ggplot2
  pie_chart <- ggplot2::ggplot(top_groups, aes(x = "", y = total_abundance, fill = !!sym(tax_level))) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    theme_function() +
    ggplot2::labs(title = paste("Global Composition by", tax_level),
         fill = tax_level) 
  
  if (table == TRUE){
    pie_chart <- pie_chart +
      theme(legend.position = "bottom")
    
    top_groups <- top_groups %>%
      mutate(percentage = round(total_abundance / sum(total_abundance) * 100, 3))
    
    table_grob <- tableGrob(
      top_groups[, c(tax_level, "percentage")], 
      theme = ttheme_minimal(
        base_size = 7  # Reduce the font size (default is 12)
      )
    )
    
    grid.arrange(
      pie_chart, 
      table_grob, 
      ncol = 2, 
      widths = c(2, 1) # Pie chart takes 3/4 of the width, table takes 1/4
    
    )
    
    
    
  }else{
              
    return(pie_chart)
  }
}
  
  
  
  
  
  
  
  
  
  

  
  
  
  
