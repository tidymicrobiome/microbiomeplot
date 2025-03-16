#' @title Plotting a Pie chart for the global composition of all samples
#' @param object microbiomedataset object
#' @param tax_level the level of taxa to fill
#' @param top_n default is 10
#' @param ... other parameters 
#'
#' @return a ggplot2 object
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
#' @return a ggplot2 object
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
  
  
  
  
  
  
  
  
  
  

  
  
  
  
