#' @title Plotting a PCA plot for microbiome data
#'
#' @param object microbiome_dataset object 
#' @param scale scaling of the variables 
#' @param grad The column in the sample_info that the data_points to be labelled
#' @param pc_x Principle Component 1
#' @param pc_y Principle Component 2
#' @param size size of the labels in the graph
#' @param legend legend for the plot
#' @param title title of the plot
#' @param theme theme of the plot 
#' @param legend.position position of the legend
#' @param scree.plot scree plot of the PCs
#' @param .. 
#'
#' @export
plot_PCA <- function(object, scale = TRUE, grad, pc_x = 1, pc_y = 2, size = 2, 
                     legend = TRUE, title = "PCA Plot for Microbiome Data", theme = "minimal", legend.position = "bottom", scree.plot = FALSE, ..){
  UseMethod("plot_PCA")
}

#' @rdname convert2phyloseq
#' @export
plotPCA <- plot_PCA


#' @method plot_PCA microbiome_dataset
#' @rdname plot_PCA
#' @import ggplot2
#' @importFrom stats prcomp
#' @importFrom microbiomedataset  transform2relative_intensity extract_expression_data extract_sample_info
#' @importFrom cowplot plot_grid
#' @importFrom dplyr left_join
#' @export
plot_PCA.microbiome_dataset <- 
  function(object , 
           scale = TRUE, 
           grad, pc_x = 1, 
           pc_y = 2, size = 2, legend = TRUE, title = "PCA Plot for Microbiome Data", theme = "minimal", legend.position = "bottom", scree.plot = FALSE, ..){

  
  #extract the expression data
  expression_data <-
    microbiomedataset::extract_expression_data(object)
  
  #handling zero columns
  zero_var_rows <- apply(expression_data, 1, function(x) var(x, na.rm = TRUE) == 0)
  
  if (any(zero_var_rows)) {
    warning("Removing ", sum(zero_var_rows), " constant columns with zero variance.")
    expression_data <- expression_data[!zero_var_rows, ]
  }
  
  # Step 4: Transpose the data (samples as rows, features as columns)
  transposed_data <- t(expression_data)

  
  #extract sample_info for coloring the datapoints 
  sample_info <- 
    microbiomedataset::extract_sample_info(object)
  
  #Perform PCA (reduction of the dimensions)
  pca_result <- 
    stats::prcomp(transposed_data, scale = scale)
  
  #extract the variance explained by each component
  variance_explained <- 
    round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 2)
  
  #Check if the input for the PCs are valid
  
  if (pc_x > ncol(pca_result$x) || 
      pc_y > ncol(pca_result$x)){
    stop("Selected principal components are out 
         of range. The maximum number of PCs is", 
         ncol(pca_result$x))
  }
  if (pc_x < 0 || pc_y < 0){
    stop("Selected principal components cannot be negative.
         Please enter a valid number")
  }
  
#Creating a dataframe for plotting of the pca scores
pca_scores <- as.data.frame(pca_result$x)
pca_scores$sample_id <- rownames(pca_scores)
pca_scores <- dplyr::left_join(pca_scores, sample_info, by = "sample_id")

theme_function <- switch(
  theme,
  "minimal" = ggplot2::theme_minimal,
  "classic" = ggplot2::theme_classic,
  "bw" = ggplot2::theme_bw,
  "light" = ggplot2::theme_light,
  "dark" = ggplot2::theme_dark,
  stop("Invalid theme name. Choose from: 'minimal', 'classic', 'bw', 'light', 'dark'")
)

pca_plot <- ggplot2::ggplot(pca_scores, 
                            ggplot2::aes(x = !!sym(paste0("PC", pc_x)), 
                                         y = !!sym(paste0("PC", pc_y)), 
                                         colour = !!sym(grad))) + 
  ggplot2::geom_point(size = size) + 
  ggplot2::geom_text(ggplot2::aes(label = !!sym(grad)), 
                     hjust = 1.5, vjust = 1.5, size = size) + 
  ggplot2::labs(title = title, 
                x = paste0("PC", pc_x, " (", variance_explained[pc_x], "%)"), 
                y = paste0("PC", pc_y, " (", variance_explained[pc_y], "%)"), 
                colour = grad) + 
  theme_function()



if (!legend) {
  pca_plot <- pca_plot + ggplot2::theme(legend.position = "none")
} else {
  pca_plot <- pca_plot + ggplot2::theme(legend.position = legend.position)
}

if (scree.plot == TRUE){
  scree_data <- data.frame(PC = 1:length(variance_explained), 
                           Variance = variance_explained)
  scree_plot <- ggplot2::ggplot(scree_data, ggplot2::aes(x = PC, y = Variance)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") + 
    ggplot2::labs(title = "Scree Plot", 
                  x = "Principal Component", 
                  y = "Variance Explained (%)") +
    theme_function() 
  
  combined_plot <- cowplot::plot_grid(pca_plot, scree_plot, ncol = 2)
  
  return(combined_plot)
}
else{
  return(pca_plot)
}
                                               
}

