#' @title Plotting a t-SNE plot for visualizing high-dimensional microbiome data
#' @param object the microbiomedataset object
#' @param legend legend for the plot 
#' @param title Title of the plot
#' @param theme theme for the ggplot 
#' @param legend.position position of the legend 
#' @param size size of the labels 
#' @param .. 
#' @return A ggplo2 class object
#' 
#' @export
plot_tSNE <- function(object, fill, dims = 2,  
                       perplexity = 30, number_of_iteration = 1000, initial_dims = 50, pca = TRUE, 
                       legend = TRUE, title = "t-SNE plot for Microbiome Data",
                       x_title = "axis 1", y_title = "axis 2", theme = "minimal", legend.position = "bottom", 
                       size = 3, title_size = 10, ..){
  UseMethod("plot_tSNE")
}


#' @method plot_tSNE microbiome_dataset
#' @rdname plot_tSNE
#' @import ggplot2 
#' @import Rtsne
#' @importFrom microbiomedataset extract_expression_data
#' @export
plot_tSNE.microbiome_dataset  <- function(object, fill, dims = 2,  
                                         perplexity = 30, number_of_iters = 1000, initial_dims = 50, pca = TRUE, 
                                         legend = TRUE, title = "t-SNE plot for Microbiome Data",
                                         x_title = "axis 1", y_title = "axis 2", theme = "minimal", legend.position = "bottom", 
                                         size = 3, title_size = 10, ..){
  
  expression_data <- microbiomedataset::extract_expression_data(object)
  sample_info <- microbiomedataset::extract_sample_info(object)
  fill_new = sample_info[fill]
  
  expression_data <- t(expression_data)
  
  tSNE_object <- Rtsne(expression_data, initial_dims = initial_dims, dims = dims, perplexity = perplexity, verbose = TRUE, max_iter = number_of_iters)
  
  theme_function <- switch(
    theme,
    "minimal" = ggplot2::theme_minimal,
    "classic" = ggplot2::theme_classic,
    "bw" = ggplot2::theme_bw,
    "light" = ggplot2::theme_light,
    "dark" = ggplot2::theme_dark,
    stop("Invalid theme name. Choose from: 'minimal', 'classic', 'bw', 'light', 'dark'")
  )
  
  
  tsne_df <- data.frame(
        X = tSNE_object$Y[, 1],
         Y = tSNE_object$Y[, 2],
        fill = fill_new)
  
  
  tsne_plot <- ggplot2::ggplot(tsne_df, ggplot2::aes(x = X, y = Y, colour = factor(fill_new))) + 
    labs(title = title, x = x_title, y = y_title) + 
    theme_function() + 
    theme(plot.title = element_text(size = title_size))
  
  tsne_plot <- tsne_plot + scale_color_viridis_d()  # Use a discrete color scale
  
        
  return(tsne_plot)
  
}
      
                  
                  
                  
                  
                  
                  
                  
                                            
  
  

