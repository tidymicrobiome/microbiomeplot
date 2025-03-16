#' @title Plotting a t-SNE plot for visualizing high-dimensional microbiome data
#'
#' @param object the microbiomedataset object
#' @param legend string; legend for the plot 
#' @param title Title of the plot
#' @param theme The theme of the ggplot to be displayed
#' - These themes include:
#'    - theme_minimal
#'    - theme_classic
#'    - theme_bw
#'    - theme_light
#'    - theme_dark
#' @param legend.position position of the legend ("bottom", "top", "right", "left")
#' @param size numeric; size of the labels (default: 2)
#' @param grad the column of the sample_info to be colored by (no default)
#' @param dims the output dimensions of the t-sne object (default: 2)
#' @param perplexity A parameter that controls the balance between local and global aspects of the data when t-SNE is creating the low-dimensional map 
#' @param number_of_iters numeric; Number of iterations (default: 1000)
#' @param initial_dims numeric; the number of dimensions that should be retained in the initial PCA step
#' @param pca logical; Whether an initial PCA step should be performed (default: TRUE)
#' @param x_title The x-axis title of the plot 
#' @param y_title the y-axis title of the plot
#' @param title_size the size of the text for the title
#' @param z_title  the z-axis title of the plot
#' @param .. other parameters 
#'
#' @return A ggplot2 class object
#' @export
plot_tSNE <- function(object, grad, dims = 2,  
                      perplexity = NULL , number_of_iters = 1000, initial_dims = 50, pca = TRUE, 
                       legend = TRUE, title = "t-SNE plot for Microbiome Data",
                       x_title = "dimension 1", y_title = "dimension 2", z_title = "dimension 3", theme = "minimal", legend.position = "bottom", 
                       size = 3, title_size = 10, ..){
  UseMethod("plot_tSNE")
}


#' @method plot_tSNE microbiome_dataset
#' @rdname plot_tSNE
#' @import ggplot2 
#' @importFrom Rtsne Rtsne
#' @importFrom microbiomedataset extract_expression_data extract_sample_info
#' @importFrom plotly plot_ly
#' @export
#' @examples
#' 
#' plot_tSNE (global_patterns, grad = "SampleType", perplexity = NULL , 
#' number_of_iters  = 1000, initial_dims = 50, pca = TRUE, 
#' legend = TRUE, title = "t-SNE plot for Microbiome Data",
#' x_title = "Dimension 1 of the tsne plot", y_title = "Dimension 2 of the tsne plot", 
#' z_title = "Dimension 3 of the tsne plot", theme = "minimal", 
#' legend.position = "bottom", 
#' size = 3, title_size = 10) 

plot_tSNE.microbiome_dataset  <- function(object, grad, dims = 2,  
                                          perplexity = NULL , number_of_iters = 1000, initial_dims = 50, pca = TRUE, 
                                         legend = TRUE, title = "t-SNE plot for Microbiome Data",
                                         x_title = "dimension 1", y_title = "dimension 2", z_title = "dimension 3", theme = "minimal", legend.position = "bottom", 
                                         size = 3, title_size = 10, ..){
  
  expression_data <- microbiomedataset::extract_expression_data(object)
  sample_info <- microbiomedataset::extract_sample_info(object)
  
  max_perplexity <- (ncol(expression_data) - 1) / 3

  if (is.null(perplexity)) {
    perplexity <- max_perplexity - 1  # Default perplexity
    perplexity <- round(perplexity, 6)  # Round to 6 decimal places for consistency
  }
  
  if (!grad %in% colnames(sample_info)) {
    stop("The column '", grad, "' does not exist in the sample_info data.")
  }
  
  tSNE_object <- Rtsne::Rtsne(X = t(expression_data), 
                              initial_dims = initial_dims, 
                              dims = dims, 
                              perplexity = perplexity, 
                              verbose = TRUE, 
                              max_iter = number_of_iters)  
  theme_function <- switch(
    theme,
    "minimal" = ggplot2::theme_minimal,
    "classic" = ggplot2::theme_classic,
    "bw" = ggplot2::theme_bw,
    "light" = ggplot2::theme_light,
    "dark" = ggplot2::theme_dark,
    stop("Invalid theme name. Choose from: 'minimal', 
         'classic', 'bw', 'light', 'dark'")
  )
  
  if (dims == 2 ){
  
  tsne_df <- data.frame(
    X = tSNE_object$Y[, 1],  # t-SNE axis 1
    Y = tSNE_object$Y[, 2],  # t-SNE axis 2
    fill_new = as.factor(sample_info[[grad]])  # Use the grad column as a factor
  )
  
  tsne_plot <- ggplot2::ggplot(tsne_df, ggplot2::aes(x = X, y = Y, 
                                                     colour = fill_new)) + 
    ggplot2::geom_point(size = size) +  # Add points
    ggplot2::labs(title = title, 
                  x = x_title, 
                  y = y_title, 
                  colour = grad) + 
    theme_function() +  
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size),  # Customize title size
                   legend.position = legend.position)  # Control legend position
  
  
return(tsne_plot)
  } 
  else if(dims == 3){
    
    tSNE_object <- Rtsne::Rtsne(X = t(expression_data),  
                                initial_dims = initial_dims, 
                                dims = dims, 
                                perplexity = perplexity, 
                                verbose = TRUE, 
                                max_iter = number_of_iters)  
    
    sample_info <- microbiomedataset::extract_sample_info(object)
    
    tsne_df <- data.frame(
      X = tSNE_object$Y[, 1],
      Y = tSNE_object$Y[, 2],
      Z = tSNE_object$Y[, 3],
      Sample = as.factor(sample_info[[grad]]) 
    )
    
    hover_text <- paste(
      "Sample:", tsne_df$Sample,
      "Dimension 1:", round(tsne_df$X, 3),
      "Dimension 2:", round(tsne_df$Y, 3),
      "Dimension 3:", round(tsne_df$Z, 3)
    )
    
    tsne_plot <- plotly::plot_ly( data = tsne_df, x = ~X, y = ~Y, z = ~Z, 
                                  type = "scatter3d", mode = "markers", 
                                  marker = list(size = size), text = hover_text,
                                  hoverinfo = "text", color = ~Sample ) %>% 
      plotly::layout( title = title, scene = list( xaxis = list(title = x_title), 
                                                   yaxis = list(title = y_title), 
                                                   zaxis = list(title = z_title) ) )
    
    return(tsne_plot)
    
    
  }
}
  
      
                  
                  
                  
                  
                  
                  
                  
                                            
  
  

