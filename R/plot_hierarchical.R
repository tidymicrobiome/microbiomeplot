#plot Hierarchical clustering

#' Hierarchial Clustering 
#'
#' @param object the microbiomedataset object
#' @param method_dist the disimilarity metric method
#' @param binary boolean for the calculation of the distnace
#' @param diag boolean for the calculation of the distnace
#' @param upper boolean for the calculation of the distnace
#' @param na.rm boolean for the calculation of the distnace
#' @param theme the theme for the plot object
#' @param method_dend the method by which the hierarchial clustering to be carried out
#' @param group the group parameter for the clusters
#' @param type type of plot 
#' @param ... other parameters
#'
#' @return a ggplot2 object
#' @export
plot_hierarchical <- function(object, method_dist =c("manhattan", "canberra", "bray",
                                                  "kulczynski", "gower", "morisita", "horn", 
                                                  "mountford", "jaccard", "raup", "binomial", "chao", 
                                                  "altGower", "cao", "mahalanobis", "clark", "chisq", "chord", 
                                                  "hellinger", "aitchison", "robust.aitchison", "euclidean") , binary = FALSE, diag = FALSE, upper = FALSE,
                              na.rm = FALSE, theme = c('classic', 'bw','dark', 'light', 'minimal'),method_dend = c('ward.D', 'single', 'complete', 'ward.D2'), group = 'SampleType', type = 'rectangle',...){
  UseMethod('plot_hierarchical')
}


#' @method plot_hierarchical microbiome_dataset
#' @importFrom ggdendro dendro_data 
#' @importFrom microbiomedataset extract_expression_data extract_sample_info
#' @importFrom  vegan vegdist
#' @import ggplot2

#' @export
plot_hierarchical.microbiome_dataset <- function(object, method_dist = c("manhattan", "canberra", "bray",
                                                                    "kulczynski", "gower", "morisita", "horn", 
                                                                    "mountford", "jaccard", "raup", "binomial", "chao", 
                                                                    "altGower", "cao", "mahalanobis", "clark", "chisq", "chord", 
                                                                    "hellinger", "aitchison", "robust.aitchison", "euclidean") , binary = FALSE, diag = FALSE, upper = FALSE,
                                                 na.rm = FALSE, theme = c('classic', 'bw','dark', 'light', 'minimal'), method_dend = c('ward.D', 'single', 'complete', 'ward.D2'), group = 'SampleType', type = 'rectangle', ...){
  
  method_dist = match.arg(method_dist)
  theme = match.arg(theme)
  method_dend = match.arg(method_dend)
  
  expression_data = t(microbiomedataset::extract_expression_data(object))
  sample_info = microbiomedataset::extract_sample_info(object)
  
  bc_dist <- vegan::vegdist(expression_data, method = method_dist, binary = binary, diag = diag, upper = upper, na.rm = na.rm)
  ward <- hclust(bc_dist, method = method_dend)
  dendr    <- ggdendro::dendro_data(ward, type= type)
  group = as.factor(sample_info[, group])
  
  
  # Determine number of clusters
    k <- length(unique(group))
    cluster_names = unique(group)
  # Cut tree to get clusters
  clusters <- cutree(ward, k = k)
  # Create dendrogram data
  dend <- as.dendrogram(ward)
  dend_data <- dendro_data(dend, type = type)
  
  # Apply custom cluster names if provided
  if (!is.null(cluster_names) && length(cluster_names) == k) {
    cluster_labels <- factor(cluster_names[clusters], levels = cluster_names)
  } else {
    cluster_labels <- factor(clusters)
  }
  
  # Prepare label data
  label_data <- ggdendro::label(dend_data)
  label_data$cluster <- cluster_labels[match(label_data$label, names(clusters))]
  
  p = ggplot2::ggplot() + 
    ggplot2::geom_segment(data=ggdendro::segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    ggplot2::geom_text(data=label_data, aes(x, y, label=label, hjust=0, color=cluster), 
              size=3) + ggplot2::coord_flip()+ scale_y_reverse(expand=c(0.2, 0)) + 
    ggplot2::theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank())
  
  
  return(p)
  
  
  
}