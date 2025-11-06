### cd-hit function

cd_cluster <- function(path) {
  
  clstr_file <- path
  lines <- readLines(clstr_file)
  
  cluster_list <- list()
  current_cluster <- NULL
  
  for (line in lines) {
    if (grepl("^>Cluster", line)) {
      current_cluster <- sub(">Cluster ", "", line)
      cluster_list[[current_cluster]] <- c()
    } else {
      seq_id <- sub(".*>([^\\.]+).*", "\\1", line)
      cluster_list[[current_cluster]] <- c(cluster_list[[current_cluster]], seq_id)
    }
  }
  
  # Convert to data.frame
  cluster_df <- do.call(rbind, lapply(names(cluster_list), function(cluster) {
    data.frame(Sequence_ID = cluster_list[[cluster]], Cluster = cluster)
    
    
  }))
  return(cluster_df)
}
