addAuthorNode <- function(df, gid, orientation=NA, k=1) {
  # Add new author node
  # 'Id', 'Label', 'Interval', 'Weight', 'Dept', 'Orientation'
  scholar <- getScholarInfo(gid)
  existed <- (nrow(filter(df, Id == gid)) > 0)
  
  node_id <- gid
  node_label <- ifelse(scholar, as.character(scholar$name), '')
  node_interval <- 0
  node_dept <- ifelse(scholar, as.character(scholar$dept), '')
  # Orientations: BIO, CS, XD
  node_orientation <- ifelse(!is.na(orientation), orientation, '')
  # Node Degree
  node_weight <- ifelse(k, k, 1)
  
  if (!existed) {
    df[nrow(df) + 1,] <- list(
      node_id, 
      node_label, 
      node_interval, 
      node_weight, 
      node_dept, 
      node_orientation
    )
  }
  return(df)
}

addEdge <- function(df, gid_source, gid_target) {
  # Add a collaboration edge
  # 'Source', 'Target', 'Type', 'Id', 'Label', 'Interval', 'Weight'
  edge_source <- gid_source
  edge_targe <- gid_target
  # Type: Direct / Mediated
  edge_type <- 'Direct'
  edge_id <- paste(gid_source, gid_target, sep="_")
  edge_label <- edge_type
  edge_interval <- ''
  edge_weight <- 1
  
  existed <- (nrow(filter(df, Source == gid_source & Target == gid_target)) > 0)
  
  if (!existed) {
    df[nrow(df) + 1,] <- list(
      edge_source,
      edge_targe,
      edge_type,
      edge_id,
      edge_label,
      edge_interval,
      edge_weight
    )
  }
  return(df)
}

calculateNodeDegree <- function(df_edges, gid) {
  return(nrow(filter(df_edges, Source == gid | Target == gid)))
}