# Author: Tung Huynh

addAuthorNode <- function(df, gid, orientation=NA, k=1) {
  # Add new author node
  # 'Id', 'Label', 'Interval', 'Weight', 'Dept', 'Orientation'
  if (!length(gid)) { return(df) }
  if (is.null(gid)) { return(df) }

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
  
  if (!is.null(scholar) && !existed) {
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

addEdge <- function(df, gid_source, gid_target, type=NA, add_mediate_links=F) {
  # Add a collaboration edge
  # 'Source', 'Target', 'Type', 'Id', 'Label', 'Interval', 'Weight'
  if (is.na(gid_source) || is.na(gid_target)) {
    return(df)
  }
  
  edge_source <- gid_source
  edge_targe <- gid_target
  # Type: Direct / Mediated
  edge_type <- ifelse(!is.na(type), type, 'Direct')
  edge_id <- paste(gid_source, gid_target, sep="_")
  edge_label <- edge_type
  edge_interval <- ''
  edge_weight <- 1
  
  existed <- (nrow(filter(df, (Source == gid_source & Target == gid_target) 
                              | (Source == gid_target & Target == gid_source))) > 0)
  
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
  
  # Add mediate link
  if (add_mediate_links) {
    coauthor_origin_links <- filter(df, (Source == gid_source & Target == gid_source))
    coauthor_target_links <- filter(df, (Source == gid_target & Target == gid_target))
    
    for(co in 1:nrow(coauthor_origin_links)) {
      orow <- coauthor_origin_links[co]
      oid <- ifelse(orow$Source == gid_source, orow$Target, orow$Source)
      for(ct in 1:nrow(coauthor_target_links)) {
        trow <- coauthor_target_links[ct]
        tid <- ifelse(orow$Source == gid_target, trow$Target, trow$Source)
        addEdge(gid_source = oid, gid_target = tid, type='Mediate')
      }
    }
  }
  
  return(df)
}

calculateNodeDegree <- function(df_edges, gid) {
  return(nrow(filter(df_edges, Source == gid | Target == gid)))
}