# This script generate output used in Gephi Application to 
# create the network graph. 
# There are 2 files to generate:
# - Nodes: (id, label, interval, size)
# - Edges: (source, target, type, id, label, interval, weight)
library(dplyr)

source('./Fig2Config.R')
source('./Fig2Scholar.R')
source('./Fig2Graph.R')

# Functions
splitPapersByYear <- function() {
  # This function splits the data by published year of  
  # a paper. Base on those preprocessed sub-dataset we
  # will create nodes and edges for each year.
  # According to the paper, the research orientation of
  # each falcuty can shift from an original department 
  # to a cross-disciplinary at the year of cross-disciplinary
  # paper has been published.
  df_papers = read.csv('./data/GoogleScholar_paper_stats.csv')
  df_papers_1990 = filter(df_papers, X2014 == 1990)
  df_papers_1995 = filter(df_papers, X2014 == 1995)
  df_papers_2000 = filter(df_papers, X2014 == 2000)
  df_papers_2005 = filter(df_papers, X2014 == 2005)
  df_papers_2010 = filter(df_papers, X2014 == 2010)
  df_papers_2015 = filter(df_papers, X2014 == 2015)
  
  cols <- c("google_id", "year", "citations", "coauthor_codes")
  colnames(df_papers_1990) <- cols
  colnames(df_papers_1995) <- cols
  colnames(df_papers_2000) <- cols
  colnames(df_papers_2005) <- cols
  colnames(df_papers_2010) <- cols
  colnames(df_papers_2015) <- cols
  
  write.csv(df_papers_1990, './preprocessed/papers_year_1990.csv')
  write.csv(df_papers_1995, './preprocessed/papers_year_1995.csv')
  write.csv(df_papers_2000, './preprocessed/papers_year_2000.csv')
  write.csv(df_papers_2005, './preprocessed/papers_year_2005.csv')
  write.csv(df_papers_2010, './preprocessed/papers_year_2010.csv')
  write.csv(df_papers_2015, './preprocessed/papers_year_2015.csv')
  return(T)
}

processEdgesAndNodesPerYear <- function(year) {
  df_papers <- read.csv(paste0('./preprocessed/papers_year_', year, '.csv'))
  
  df_nodes <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df_nodes) <- c('Id', 'Label', 'Interval', 'Weight', 'Dept', 'Orientation')
  df_edges <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_edges) <- c('Source', 'Target', 'Type', 'Id', 'Label', 'Interval', 'Weight')
  
  # Read papers and create nodes and edges
  for (row in 1:nrow(df_papers)) {
    gid <- as.character(df_papers[row, "google_id"])
    
    coauthor_codes <- as.character(df_papers[row, "coauthor_codes"])
    coauthors <- unlist(strsplit(coauthor_codes, ','))
    isXD = isCrossDisciplinaryByPollinators(coauthors)
    
    orientation_xd = ifelse(isXD, 'XD', NA)
    df_nodes <- addAuthorNode(df_nodes, gid, orientation = orientation_xd)
    
    # Directed author
    directed_coauthors = coauthors[coauthors != '0' & coauthors != '1' & coauthors != '2']
    for (row_dca in 1:length(directed_coauthors)) {
      coauthor_gid <- directed_coauthors[row_dca]
      if ( gid != coauthor_gid ) {
        df_nodes <- addAuthorNode(df_nodes, coauthor_gid)
        df_edges <- addEdge(df_edges, gid_source=gid, gid_target=coauthor_gid)
      }
    }
  }
  
  # Elimiate nodes having no edges
  df_nodes_cleaned <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df_nodes_cleaned) <- c('Id', 'Label', 'Interval', 'Weight', 'Dept', 'Orientation')
  
  for (row in 1:nrow(df_nodes)) {
    gid <- as.character(df_nodes[row, "Id"])
    
    node_degree <- calculateNodeDegree(df_edges, gid)
    if( node_degree > 0 ) {
      xd <- (as.character(df_nodes[row, "Orientation"]) == 'XD')
      node_coauthors <- getCoauthors(df_edges, gid)
      node_orientation <- ifelse(xd, 'XD', getScholarOrientation(df_nodes, gid, node_coauthors))
      # Add node
      df_nodes_cleaned <- addAuthorNode(df_nodes_cleaned, gid, orientation=node_orientation, k=node_degree)
    }
  }
  
  # Save
  write.csv(df_nodes_cleaned, paste0('./preprocessed/nodes_year_', year, '.csv'))
  write.csv(df_edges, paste0('./preprocessed/edges_year_', year, '.csv'))
  
  return(T)
}

# Main
if (!DATA_SPLITTED_BY_YEARS) {
  splitPapersByYear()
}

if (!DATA_PROCESSED_NODES_EDGES) {
  processEdgesAndNodesPerYear(1990)
  processEdgesAndNodesPerYear(1995)
  processEdgesAndNodesPerYear(2000)
  processEdgesAndNodesPerYear(2005)
  processEdgesAndNodesPerYear(2010)
  processEdgesAndNodesPerYear(2015)
}
