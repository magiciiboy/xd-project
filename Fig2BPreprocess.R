# Author: Tung Huynh
library(tidyr)

FILE_NAME = "./preprocessed/collaborations_xd_fraction.csv"
library(dplyr)

source('./Fig2Config.R')
source('./Fig2Scholar.R')

PAPERS_SPLITTED_BY_YEAR = T

# Functions
splitPapersByYear <- function(bin=1) {
  # This function splits the data by published year of  
  # a paper. Base on those preprocessed sub-dataset we
  # will create nodes and edges for each year.
  # According to the paper, the research orientation of
  # each falcuty can shift from an original department 
  # to a cross-disciplinary at the year of cross-disciplinary
  # paper has been published.
  df_papers = read.csv('./data/GoogleScholar_paper_stats.csv')
  years = c(1990:2015)
  
  cols <- c("google_id", "year", "citations", "coauthor_codes")
  
  # Five year bin
  for (year in years) {
    df_papers_year = filter(df_papers, X2014 <= year & X2014 > (year - bin))
    colnames(df_papers_year) <- cols
    write.csv(df_papers_year, paste0('./preprocessed/2b/papers_year_', year, '.csv'))
  }
  
  return(T)
}

processEdgesAndNodesPerYear <- function(year) {
  df_papers <- read.csv(paste0('./preprocessed/2b/papers_year_', year, '.csv'))
  
  df_nodes <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df_nodes) <- c('Id', 'Label', 'Interval', 'Weight', 'Dept', 'Orientation')
  df_edges <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_edges) <- c('Source', 'Target', 'Type', 'Id', 'Label', 'Interval', 'Weight')
  
  # Read papers and create nodes and edges
  for (row in 1:nrow(df_papers)) {
    gid <- as.character(df_papers[row, "google_id"])
    
    coauthor_codes <- as.character(df_papers[row, "coauthor_codes"])
    coauthors <- unique(strsplit(coauthor_codes, ',')[[1]])
    isXD = isCrossDisciplinaryByPollinators(coauthors)
    
    orientation_xd = ifelse(isXD, 'XD', NA)
    df_nodes <- addAuthorNode(df_nodes, gid, orientation = orientation_xd)
    
    # Directed author
    directed_coauthors = coauthors[coauthors != '0' 
                                   & coauthors != '1'
                                   & coauthors != '2' 
                                   & coauthors != gid
                                   & !is.na(coauthors)
                                   & !is.null(coauthors)
                                   & coauthors != '']
    
    n_coauthors = length(directed_coauthors)
    if(n_coauthors) {
      for (row_dca in 1:n_coauthors) {
        coauthor_gid <- directed_coauthors[row_dca]
        # Add node and an edge to the falcuty
        df_nodes <- addAuthorNode(df_nodes, coauthor_gid)
        df_edges <- addEdge(df_edges, gid_source=gid, gid_target=coauthor_gid)
        
        # Add edge between coauthors
        if (n_coauthors > 1 && row_dca < n_coauthors) {
          for (row_a_to_a in row_dca + 1:n_coauthors) {
            next_coauthor_id <- directed_coauthors[row_a_to_a]
            df_edges <- addEdge(df_edges, gid_source=coauthor_gid, gid_target=next_coauthor_id)
          }
        }
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
  write.csv(df_nodes_cleaned, paste0('./preprocessed/2b/nodes_year_', year, '.csv'))
  write.csv(df_edges, paste0('./preprocessed/2b/edges_year_', year, '.csv'))
  
  return(T)
}

aggreateEdgesForYears <- function() {
  df_fraction <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df_fraction) <- c('xd_direct', 'xd_mediate', 'year')
  
  xd_direct_count <- 0 
  xd_mediate_count <- 0 
  xd_total_count <- 0
  
  for (year in years) {
    df_papers_year = read.csv(paste0('./preprocessed/2b/papers_year_', year, '.csv'))
    xd_direct <- xd_direct_count / xd_total_count
    xd_mediate <- xd_mediate_count / xd_total_count
    
    df_fraction[nrow(df_fraction) + 1,] <- list(
      xd_direct,
      xd_medate,
      year
    )
  }
  
  write.csv(FILE_NAME, df_fraction)
}

if (!PAPERS_SPLITTED_BY_YEAR) {
  splitPapersByYear()
}