# Load common data
df_scholars = read.csv('./data/Faculty_GoogleScholar_Funding_Data_N4190.csv')

getScholarInfo <- function(gid) {
  # Get general information of a faculty
  # name, dept
  f_ <- filter(df_scholars, google_id == gid)
  if ( nrow(f_) ) {
    return(f_[1,])
  } else {
    return(NULL) 
  }
}

getCoauthors <- function(df, gid) {
  df_coauthors <- filter(df, Source == gid | Target == gid)
  if (nrow(df_coauthors) > 0) {
    coauthors <- unique(c(as.vector(unique(df$Source)), as.vector(unique(df$Target))))
    coauthors <- mapply(function(x) { return(x == gid) }, coauthors)
    return(coauthors)
  }
  return(NA)
}

isCrossDisciplinaryByPollinators <- function(coauthors) {
  # Pollinator code
  # 0 = pollinator j only appeared in our dataset with other BIO ;
  # 1 = pollinator j only appeared with other CS ;
  # 2 corresponds to a mixture of CS and BIO - i.e. coauthor j is a cross-pollinator.
  if ( !is.na(coauthors) && length(coauthors) > 1 ) {
    if ( ("0" %in% coauthors) && ("1" %in% coauthors) ) {
      return(T)
    }
  }
  return(F)
}

getScholarOrientation <- function(df, gid, coauthors) {
  # df: node dataset
  # gid:
  # coauthors: vector
  # return "CS", "BIO", "XD"
  scholar <- filter(df, Id == gid)
  my_dept <- as.character(scholar[1,]$Dept)
  
  df_coauthors <- filter(df, Id %in% coauthors)
  depts <- as.vector(unique(df$Dept))
  their_depts <- mapply(function(x) { return(x != my_dept) }, depts)
  
  if ( length(their_depts) > 0 ) {
    return("XD")
  } else {
    return(my_dept)
  }
}