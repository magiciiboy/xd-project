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

isCrossDisciplinaryByPollinators <- function(coauthors) {
  # Pollinator code
  # 0 = pollinator j only appeared in our dataset with other BIO ;
  # 1 = pollinator j only appeared with other CS ;
  # 2 corresponds to a mixture of CS and BIO - i.e. coauthor j is a cross-pollinator.
  if (coauthors) {
    if ( ("0" %in% coauthors) && ("1" %in% coauthors) ) {
      return(T)
    }
  }
  return(F)
}

isCrossDisciplinaryByDirectAuthors <- function(coauthor) {
  
}