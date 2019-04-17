library(data.table)
library(stringr)

df_scholars = fread('./data/Faculty_GoogleScholar_Funding_Data_N4190.csv',
                    sep=",",
                    header=TRUE,
                    strip.white=TRUE,
                    showProgress=FALSE,
                    data.table=TRUE,
                    logical01=FALSE,
                    colClasses=c("character","character","character",
                                 rep("numeric",20),
                                 "character", "numeric")
                    )
df_papers = fread('./data/GoogleScholar_paper_stats.csv',
                  col.names = c('google_id', 'year', 'citations', 'co_authors'),
                  sep=",",
                  header=FALSE,
                  strip.white=TRUE,
                  showProgress=FALSE,
                  data.table=TRUE,
                  logical01=FALSE,
                  colClasses=c("character","numeric","numeric","character")
                  )

shape_scholars = dim(df_scholars)  # (4190, 25)
shape_papers = dim(df_papers)      # (424827, 4)

df_papers_2017 = df_papers[df_papers[["t"]] <= 2017,]

# Cached mu, sigma for y,t
cached_mu_sigma = vector(mode="list")
idx = 0

# Map scholars to dict
scholars = vector(mode="list")
for ( index in 1:nrow(df_scholars) ) {
  scholars[[ df_scholars[[index,"google_id"]] ]] = df_scholars[index,]
}


compute_logc = function(p) {
  if(is.null(p[["c"]])) {
    return( log(1) )
  } else {
    return( log(1 + p[["c"]] ) )
  }
}

compute_dept = function(p) {
  return( scholars[[ p[['i']] ]][['dept']] )
}

count_comma = function(str) {
  return ( str_count(str, ",") + 1 )
}


print('Computing dept ...')
paper_index_in_scholar = match( df_papers[["google_id"]], df_scholars[["google_id"]] )
print( sum( is.na(paper_index_in_scholar) ) ) # check na

df_papers[["dept"]] = df_scholars[["dept"]][paper_index_in_scholar]


print('Computing a ...')
df_papers[["a"]] = lapply( df_papers[["co_authors"]], count_comma )


print('Computing tau ...')
career_start = df_scholars[["min_year"]][paper_index_in_scholar]
df_papers[["tau"]] = df_papers[["year"]] - career_start + 1


print('Computing I ...')





