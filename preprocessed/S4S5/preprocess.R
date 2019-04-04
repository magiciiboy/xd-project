library(data.table)

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
                  col.names = c('i', 't', 'c', 'coauthors'),
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
for (index in nrow(df_scholars)) {
  scholars[[ df_scholars[[1,"google_id"]] ]] = s_row
}


