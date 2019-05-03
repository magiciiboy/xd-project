library(sjstats)
library(plm)
library(dplyr)
library(robustHD)

library(data.table)
library(fastmatch)

dat <- fread("./preprocessed/S4S5/panel_model_paper_citations_data_XD_A.csv")

dat$t = as.numeric(dat$t)
dat$a = as.numeric(dat$a)

# Remove row having NA (Ex: pagerank)
dat <- na.omit(dat)
print( nrow(dat) )

xd_data = dat[ dat$I == 1, ]
noxd_data = dat[ dat$I == 0, ]



match_pair <- function(in_row) {
    id = in_row[["i"]]
    year = as.numeric(in_row[["t"]])
    n_ca = as.numeric(in_row[["a"]])
    
    author_record = noxd_data[ noxd_data$i == id, ]
    # print(author_record)
    
    author_record = author_record[ author_record$t >= year-2 & author_record$t <= year+2 , ]
    # print(author_record)
    
    ca_diff = abs( author_record$a - n_ca ) / n_ca < 0.2
    author_record = author_record[ca_diff,]
    # print(author_record)
    n = nrow(author_record)
    return( n )
}


result = apply(xd_data, 1, match_pair)

xd_data$n_pair = result

uniq_author = unique(xd_data$i)
uniq_author = data.frame(uniq_author)

for (i in 1:nrow(uniq_author)) {
    auth = uniq_author$uniq_author[i]
    r = xd_data[xd_data$i == auth,]
    uniq_author$sel[i] = sum(r$n_pair > 0)
}

sum( uniq_author$sel >= 10 )







