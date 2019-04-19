library(data.table)
library(stringr)
library(fastmatch)


df_scholars = fread(
    './data/Faculty_GoogleScholar_Funding_Data_N4190.csv',
    sep = ",",
    header = TRUE,
    strip.white = TRUE,
    showProgress = FALSE,
    data.table = TRUE,
    logical01 = FALSE,
    colClasses = c(
        "character",
        "character",
        "character",
        rep("numeric", 20),
        "character",
        "numeric"
    )
)
df_papers = fread(
    './data/GoogleScholar_paper_stats.csv',
    col.names = c('google_id', 'year', 'citations', 'co_authors'),
    sep = ",",
    header = FALSE,
    strip.white = TRUE,
    showProgress = FALSE,
    data.table = TRUE,
    logical01 = FALSE,
    colClasses = c("character", "numeric", "numeric", "character")
)

shape_scholars = dim(df_scholars)  # (4190, 25)
shape_papers = dim(df_papers)      # (424827, 4)

# df_papers_2017 = df_papers[df_papers[["t"]] <= 2017, ]  # no difference ?? 


# Map scholars to dict
# print("making the dict")
# scholars = vector(mode = "list")
# for (index in 1:nrow(df_scholars)) {
#     scholars[[df_scholars[[index, "google_id"]]]] = df_scholars[index, ]
# }


compute_logc = function(p) {
    if (is.null(p[["c"]])) {
        return(log(1))
    } else {
        return(log(1 + p[["c"]]))
    }
}

compute_dept = function(p) {
    return(scholars[[p[['i']]]][['dept']])
}

compute_coauthor = function(str) {
    return (str_count(str, ",") + 1)
}

compute_paper_orientation = function(str) {
    has_bio = FALSE
    has_cs = FALSE

    coauth_list = str_split(str, ",", simplify = TRUE)

    for (d in coauth_list) {
        if (has_bio && has_cs)
            break
        # print(d)
        if (d == "0") {
            has_bio = TRUE
        } else if (d == "1") {
            has_cs = TRUE
        } else if (d == "2") {
            # ??
        } else {
            # scholar_dept = scholars[[d]][["dept"]]
            id = fmatch(d, df_scholars$google_id)
            if(! is.na(id) ) {
                scholar_dept = df_scholars$dept[id]
                if(scholar_dept == "BIO"){
                    has_bio = TRUE
                } else if(scholar_dept == "CS") {
                    has_cs = TRUE
                }
            } else {
                print(d)
            }
        }
    }

    if (has_bio && has_cs) { return (1) }
    else { return (0) }
}


compute_z_score = function(dept, year, cita) {
    
    mu_sigma = cached_mu_sigma[[paste0(dept, year)]]
    z_ip = (log(cita + 1) - mu_sigma[1]) / mu_sigma[2]
    return (z_ip)
    
}


print('Computing dept ...')
paper_index_in_scholar = match(df_papers$google_id, df_scholars$google_id)
print( sum(is.na(paper_index_in_scholar)) == 0 ) # check na
df_papers$dept = df_scholars$dept[paper_index_in_scholar]


print('Computing a ...')
df_papers$a = sapply(df_papers$co_authors, compute_coauthor)
print( sum(is.na(df_papers$a)) == 0 ) # check na
df_papers$ln_a = log( df_papers$a )


print('Computing tau ...')
career_start = df_scholars$min_year [paper_index_in_scholar]
df_papers$tau = df_papers$year - career_start + 1


print('Computing I ...')
df_papers$I = sapply(df_papers$co_authors, compute_paper_orientation)
sum( df_papers$I )  # should be 3915


print("Computing XD ...")
df_papers$XD = df_scholars$XDIndicator[paper_index_in_scholar] == "XD"
sum( df_papers$XD ) # should be 166621


print("Computing PR ...")
df_papers$PR = log( df_scholars$PRCentrality[paper_index_in_scholar] )


# print('Computing Bridge ratio ...')



print('Computing z-score ...')
dept_set = unique( df_papers$dept )
year_set = unique( df_papers$year )
cached_mu_sigma = vector(mode = "list")

for (d in dept_set) {
    for (y in year_set) {
        df_papers_st = df_papers[df_papers$dept==d & df_papers$year==y , ]
        df_papers_st$ln_c = log( df_papers_st$citations + 1 )
        mu_t = mean ( df_papers_st$ln_c )
        sigma_t = sd( df_papers_st$ln_c )
        cached_mu_sigma[[ paste0(d,y) ]] = c(mu_t, sigma_t)
    }
}

df_papers$z = mapply(compute_z_score , df_papers$dept, df_papers$year, df_papers$citations , SIMPLIFY = "array")



write.csv(df_papers, "panel_model_paper_citations_data_all_RPRE.csv")
write.csv(df_papers[df_papers$XD==1 , ], "panel_model_paper_citations_data_xd_RPRE.csv")



