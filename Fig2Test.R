df_papers = read.csv('./data/GoogleScholar_paper_stats.csv')
cols <- c("google_id", "year", "citations", "coauthor_codes")
colnames(df_papers) <- cols

s1st <- "UedS9LQAAAAJ"
s2nd <- "LXVfPc8AAAAJ"

df_1st = filter(df_papers, (google_id==s1st | grepl(s1st, coauthor_codes)))
df_2nd <- filter(df_papers, (google_id==s2nd | grepl(s2nd, coauthor_codes)))

n1st <- nrow(df_1st)
n2nd <- nrow(df_2nd)

coauthors_1st = c()
coauthors_2nd = c()

for (row in 1:n1st) {
  cc_str = df_1st[row, "coauthor_codes"]
  if(!is.null(cc_str) && !is.na(cc_str)) {
    cc = strsplit(as.character(cc_str), ',')[[1]]
    coauthors_1st <- c(coauthors_1st, cc)
  }
}

direct_coauthors_1st <- coauthors_1st[coauthors_1st!="0" 
                               & coauthors_1st!="1" 
                               & coauthors_1st!="2"
                               & coauthors_1st!=s1st]

pollinators_1st <- coauthors_1st[coauthors_1st=="0"
                                 | coauthors_1st=="1"
                                 | coauthors_1st=="2"]

for (row in 1:n2nd) {
  cc_str = df_2nd[row, "coauthor_codes"]
  if(!is.null(cc_str) && !is.na(cc_str)) {
    cc = strsplit(as.character(cc_str), ',')[[1]]
    coauthors_2nd <- c(coauthors_2nd, cc)
  }
}
direct_coauthors_2nd <- coauthors_2nd[coauthors_2nd!="0" 
                               & coauthors_2nd!="1" 
                               & coauthors_2nd!="2"
                               & coauthors_2nd!=s2nd]
pollinators_2nd <- coauthors_2nd[coauthors_2nd %in% c("0", "1", "2")]

#print(coauthors_1st)
#print(coauthors_2nd)

print("Wei Wang")
print(paste("No. of papers:", n1st))
print(paste("No. of coauthors:", length(unique(direct_coauthors_1st))))
print(paste("Ratio of pollinators:", length(pollinators_1st) / length(coauthors_1st)))

print("Eric Lander")
print(paste("No. of papers:", n2nd))
print(paste("No. of coauthors:", length(unique(direct_coauthors_2nd))))
print(paste("Ratio of pollinators:", length(pollinators_2nd) / length(coauthors_2nd)))


