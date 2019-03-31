library(igraph)
library(dplyr)
library(data.table)
library(matrixStats)

edge_list = fread("preprocessed/edges_year_2015.csv")
edge_list = select(edge_list, Source, Target)
graph = graph_from_data_frame(edge_list, directed = TRUE)
g_ecount = ecount(graph)

cmpnt = components(graph, mode = "weak")
original_size = cmpnt$csize[1]

el = as_edgelist(graph, names = TRUE)
el[1:5,]



max_comp = matrix(0, nrow = g_ecount, ncol = 40)
frag_size = matrix(0, nrow = g_ecount, ncol = 40)

for (repi in 1:10) {
  print(repi)
  g2 = graph
  for (i in 1:g_ecount) {
    current_size = ecount(g2)
    eid = sample(1:current_size, 1)
    g2 = delete.edges(g2, edges = c(eid))
    
    cmpnt = components(g2, mode = "weak")
    max_comp[i, repi] =  cmpnt$csize[1]
    frag_size[i, repi] = var(  cmpnt$csize[cmpnt$csize < max(cmpnt$csize)]   )
  }
}


x_intv = seq(1, g_ecount, 300)
x_intv_norm = x_intv / g_ecount


png("./output/FigS1.png", width=1200, height=2000)
layout(matrix(c(1,2), 2, 1, byrow = TRUE))
par(cex=2)

max_comp[max_comp==1] = NA
max_comp_perc = max_comp / original_size

p1_mean = rowMeans(max_comp_perc[,1:10], na.rm = TRUE)
p1_var = rowVars(max_comp_perc[,1:10], na.rm = TRUE)

plot(x=(1:g_ecount)/g_ecount, y=p1_mean, type="l", ylim=range(0,1), xlim=range(0,1), lwd = 2,
     xlab="Fraction of Removed Links", ylab="Size of Giant Component")
par(new=TRUE)
arrows( x_intv_norm, p1_mean[x_intv]-p1_var[x_intv], x_intv_norm, p1_mean[x_intv]+p1_var[x_intv],
        length=0.05, angle=90, code=3, ylim=range(0,1), xlim=range(0,1), lwd = 2)






# frag_size[frag_size<=1] = NA

p2_mean = rowMeans(frag_size, na.rm = TRUE)
p2_var = rowVars(frag_size[,1:10], na.rm = TRUE)

plot(x=(1:g_ecount)/g_ecount, y=p2_mean, type="l", ylim=range(0,2), xlim=range(0,1), lwd = 2,
     xlab="Fraction of Removed Links", ylab="Variation in Fragmentation Size")


dev.off()