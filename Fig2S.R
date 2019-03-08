##plot a includes biodirect and biomediated
S2A <- plot_ly(x = biodirect, opacity = 0.5, type = "histogram", name = "Direct (Biology)", width = 1000, showlegend = TRUE)  %>% add_trace(x = biomediated, name = "Mediated (Biology)") %>% add_segments(S2A, x=mean(biodirect), y=1, xend=mean(biodirect), yend=1000) %>% add_segments(S2A, x=mean(biomediated), y=1, xend=mean(biomediated), yend=1000) %>% layout(xaxis = list(title = "Link Degree", type = "linear"), yaxis = list(title = "Count", type = "log"), barmode = "overlay")
S2A


##plot includes csdirect and csmediated
S2B <- plot_ly(x = csdirect, opacity = 0.5, type = "histogram", name = "Direct (Computer Science)") %>% add_trace(x = csmediated, name = "Mediated (Computer Science)") %>% add_segments(S2B, x=mean(csdirect), y=1, xend=mean(csdirect), yend=1000) %>% add_segments(S2B, x=mean(csmediated), y=1, xend=mean(csmediated), yend=1000) %>% layout(xaxis = list(title = "Link Degree", type = "linear"), yaxis = list(title = "Count", type = "log"), barmode = "overlay")
S2B 