# getwd
# setwd("D:\\bwm03\\Documents\\Graduate Classes\\Statistical Methods R")
library(ggplot2)
library(Cairo)

gs_data <- read.csv("./data/Faculty_GoogleScholar_Funding_Data_N4190.csv", header = TRUE)

head(gs_data, n=1)

nf_bio = sum(gs_data$XDIndicator=="BIO")
nf_cs = sum(gs_data$XDIndicator=="CS")
nf_xd = sum(gs_data$XDIndicator=="XD")

density_prc_bio = stats::density(gs_data$PRCentrality[gs_data$XDIndicator=="BIO"]*nf_bio, from=0, to=3)
density_prc_cs = stats::density(gs_data$PRCentrality[gs_data$XDIndicator=="CS"]*nf_cs, from=0, to=3)
density_prc_xd = stats::density(gs_data$PRCentrality[gs_data$XDIndicator=="XD"]*nf_xd, from=0, to=3)

# density_prc_bio$y[density_prc_bio$y < 1e-4] = NA
# density_prc_cs$y[density_prc_cs$y < 1e-4] = NA
# density_prc_xd$y[density_prc_xd$y < 1e-4] = NA

cut_bio = min(which(density_prc_bio$y < 1e-4))
cut_cs = min(which(density_prc_cs$y < 1e-4))
cut_xd = min(which(density_prc_xd$y < 1e-4))

density_df_bio = data.frame(x=density_prc_bio$x[1:cut_bio],
                        density=density_prc_bio$y[1:cut_bio] )
density_df_cs = data.frame(x=density_prc_bio$x[1:cut_cs],
                            density=density_prc_cs$y[1:cut_cs] )
density_df_xd = data.frame(x=density_prc_bio$x[1:cut_xd],
                            density=density_prc_xd$y[1:cut_xd] )

dens_df <- rbind(density_df_bio, density_df_cs, density_df_xd)
dens_df$group <- c(rep("BIO", cut_bio), rep("CS", cut_cs), rep("XD", cut_xd))

myorder = c("BIO","CS","XD")

CairoWin()
ggplot(data=dens_df, aes(x=x, y=density, ymax=density, ymin=1e-5, col=group, fill=group)) + 

  # geom_ribbon(color=NA, alpha = 0.5)  + 
  # scale_fill_manual(values=c("green", "purple", "black"))  + 
  
  geom_line(size=1.5)  + 
  scale_color_manual(values=c(XD="black", CS="purple", BIO="green"))  + 
  
  scale_y_log10() + 
  coord_cartesian(ylim = c(1.0e-4, 5), expand=FALSE)



fig3d <- ggplot() +
  geom_ribbon (density_df_bio, aes(x = x, ymax=density, ymin=1e-4 ), colour="green", alpha = 0.3, na.rm = TRUE)
  geom_ribbon (aes(x = x, ymax=density, ymin=1e-4 ), colour="purple", alpha = 0.3, na.rm = TRUE) +
  geom_ribbon (aes(x = x, ymax=density, ymin=1e-4 ), colour="black", alpha = 0.3, na.rm = TRUE) +
  scale_y_log10()



