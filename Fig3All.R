library(ggplot2)
library(gridExtra)
library(plotly)
library(plyr)
library(scales)
library(ggpubr)

fund_data <- read.csv("./data/Faculty_GoogleScholar_Funding_Data_N4190.csv", header = TRUE)


# =========================================================================================================== A
mu <- ddply(fund_data, "XDIndicator", summarise, grp.mean=mean(min_year))

fig3a <- ggplot(fund_data, aes(x = min_year, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)

fig3a <- fig3a + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") +
  scale_x_continuous(name = "Year of First Publication", breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = expression(paste("PDF (y" [i] ^ "0",")" ) ) )
# fig3a

# =========================================================================================================== B
mu <- ddply(fund_data, "XDIndicator", summarise, grp.mean=mean(KTotal))

fig3b <- ggplot(fund_data, aes(x = KTotal, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)

fig3b <- fig3b + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") +
  scale_x_continuous(name = "Total Degree of Collaboration, Ki", breaks = c(0, 500, 1000, 1500)) +
  scale_y_continuous(name = expression(paste("PDF (y" [i] ^ "0",")" ) ) )
# fig3b

# =========================================================================================================== c
mu <- ddply(fund_data, "XDIndicator", summarise, grp.mean=mean(Chi))

fig3c <- ggplot(fund_data, aes(x = X, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)

fig3c <- fig3c + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") +
  scale_x_continuous(name = "Cross-Disciplinarity", breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_continuous(name = expression(paste("PDF (", chi[i], ")" ) ) )
fig3c


# =========================================================================================================== D
nf_bio = sum(fund_data$XDIndicator=="BIO")
nf_cs = sum(fund_data$XDIndicator=="CS")
nf_xd = sum(fund_data$XDIndicator=="XD")

density_prc_bio = stats::density(fund_data$PRCentrality[fund_data$XDIndicator=="BIO"]*nf_bio, from=0, to=3)
density_prc_cs = stats::density(fund_data$PRCentrality[fund_data$XDIndicator=="CS"]*nf_cs, from=0, to=3)
density_prc_xd = stats::density(fund_data$PRCentrality[fund_data$XDIndicator=="XD"]*nf_xd, from=0, to=3)

cut_bio = min(which(density_prc_bio$y < 1e-4))
cut_cs = min(which(density_prc_cs$y < 1e-4))
cut_xd = min(which(density_prc_xd$y < 1e-4))

density_df_bio = data.frame(x=density_prc_bio$x[1:cut_bio],
                            density=density_prc_bio$y[1:cut_bio] )
density_df_cs = data.frame(x=density_prc_bio$x[1:cut_cs],
                           density=density_prc_cs$y[1:cut_cs] )
density_df_xd = data.frame(x=density_prc_bio$x[1:cut_xd],
                           density=density_prc_xd$y[1:cut_xd] )

mu$grp.mean[1] = mean(fund_data$PRCentrality[fund_data$XDIndicator=="BIO"]*nf_bio)
mu$grp.mean[2] = mean(fund_data$PRCentrality[fund_data$XDIndicator=="CS"]*nf_cs)
mu$grp.mean[3] = mean(fund_data$PRCentrality[fund_data$XDIndicator=="XD"]*nf_xd)

dens_df <- rbind(density_df_bio, density_df_cs, density_df_xd)
dens_df$group <- c(rep("BIO", cut_bio), rep("CS", cut_cs), rep("XD", cut_xd))

fig3d = ggplot(data=dens_df, aes(x=x, y=density, ymax=density, ymin=1e-5, col=group, fill=group)) + 
  geom_ribbon(color=NA, alpha = 0.5)  +
  # scale_fill_manual(values=c("green", "purple", "black"))  +
  geom_line(size=1.1)  + 
  # scale_color_manual(values=c(XD="black", CS="purple", BIO="green"))  + 
  scale_y_log10() + 
  geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") +
  coord_cartesian(ylim = c(1.0e-4, 5), expand=FALSE) +
  ylab(expression(paste("PDF" ) ) ) +
  xlab("PageRank Centrality")
fig3d


# =========================================================================================================== E
density_mif_bio = stats::density(fund_data$mean_of_IF[fund_data$XDIndicator=="BIO"])
density_mif_cs = stats::density(fund_data$mean_of_IF[fund_data$XDIndicator=="CS"])
density_mif_xd = stats::density(fund_data$mean_of_IF[fund_data$XDIndicator=="XD"])
mu <- ddply(fund_data, "XDIndicator", summarise, grp.mean=mean(mean_of_IF))

bio_start = 11
bio_end = 495

cs_start = 2
cs_end = min(which(density_mif_cs$y < 1e-4 * 1.5)) - 10

xd_start = 10
xd_end = 488

density_df_bio = data.frame(x=density_mif_bio$x[bio_start:bio_end],
                            density=density_mif_bio$y[bio_start:bio_end] )
density_df_cs = data.frame(x=density_mif_cs$x[cs_start:cs_end],
                           density=density_mif_cs$y[cs_start:cs_end] )
density_df_xd = data.frame(x=density_mif_xd$x[xd_start:xd_end],
                           density=density_mif_xd$y[xd_start:xd_end] )

dens_df <- rbind(density_df_bio, density_df_cs, density_df_xd)
dens_df$group <- c(rep("BIO", bio_end-bio_start+1), rep("CS", cs_end-cs_start+1), rep("XD", xd_end-xd_start+1))

fig3e = ggplot(data=dens_df, aes(x=x, y=density, ymax=density, ymin=1e-5, col=group, fill=group)) + 
  geom_ribbon(color=NA, alpha = 0.5)  +
  # scale_fill_manual(values=c("green", "purple", "black"))  +
  geom_line(size=1.1) +
  # scale_color_manual(values=c(XD="black", CS="purple", BIO="green"))  + 
  scale_y_log10() + 
  coord_cartesian(ylim = c(1.0e-4, 5), xlim = c(0, density_mif_xd$x[xd_end]), expand=FALSE) +
  geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") +
  ylab(expression(paste("PDF" ) ) ) +
  xlab("Mean publication impact factor")
# fig3d


# =========================================================================================================== F
density_pub_bio = stats::density(fund_data$t_pubs_citations[fund_data$XDIndicator=="BIO"], from=10, to=100000)
density_pub_cs = stats::density(fund_data$t_pubs_citations[fund_data$XDIndicator=="CS"], from=10, to=100000)
density_pub_xd = stats::density(fund_data$t_pubs_citations[fund_data$XDIndicator=="XD"], from=10, to=100000)
mu <- ddply(fund_data, "XDIndicator", summarise, grp.mean=mean(t_pubs_citations))

bio_start = 1
bio_end = 174

cs_start = 1
cs_end = 194

xd_start = 1
xd_end = 215

density_df_bio = data.frame(x=density_pub_bio$x[bio_start:bio_end],
                            density=density_pub_bio$y[bio_start:bio_end] )
density_df_cs = data.frame(x=density_pub_cs$x[cs_start:cs_end],
                           density=density_pub_cs$y[cs_start:cs_end] )
density_df_xd = data.frame(x=density_pub_xd$x[xd_start:xd_end],
                           density=density_pub_xd$y[xd_start:xd_end] )

dens_df <- rbind(density_df_bio, density_df_cs, density_df_xd)
dens_df$group <- c(rep("BIO", bio_end-bio_start+1), rep("CS", cs_end-cs_start+1), rep("XD", xd_end-xd_start+1))

fig3f = ggplot(data=dens_df, aes(x=x, y=density, ymax=density, ymin=1e-6, col=group, fill=group)) + 
  geom_ribbon(color=NA, alpha = 0.5)  +
  # scale_fill_manual(values=c("green", "purple", "black"))  +
  geom_line(size=1.1) +
  # scale_color_manual(values=c(XD="black", CS="purple", BIO="green"))  + 
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian(ylim = c(1.0e-6, 1e-2), expand=FALSE) +
  geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") +
  ylab(expression(paste("PDF" ) ) ) +
  xlab("Total career citations")
# fig3f


allplot = ggarrange(fig3a, fig3b, fig3c, fig3d, fig3e, fig3f, ncol=2, nrow=3, common.legend = TRUE, legend="bottom", align="hv")
allplot

ggsave("plot3_all.png", allplot, width=10, height=15, units="in", dpi=300, scale = 0.8)
