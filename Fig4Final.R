getwd()
setwd("D:/bwm03/Documents/Graduate Classes/Statistical Methods R")

genomics <- read.csv("Faculty_GoogleScholar_Funding_Data_N4190.csv", header = TRUE)
genomics$t_pubs_citations -> Ci #log transform all but Chi
genomics$SchoolRank -> sr
genomics$h_index -> hi
genomics$t_deflated_nsf -> tdnsf
genomics$num_nsf -> nnsf
genomics$t_deflated_nih -> tdnih
genomics$num_nih -> nnih
genomics$PRCentrality -> prc
chi <- genomics$Chi
xdi <- genomics$XDIndicator ##dummy variables
y05yr <- genomics$Y05yr ## dummy variables for 5 year categories

###create xdi data frame 
dum <- list(xdi)
dum <- data.frame(dum)
dum

###create dummy variables for y05yr
one <- ifelse(y05yr == 1945, 1, 0)
two <- ifelse(y05yr == 1950, 1, 0)
three <- ifelse(y05yr == 1955, 1, 0)
four <- ifelse(y05yr == 1960, 1, 0)
five <- ifelse(y05yr == 1965, 1, 0)
six <- ifelse(y05yr == 1970, 1, 0)
seven <- ifelse(y05yr == 1975, 1, 0)
eight <- ifelse(y05yr == 1980, 1, 0)
nine <- ifelse(y05yr == 1985, 1, 0)
ten <- ifelse(y05yr == 1990, 1, 0)
eleven <- ifelse(y05yr == 1995, 1, 0)
twelve <- ifelse(y05yr == 2000, 1, 0)
thirteen <- ifelse(y05yr == 2005, 1, 0)
fourteen <- ifelse(y05yr == 2010, 1, 0)


### replace all 0s with 0.0001 in the following columns
tdnsf[tdnsf==0] <- 0.00001
nnsf[nnsf==0] <- 0.00001
tdnih[tdnih==0] <- 0.00001
nnih[nnih==0] <- 0.00001
prc[prc==0] <- 0.00001

###log transform variables where necessary
Ci <- log(Ci)
sr <- log(sr)
hi <- log(hi)
tdnsf <- log(tdnsf)
nnsf <- log(nnsf)
nnih <- log(nnih)
prc <- log(prc)

###create dummy variables for XDI
library(dummies)
XDI <- dummy.data.frame(dum, sep = " ")
dxdi <- dummy(xdi, sep = ".")
dxdi

###insert the dummy data frame dxdi into the data frame with the transformed variables
genomic <- data.frame(Ci, sr, hi, tdnsf, nnsf, tdnih, nnih, prc, chi, dxdi, one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirteen, fourteen)
genomic

###rename dummy columns
bio <- genomic$xdi.BIO
cs <- genomic$xdi.CS
xd <- genomic$xdi.XD


###put together the data frame
genomic <- data.frame(Ci, sr, hi, tdnsf, nnsf, tdnih, nnih, prc, chi, bio, cs, xd, one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirteen, fourteen)
genomic

###run the model
genomicmodel <- lm(genomic$Ci ~., data = genomic)
summary(genomicmodel)

###Tried to Scale the data - it came out really weird
genomicscale <- lm(scale(genomic$Ci) ~ scale(sr) + scale(hi) + scale(tdnsf) + scale(nnsf) + scale(tdnih) + scale(nnih) + scale(prc) + scale(chi) + factor(bio) + factor(cs) + factor(xd) + factor(one) + factor(two) + factor(three) + factor(four) + factor(five) + factor(six) + factor(seven) + factor(eight) + factor(nine) + factor(ten) + factor(eleven) + factor(twelve) + factor(thirteen) + factor(fourteen), data = genomic)
summary(genomicscale)


###check summary data with table s2 when done... within 5% of example data

###sjstats for standardized beta
install.packages("sjstats")
library(sjstats)
std_beta(genomicmodel)
std_beta(genomicscale)

###plot figure 4
library(ggplot2)
library(grid)


###Creat data frame using values from the regression analysis
df <- data.frame(
  trt = factor(c(1, 2, 3, 4, 5, 6, 7)),
  resp = c(-0.046, -0.05718, 0.04308, -.007648, 0.0182813, 0.02303, 0.065367),
  upper = c(-0.04054, -0.033252, 0.06688, -0.001864, 0.0243823, 0.029785, 0.07225),
  lower = c(-0.05146, -0.081108, 0.01928, -0.013432, 0.0121803, 0.016275, 0.058484009)
)

x_labs <- c( expression( bolditalic( beta[paste("r")] ) ), #1
             expression( bolditalic( beta[paste("$1")] ) ), #2
             expression( bolditalic( beta[paste("N1")] ) ), #3
             expression( bolditalic( beta[paste("$2")] ) ), #4
             expression( bolditalic( beta[paste("N2")] ) ), #5
             expression( bolditalic( beta[paste("C") ^ "PR"] ) ), #6
             expression( bolditalic( beta[paste(chi)] ) )  #7
)

p <- ggplot(df, aes(x=trt, y=resp))
p = p + geom_pointrange(aes(ymin = lower, ymax = upper), fatten = 5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept=0, linetype=2) +
  xlab("") + ylab("Standardized regression coefficients") +
  scale_x_discrete(breaks=1:7, labels=x_labs ) + theme_light() +
  theme(axis.text.x = element_text(size=20, face="bold",family="serif" ),
        axis.text.y = element_text(size=20, face="bold" ),
        axis.title = element_text(size=20, face="bold" ),
        plot.margin = unit(c(6,2,2,2), "lines") ) 

lg = linesGrob(gp=gpar(lwd=3))

p = p +
  annotation_custom(grob = textGrob("CV"),  xmin = 3, xmax = 3, ymin = 0.115, ymax = 0.115) +
  annotation_custom(grob = lg, xmin = 0.7, xmax = 5.3, ymin = 0.1, ymax = 0.1) +
  annotation_custom(grob = lg, xmin = 0.7, xmax = 0.7, ymin = 0.095, ymax = 0.1) +
  annotation_custom(grob = lg, xmin = 5.3, xmax = 5.3, ymin = 0.095, ymax = 0.1)

p = p +
  annotation_custom(grob = textGrob("Network"),  xmin = 6.5, xmax = 6.5, ymin = 0.115, ymax = 0.115) +
  annotation_custom(grob = lg, xmin = 5.7, xmax = 7.3, ymin = 0.1, ymax = 0.1) +
  annotation_custom(grob = lg, xmin = 5.7, xmax = 5.7, ymin = 0.095, ymax = 0.105) +
  annotation_custom(grob = lg, xmin = 7.3, xmax = 7.3, ymin = 0.095, ymax = 0.105)

tg = textGrob("***", rot = 90)

p = p + 
  annotation_custom(grob = tg,  xmin = 1, xmax = 1, ymin = 0.087, ymax = 0.087) +
  annotation_custom(grob = tg,  xmin = 4, xmax = 4, ymin = 0.087, ymax = 0.087) +
  annotation_custom(grob = tg,  xmin = 5, xmax = 5, ymin = 0.087, ymax = 0.087) + 
  annotation_custom(grob = tg,  xmin = 7, xmax = 7, ymin = 0.087, ymax = 0.087)

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
