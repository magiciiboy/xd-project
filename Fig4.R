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
lm(scale(your.y) ~ scale(your.x), data=your.Data)
genomicscale <- lm(scale(genomic$Ci) ~ scale(sr + hi + tdnsf + nnsf + tdnih + nnih + prc + chi + bio + cs + xd + one + two + three + four + five + six + seven + eight + nine + ten + eleven + twelve + thirteen + fourteen), data = genomic)
summary(genomicscale)

###check summary data with table s2 when done... within 5% of example data

###plot it

