# 2015-02-27 heart transplant funnel plot adults 30d mortality

## code: download data, select variables for analysis ####
# check if _data directory exists, create if needed
if (!file.exists("_data")) {dir.create("_data")}

# download and read data
if (!file.exists("_data/heartTables.zip")){
fileUrl <- "http://www.srtr.org/csr/archives/201406/csrs_final_tables201406HR.zip"
download.file(fileUrl, destfile="_data/heartTables.zip", method="curl")
unzip("_data/heartTables.zip", exdir="_data")}

# read table 10 contains observed and expected
fname <- '_data/csrs_final_tables201406HR.xls'
temp <- xlsx::read.xlsx(file=fname, sheetName='Table 10', stringsAsFactors=FALSE)[-1,]
varDesc <- xlsx::read.xlsx(file=fname, sheetName='Table 10', stringsAsFactors=FALSE)[1,]
rm(fname)

# clean up variable names and change to lower case
names(temp) <- tolower(gsub("X.GSR_", "", names(temp)))
names(varDesc) <- tolower(gsub("X.GSR_", "", names(varDesc)))

# generate list of variables and descriptions, ie data dictionary
varDesc <- t(varDesc[,-1])

# select variables for analysis (adults)
# center code, expected events 1 month, observed, smr, center transplants, national transplants 
hrtAd <- temp[, Hmisc::Cs(x.ctr_cd, ad_exp_c1m, ad_obs_c1m, ad_smr_c1m, ad_n_c1m, ad_n_u1m)]

# crude convert to numeric and missings, delete centers all missing
hrtAd[, -1] <- apply(hrtAd[, -1], 2, as.numeric)
hrtAd <- hrtAd[!is.na(hrtAd[,2]),]

# delete 10 institutions 1 transplant 
# MASS::truehist(hrtAd$ad_n_c1m, nbins=30)
hrtAd <- hrtAd[hrtAd$ad_n_c1m > 2, ]

# * (end)


## code: data ####

# calculate bounds, "Exact" 95% Confidence Intervals,  (qchisq(α/2, 2*x)/2, qchisq(1-α/2, 2*(x+1))/2 )
# Ulm K. A simple method to calculate the confidence interval of a standardized mortality ratio. 
# American Journal of Epidemiology 1990;131(2):373-375.

# lower limits as %
low2alpha <- -(1 - qchisq(0.025, 2*(c(1:10)))/2/c(1:10))*100
low3alpha <- -(1 - qchisq(0.001, 2*(c(1:10)))/2/c(1:10))*100

# upper limits as %
up2alpha <- (qchisq(0.975, 2*(c(1:10)+1))/2/c(1:10) - 1)*100
up3alpha <- (qchisq(0.999, 2*(c(1:10)+1))/2/c(1:10) - 1)*100

# lower limits SMRs
low2smr <- low2alpha/100 + 1
low3smr <- low3alpha/100 + 1

# upper limits SMRS
up2smr <- up2alpha/100 + 1
up3smr <- up3alpha/100 + 1

limits <- data.frame(c(1:10), low2alpha, low3alpha, up2alpha, up3alpha, low2smr, low3smr, up2smr, up3smr)
names(limits)[1] <- "events"
limits <- limits[limits$events < 9,]

# calculate proportionate increase SMR
hrtAd$ests <- with(hrtAd, (ad_obs_c1m - ad_exp_c1m)/ad_exp_c1m * 100 )

# * (end)


## code: create plot ggplot ####
library(ggplot2)
library(scales)  

adult30d <- ggplot(data=hrtAd, aes(x=ad_exp_c1m, y=ad_smr_c1m)) +
    geom_point(origin=0)  +
    labs(x = "Expected Deaths at 30 Days", y = "SMR") +
    stat_smooth(data=limits, aes(events, low2smr), size=1, method="loess", se = FALSE) +
    stat_smooth(data=limits, aes(events, low3smr), size=1, method="loess", se = FALSE, colour="red") +
    stat_smooth(data=limits, aes(events, up2smr),  size=1, method="loess", se = FALSE) +
    stat_smooth(data=limits, aes(events, up3smr),  size=1, method="loess", se = FALSE, colour="red") + 
    scale_y_continuous(trans=sqrt_trans(), breaks=c(0:10), limits=c(0,10)) +
    geom_hline(yintercept = 1.0)
    
ggsave("adult30d.pdf", width=5, height=5)

# * (end)


## code: create plot base graphics ####
adult30dBase <- function(){
par(bty="n")
with(hrtAd, plot(ad_exp_c1m, sqrt(ad_smr_c1m), yaxt="n", ylab="SMR", xlab="Expected Deaths at 30 Days", ylim=c(0, sqrt(8))))
axis(side=2, at=sapply(c(0:8), FUN=function(x) sqrt(x)), labels=c(0:8)) 
with(limits, xspline(events, sqrt(low2smr), shape=1, lty=2))
with(limits, xspline(events, sqrt(low3smr), shape=1, lty=3))
with(limits, xspline(events, sqrt(up2smr),  shape=1, lty=2))
with(limits, xspline(events, sqrt(up3smr),  shape=1, lty=3))
abline(1, 0, col="gray")}

adult30dBase()

pdf("adult30dBase.pdf", width=5, height=5)
adult30dBase()
dev.off()

# * (end)



