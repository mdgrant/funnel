# 2015-02-27 heart transplant funnel plot adults 30d and 1y SMRs 


## code: download data, select variables for analysis ####
# check if _data directory exists, create if needed
if (!file.exists("_data")) {
    dir.create("_data")
}

# download and read data
if (!file.exists("_data/heartTables.zip")) {
    fileUrl <- "http://www.srtr.org/csr/archives/201406/csrs_final_tables201406HR.zip"
    download.file(fileUrl, destfile="_data/heartTables.zip", method="curl")
    unzip("_data/heartTables.zip", exdir="_data")
}

# read table 10 contains observed and expected
fname <- '_data/csrs_final_tables201406HR.xls'
temp <- xlsx::read.xlsx(file=fname, sheetName='Table 10', stringsAsFactors=FALSE)[-1,]
varDesc <- xlsx::read.xlsx(file=fname, sheetName='Table 10', stringsAsFactors=FALSE)[1,]
rm(fname)

# clean up variable names and change to lower case
names(temp) <- tolower(gsub("X.GSR_", "", names(temp)))
temp <- data.frame(apply(temp, 2, function(x) gsub("\\s", "", x)), stringsAsFactors=F)
names(temp)[1] <- "inst"
names(varDesc) <- tolower(gsub("X.GSR_", "", names(varDesc)))

# generate list of variables and descriptions, ie data dictionary
varDesc <- t(varDesc[,-1])

# select variables for analysis (adults)
# center code, expected events 1 month, observed, smr, center transplants, national transplants 
vars <- Hmisc::Cs(inst,x.ctr_cd,ad_exp_c1m,ad_obs_c1m,ad_smr_c1m,ad_n_c1m,ad_n_u1m,ad_exp_c1y,ad_obs_c1y,ad_smr_c1y,ad_n_c1y,ad_n_u1y,ad_smrlcl_c1y,ad_smrucl_c1y)
hrtAd <- temp[, vars]

# crude convert to numeric and missings, delete centers all missing
hrtAd[, -c(1,2)] <- apply(hrtAd[, -c(1,2)], 2, as.numeric)
hrtAd <- hrtAd[!is.na(hrtAd[,2]),]

# delete 10 institutions 1 transplant 
# MASS::truehist(hrtAd$ad_n_c1m, nbins=30)
hrtAd <- hrtAd[hrtAd$ad_n_c1m > 2, ]

hrtAd <- hrtAd[!is.na(hrtAd$ad_smr_c1y),]

# * (end)


## code: data ####

# calculate bounds, "Exact" 95% Confidence Intervals,  (qchisq(α/2, 2*x)/2, qchisq(1-α/2, 2*(x+1))/2 )
# Ulm K. A simple method to calculate the confidence interval of a standardized mortality ratio. 
# American Journal of Epidemiology 1990;131(2):373-375.

xAxis <- c(1:20)
# lower limits as %
low2alpha <- -(1 - qchisq(0.025, 2*(xAxis))/2/xAxis)*100
low3alpha <- -(1 - qchisq(0.001, 2*(xAxis))/2/xAxis)*100

# upper limits as %
up2alpha <- (qchisq(0.975, 2*(xAxis+1))/2/xAxis - 1)*100
up3alpha <- (qchisq(0.999, 2*(xAxis+1))/2/xAxis - 1)*100

# lower limits SMRs
low2smr <- low2alpha/100 + 1
low3smr <- low3alpha/100 + 1

# upper limits SMRS
up2smr <- up2alpha/100 + 1
up3smr <- up3alpha/100 + 1

limits <- data.frame(xAxis, low2alpha, low3alpha, up2alpha, up3alpha, low2smr, low3smr, up2smr, up3smr)
names(limits)[1] <- "events"
# limits <- limits[limits$events < 9,]

# calculate proportionate increase SMR
hrtAd$ests <- with(hrtAd, (ad_obs_c1m - ad_exp_c1m)/ad_exp_c1m * 100 )

# * (end)


## code: create plot base graphics 30d ####

adult30dBase <- function(){
    par(bty="n")
    with(hrtAd, plot(ad_exp_c1m, sqrt(ad_smr_c1m), yaxt="n", ylab="SMR", xlab="Expected Deaths at 30 Days", ylim=c(0, sqrt(8))))
    axis(side=2, at=sapply(c(0:8), function(x) sqrt(x)), labels=c(0:8)) 
    with(limits, xspline(events, sqrt(low2smr), shape=1, lty=2))
    with(limits, xspline(events, sqrt(low3smr), shape=1, lty=3))
    with(limits, xspline(events, sqrt(up2smr),  shape=1, lty=2))
    with(limits, xspline(events, sqrt(up3smr),  shape=1, lty=3))
    abline(1, 0, col="gray")
    text(8, 1.7, "99.8% limit", cex=.7)
    text(7.95, 1.47, "95% limit", cex=.7)
}

adult30dBase()

pdf("adult30dBase.pdf", width=7.5, height=7.55)
adult30dBase()
dev.off()

# * (end)


## code: create plot base graphics 1y ####

adult1yBase <- function(){
    par(bty="n")
    with(hrtAd, plot(ad_exp_c1y, sqrt(ad_smr_c1y), yaxt="n", ylab="SMR", xlab="Expected Deaths at 1 Year", xlim=c(0,20), ylim=c(0, sqrt(8))))
    axis(side=2, at=sapply(c(0:8), function(x) sqrt(x)), labels=c(0:8)) 
    with(limits, xspline(events, sqrt(low2smr), shape=1, lty=2))
    with(limits, xspline(events, sqrt(low3smr), shape=1, lty=3))
    with(limits, xspline(events, sqrt(up2smr),  shape=1, lty=2))
    with(limits, xspline(events, sqrt(up3smr),  shape=1, lty=3))
    abline(1, 0, col="gray")
    text(18, 1.45, "99.8% limit", cex=.7)
    text(17.8, 1.3,  "95% limit", cex=.7)
}

adult1yBase()

pdf("adult1ydBase.pdf", width=7.5, height=7.5)
adult1yBase()
dev.off()

# * (end)


## code: dotplot with ci by facility ####
library(ggplot2)
hrtAd$newLab <- reorder(hrtAd$x.ctr_cd, hrtAd$ad_smr_c1y)
hrtAd$newUp <- with(hrtAd, ifelse(ad_smrucl_c1y>10, 10, ad_smrucl_c1y))

ggplot(data=hrtAd[hrtAd$ad_smr_c1y < 7, ], aes(x=ad_smr_c1y, y=newLab)) +
    geom_point() +
    geom_errorbarh(aes(xmin=ad_smrlcl_c1y, xmax=newUp), height=0.1, size=.35) +
    geom_vline(xintercept = 1) +
    scale_x_continuous(limits=c(0, 10), breaks=c(0:10)) +
    labs(x = "SMR, 95% CI (truncated at limit of 10)", size=.5, y = "Center") +
    theme(axis.text.y = element_text(size = 7)) 

ggsave("adult1yCis.pdf", width=5, height=10)

# * (end)


## code: create plot ggplot ####
# library(ggplot2)
# library(scales)  
# 
# adult30d <- ggplot(data=hrtAd, aes(x=ad_exp_c1m, y=ad_smr_c1m)) +
#     geom_point(origin=0)  +
#     labs(x = "Expected Deaths at 30 Days", y = "SMR") +
#     stat_smooth(data=limits, aes(events, low2smr), size=1, method="loess", se = FALSE) +
#     stat_smooth(data=limits, aes(events, low3smr), size=1, method="loess", se = FALSE, colour="red") +
#     stat_smooth(data=limits, aes(events, up2smr),  size=1, method="loess", se = FALSE) +
#     stat_smooth(data=limits, aes(events, up3smr),  size=1, method="loess", se = FALSE, colour="red") + 
#     scale_y_continuous(trans=sqrt_trans(), breaks=c(0:10), limits=c(0,10)) +
#     geom_hline(yintercept = 1.0)
# 
# adult30d
# 
# ggsave("adult30d.pdf", width=5, height=5)
# * (end)

