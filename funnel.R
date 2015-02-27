# 2015-02-27 heart transplant


## code: download data, select variables for analysis ####
# download and read data
fileUrl <- "http://www.srtr.org/csr/archives/201406/csrs_final_tables201406HR.zip"
download.file(fileUrl, destfile="_data/heartTables.zip", method="curl")
unzip("_data/heartTables.zip", exdir="_data")

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
# Ulm K. A simple method to calculate the confidence interval of a standardized mortality ratio. American Journal of Epidemiology 1990;131(2):373-375.

# lower limits
low2alpha <- -(1 - qchisq(0.025, 2*(c(1:10)))/2/c(1:10))*100
low3alpha <- -(1 - qchisq(0.001, 2*(c(1:10)))/2/c(1:10))*100

# upper limits
up2alpha <- (qchisq(0.975, 2*(c(1:10)+1))/2/c(1:10) - 1) * 100
up3alpha <- (qchisq(0.999, 2*(c(1:10)+1))/2/c(1:10) - 1) * 100

# calculate proportionate increase SMR
hrtAd$ests <- with(hrtAd, (ad_obs_c1m - ad_exp_c1m)/ad_exp_c1m * 100 )

# * (end)


## code: create plot ####


# * (end)






