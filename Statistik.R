#script for statistical computation; do predictor 'income' and outcome 'NDVI' (as a proxy for the 'greenness of an area)
#correlate and if so, how strong is the correlation?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("pastecs")

#build data set:

#read in predictor variable
Bezirke_Einkommen <- read.csv(file = './data/Einkommen_und_Preise_Nettoeinkommen_SBZ.csv', sep=";", dec=",", row.names=NULL, encoding = "UTF-8" )
OT_Einkommen <- read.csv(file = './data/Einkommen_und_Preise_Nettoeinkommen_OT.csv', sep=";", dec=",", row.names=NULL, encoding = "UTF-8" )

#read in outcome variable
Bezirke_NDVI <- read.csv(file = './data/mean_sbz_ndvi.csv', row.names=NULL, encoding = "UTF-8")
Bezirke_EVI <- read.csv(file = './data/mean_sbz_evi.csv', row.names=NULL, encoding = "UTF-8")

OT_NDVI <- read.csv(file = './data/mean_ot_ndvi.csv', row.names=NULL, encoding = "UTF-8")
OT_EVI <- read.csv(file = './data/mean_ot_evi.csv', row.names=NULL, encoding = "UTF-8")

#merge:
Ortsteildaten_tmp <- merge(OT_Einkommen, OT_NDVI )
Bezirksdaten_tmp <- merge(Bezirke_Einkommen, Bezirke_NDVI)

Ortsteildaten <- merge(Ortsteildaten_tmp, OT_EVI) #final data set for Ortsteile
Bezirksdaten <- merge(Bezirksdaten_tmp, Bezirke_EVI) #final data set for Bezirke


#test for normality (=requirement for Pearsons r):
#for n<50 (Bezirksdaten): Shapiro-Wilk-Test (compare W to W(kr) from Shapiro-Wilk table (with n = 10 and p = 0.5); if W > W(kr) then probably normality of data);
#for n>50 (Ortsteildaten): Test by Patrick Royston (1982); confusingly, the test in R is called 'shapiro.test', but is not the actual Shapiro-Wilk Test; here, if p<0.05
#then normality is unlikely

#for n<50 we can use a this test for normality:

round(stat.desc(Bezirksdaten$Einkommen, basic = FALSE, norm = TRUE), digits=3) #normtest.W is the result of the Shapiro-Wilk Test for n<50 -->0.924
round(stat.desc(Bezirksdaten$NDVI, basic = FALSE, norm = TRUE), digits=3) #normtest.W is the result of the Shapiro-Wilk Test for n<50 -->0.843
round(stat.desc(Bezirksdaten$EVI, basic = FALSE, norm = TRUE), digits=3) #normtest.W is the result of the Shapiro-Wilk Test for n<50 -->0.886
#W(kr) for n= 10 and a = 0.5 is 0.938 --> conclusion: all three data rows are unlikely to be distributed normally


#for n>50 we have to use this different test for normality:
shapiro.test(Ortsteildaten$Einkommen) #p = 0.4593
shapiro.test(Ortsteildaten$NDVI) #p = 0.9522
shapiro.test(Ortsteildaten$EVI) #p = 0.7283
#conclusion: normality is likely for all three data rows




#compute Persons r to see if predictor and outcome correlate:
#since normality is only given with data set Ortsteildaten, we can only use this data set for Pearsons r


#Persons r: 
r_OT_NDVI = cor.test(Ortsteildaten$Einkommen, Ortsteildaten$NDVI)[4] 
r_OT_NDVI = r_OT_NDVI[["estimate"]][["cor"]] #result: r = -0.04909352 
r_OT_NDVI
r_OT_EVI = cor.test(Ortsteildaten$Einkommen, Ortsteildaten$EVI)[4]  
r_OT_EVI = r_OT_EVI[["estimate"]][["cor"]]
r_OT_EVI #result: r = -0.005783995


#interpretation:
#generally with r: +/- 0.1 : weak correlation
#                  +/- 0.3 : moderate correlation
#                  +/- 0.5 : strong correlation
#so for predictor 'income' and outcome 'NDVI' there is only a very, very slight correlation (those are estimates though and not proofs; further comparisons to other
#studies needed to properly contextualize these results); for predictor 'income' and outcome 'EVI' there can be no correlation found statistically



#Test result for significance:
#-----------------####still TODO######-----------------------



#compute R^2 (= the so called 'determination coefficient') to estimate how much of the variation of outcome variable 'NDVI' bzw. 'EVI' is explained by predictor 'income':
det_coeff_OT_NDVI = r_OT_NDVI * r_OT_NDVI
det_coeff_OT_NDVI #  result: 0.002410174
#interpretation: only 0.0025% of the differences in the NDVI values from Ortsteil to Ortsteil is explained by the income variable

det_coeff_OT_EVI = r_OT_EVI * r_OT_EVI
det_coeff_OT_EVI





