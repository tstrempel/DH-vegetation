#script for statistical computation; do predictor 'income' and outcome 'NDVI' (as a proxy for the 'greenness' of an area)
#correlate in our data set and if so, how strong is the correlation?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("pastecs") #statistical functions

#build data set:

#read in predictor variable
Bezirke_Einkommen <- read.csv(file = './data/Einkommen_und_Preise_Nettoeinkommen_SBZ.csv', sep=";", dec=",", row.names=NULL, encoding = "UTF-8" )
OT_Einkommen <- read.csv(file = './data/Einkommen_und_Preise_Nettoeinkommen_OT.csv', sep=";", dec=",", row.names=NULL, encoding = "UTF-8" )

#read in outcome variable
Bezirke_NDVI <- read.csv(file = './data/sbz_ndvi.csv', row.names=NULL, encoding = "UTF-8")
Bezirke_EVI <- read.csv(file = './data/sbz_evi.csv', row.names=NULL, encoding = "UTF-8")

OT_NDVI <- read.csv(file = './data/ot_ndvi.csv', row.names=NULL, encoding = "UTF-8")
OT_EVI <- read.csv(file = './data/ot_evi.csv', row.names=NULL, encoding = "UTF-8")

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
#W(kr) for n= 10 and a = 0.5 is 0.938 --> conclusion: all three data rows are unlikely to be distributed normally, with Einkommen being the closest to it


#for n>50 we have to use this different test for normality:
shapiro.test(Ortsteildaten$Einkommen) #p = 0.4593
shapiro.test(Ortsteildaten$NDVI) #p = 0.9591
shapiro.test(Ortsteildaten$EVI) #p = 2.2e-16
#here we have to compare the results to p = 0.05; conclusion: normality is likely for the first two three data rows (since they are greater then 0.05)


#compute Persons r to see if predictor and outcome correlate:
#since normality is only given with data set Ortsteildaten, we can only use this data set for Pearsons r


#Persons r: 

NDVI_cor = cor(Ortsteildaten$Einkommen, Ortsteildaten$NDVI) 
NDVI_cor #result: r = -0.02544751
EVI_cor = cor(Ortsteildaten$Einkommen, Ortsteildaten$EVI) 
EVI_cor #result: r = -0.08023683

#interpretation:
#generally with r: +/- 0.1 : weak correlation
#                  +/- 0.3 : moderate correlation
#                  +/- 0.5 : strong correlation
#so for predictor 'income' and outcome 'EVI' there is at best a very, very slight correlation (those are estimates though and not proofs; further comparisons to other
#studies needed to properly contextualize these results); for predictor 'income' and outcome 'NDVI' there can be no correlation found statistically



#compute R^2 (= the so called 'determination coefficient') to estimate how much of the variation of outcome variable 'NDVI' bzw. 'EVI' is explained by predictor 'income':
r_OT_NDVI = cor.test(Ortsteildaten$Einkommen, Ortsteildaten$NDVI)
r_OT_NDVI = r_OT_NDVI[["estimate"]][["cor"]]
det_coeff_OT_NDVI = r_OT_NDVI * r_OT_NDVI
det_coeff_OT_NDVI #  result: 0.0006475757
#interpretation: only 0.06475757% of the differences in the NDVI values (compared to random distribution) is explained by the income variable
# ---> that was of course already clear, since in the previous step there was no correlation found




#simple visualization:
plot(main = "Einkommen und NDVI (Ortsteile Leipzig)", xlab= "Durchschnittseinkommen in Euro",
     ylab="NDVI", Ortsteildaten$Einkommen, Ortsteildaten$NDVI)


plot(main = "Einkommen und EVI (Ortsteile Leipzig)", xlab= "Durchschnittseinkommen in Euro",
     ylab="EVI", Ortsteildaten$Einkommen, Ortsteildaten$EVI)

#interpretation: would there be a correlation, one would see the NDVI rise along with Einkommen, but it seems to be pretty much randomly distributed
