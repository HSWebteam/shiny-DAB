###
# Test script
# This script can be used to check the numbers in the norm report in the DAB tool.
# It generates an overview for the different models.
###

library(eeptools)

gebdat <- as.Date('2014-04-21')
testdat <- as.Date('2019-04-10')
ld <- as.numeric(age_calc(gebdat,enddate=testdat,units='days'))
scoreleeuwen = .8525
scoreApen = .7641667
discrepantiescore = -7.3997
# 7.399678 
# Discrepantie model with birthdate
source("Discrepantie.R", echo = FALSE)
source("Discrep_perc.R", echo = FALSE)
print("Discrepantie model no gender")
print("percentiel score")
discrepantie5 <- disc(ld, scoreleeuwen, scoreApen, 5)
print(discrepantie5)
pcds(ld,discrepantie5[1])
discrepantie10 <- disc(ld, scoreleeuwen, scoreApen, 10)
print(discrepantie10)
pcds(ld,discrepantie10[1])

# Lion model with birthdate and gender 1=girls 2=boys
source("Discrep2_perc.R", echo = FALSE)
print("Discrepantie with girls")
print("percentiel score")
pcds2(ld, discrepantiescore, 1)
pcds2(ld, discrepantiescore, 1)

print("Discrepantie model with boys")
pcds2(ld, discrepantiescore, 2)
pcds2(ld, discrepantiescore, 2)
