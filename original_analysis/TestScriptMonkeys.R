###
# Test script
# This script can be used to check the numbers in the norm report in the DAB tool.
# It generates an overview for the different models.
###

library(eeptools)

gebdat <- as.Date('2006-04-12')
testdat <- as.Date('2019-01-16')
ld<-as.numeric(age_calc(gebdat,enddate=testdat,units='days'))
score=0.7641667

# Monkey model with birthdate
source("Monkey_Sample.R", echo=FALSE)
print("Monkey model no gender")
print("percentiel score")
pcs(ld, score)
print("betrouwbaarheidsinterval")
bi(ld, score)
print("tsore")
tscore(ld, score)
print("groei curve")
grafiek(ld, score)

# Lion model with birthdate and gender 1=girls 2=boys
source("Monkey2_Sample.R", echo=FALSE)
print("Monkey model with girls")
print("percentiel score")
pcs(ld, score, 1)
print("betrouwbaarheidsinterval")
bi(ld, score, 1)
print("tsore")
tscore(ld, score, 1)
print("groei curve")
grafiek(ld, score, 1)

print("Monkey model with boys")
pcs(ld, score, 2)
print("betrouwbaarheidsinterval")
bi(ld, score, 2)
print("tsore")
tscore(ld,score, 2)
print("groei curve")
grafiek(ld, score, 2)
