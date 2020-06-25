###
# Test script
# This script can be used to check the numbers in the norm report in the DAB tool.
# It generates an overview for the different models.
###

library(eeptools)

gebdat <- as.Date('2006-06-21')
testdat <- as.Date('2019-01-16')
ld <- as.numeric(age_calc(gebdat,enddate=testdat,units='days'))
score=0.8525
library("devtools")
mydata = read.csv('Data_voor_casussen_voor_Daan.csv', header=TRUE, sep=";")

for (row in 1:nrow(mydata)) {
  gender <- mydata[row, "Geslacht"]
  testdate  <- mydata[row, "Testdatum"]
  birthdate  <- mydata[row, "Geboortedatum"]

  if(gender = jongen) {
    print(paste("On", date, "the stock price was", price))
  }
}

write.csv(newDataFrame, paste(newFile, "csv", sep="."), row.names=TRUE, quote=FALSE, na = "")
# Lion model with birthdate
source("Lion1_Sample.R", echo=FALSE)
print("Lions model no gender")
print("percentiel score")
pcs(ld, score)
print("betrouwbaarheidsinterval")
bi(ld, score)
print("tsore")
tscore(ld, score)
print("groei curve")
grafiek(ld, score)

# Lion model with birthdate and gender 1=girls 2=boys
source("Lion2_Sample.R", echo=FALSE)
print("Lions model with girls")
print("percentiel score")
pcs(ld, score, 1)
print("betrouwbaarheidsinterval")
bi(ld, score, 1)
print("tsore")
tscore(ld, score, 1)
print("groei curve")
grafiek(ld, score, 1)

print("Lions model with boys")
pcs(ld, score, 2)
print("betrouwbaarheidsinterval")
bi(ld, score, 2)
print("tsore")
tscore(ld,score, 2)
print("groei curve")
grafiek(ld, score, 2)
