# September 2014
# Short-a paper data processing

setwd('~/Desktop/Phila-short-a/')
library(plyr)

# read in speaker demographics
info <- read.csv("~/Desktop/Higher_Ed/Data/SpkInfo.csv")

# Process original coding
olda <- read.csv("~/Desktop/Higher_Ed/Data/PNC-2014-09-16.csv")
olda2 <- merge(olda, info, all.x=TRUE, all.y=FALSE)
olda3 <- droplevels(subset(olda2, 
                           !Ethnicity %in% c("", "a", "a/o", "a/w", "h", "o",
                                             "s", "s/p", "u", "b", "b/w")
                           & Age >= 18))

# Process new coding
newa <- read.csv("~/Desktop/Higher_Ed/Data/PNC-2014-09-15-newa.csv")
newa2 <- merge(newa, info, all.x=TRUE, all.y=FALSE)
newa3 <- droplevels(subset(newa2, Ethnicity == "w" & Age >= 18))


# monosyllablic function words from Selkirk 1984:352-353
# + BASIL (CMU error), THAT'S, YEAH
STOPWORDS <- c('A', 'ALL', 'AM', 'AN', 'AND', "AN'", 'ARE', "AREN'T", 
               'AS', 'AT', 'BE', 'BEEN', 'BOTH', 'BUT', 'BY', 'CAN', 
               "CAN'T", 'COULD', 'CUZ', 'DID', 'DO', 'DOES', 'DOWN', 
               'EACH', 'FOR', 'FROM', 'HAD', 'HAS', 'HAVE', 'HE', 'HER', 
               'HERE', 'HIS', 'I', 'IF', 'IN', 'IS', 'IT', 'ITS', 'LIKE', 
               'MAY', 'ME', 'MIGHT', 'MUST', 'MY', 'NO', 'NOR', 'OF', 
               'ON', 'ONE', 'OR', 'OUR', 'OUT', 'ROUND', 'SHALL', 'SHE', 
               'SHOULD', 'SINCE', 'SO', 'SOME', 'SUCH', 'THAN', 'THAT', 
               "THAT'S", 'THE', 'THEIR', 'THEM', 'THESE', 'THEY', 'THIS', 
               'THOSE', 'THROUGH', 'TILL', 'TO', 'TOO', 'UP', 'US', 'WAS', 
               'WE', 'WERE', 'WHAT', 'WHEN', 'WHO', 'WHOM', 'WHOSE', 'WHY',  
               'WILL', 'WITH', 'WOULD', 'YEAH', 'YOU', 'YOUR', 'BASIL')

olda4 <- droplevels(subset(olda3, !Word %in% STOPWORDS))
newa4 <- droplevels(subset(newa3, !Word %in% STOPWORDS))

olda5 <- ddply(olda4, .(Subject), transform, Z1=scale(F1), Z2=scale(F2), DOB=Year-Age)
newa5 <- ddply(newa4, .(Subject), transform, Z1=scale(F1), Z2=scale(F2), DOB=Year-Age)

mel <- function(x) {
  2595*log(1+(x/700))
}

olda5$M1=mel(olda5$F1)
olda5$M2=mel(olda5$F2)
newa5$M1=mel(newa5$F1)
newa5$M2=mel(newa5$F2)

olda6 <- ddply(olda5, .(Subject), transform, MZ1=scale(M1), MZ2=scale(M2))
newa6 <- ddply(newa5, .(Subject), transform, MZ1=scale(M1), MZ2=scale(M2))

oldaf <- droplevels(subset(olda6, VClass %in% c('ae','aeh','aeBR')))
newaf <- droplevels(subset(newa6, VClass %in% c('ae','aeh','aeBR')))

write.csv(oldaf, "shorta-old.csv", row.names=FALSE, quote=FALSE)
write.csv(newaf, "shorta-new.csv", row.names=FALSE, quote=FALSE)