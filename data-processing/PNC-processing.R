#!/usr/bin/env Rscript
# Short-a paper preliminary data processing

# setwd('~/Desktop/Phila-short-a/')

PROCESS.OLD <- TRUE
PROCESS.NEW <- FALSE

library(plyr)

# Convenience function for Mel transform
mel <- function(x) {
  2595*log(1+(x/700))
}

# read in speaker demographics
info <- read.csv("~/Desktop/Higher_Ed/Data/SpkInfo.csv")

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

if (PROCESS.OLD) {
  # Process original coding
  olda <- read.csv("~/Desktop/Higher_Ed/Data/PNC-2014-09-16.csv")
  olda2 <- merge(olda, info, all.x=TRUE, all.y=FALSE)
  olda3 <- droplevels(subset(olda2, Ethnicity == 'w' & Age >= 18))
  olda4 <- droplevels(subset(olda3, !Word %in% STOPWORDS))
  olda5 <- ddply(olda4, .(Subject), transform, Z1=scale(F1), Z2=scale(F2), 
               DOB=Year-Age)
  olda6 <- transform(olda5, M1=mel(F1), M2=mel(F2))
  olda7 <- ddply(olda6, .(Subject), transform, MZ1=scale(M1), MZ2=scale(M2))
  olda8 <- droplevels(subset(olda7, VClass %in% c('ae','aeh','aeBR') & Stress == 1))
  # Remove speakers with less than 5 tokens in each category
  #for (speaker in levels(olda8$Subject)) {
  #  data <- subset(olda10, Subject==speaker)
  #  ae.d <- data[data$VClass=="ae", ]
  #  aeh.d <- data[data$VClass=="aeh", ]
  #  if(nrow(ae.d) < 5 | nrow(aeh.d) < 5) print(speaker)
  #}
  oldaf <- droplevels(subset(olda8, !Subject %in% c("PH10-2-10", "PH10-2-11", "PH74-00-6", 
                             "PH78-5-3", "PH79-3-8", "PH79-4-5", "PH80-2-8", "PH84-1-3", 
                             "PH84-2-10", "PH85-3-8", "PH86-3-3", "PH90-2-2", "PH91-2-19", 
                             "PH91-2-21", "PH91-2-3", "PH91-2-8", "PH94-1-1")))
  write.csv(oldaf, "shorta-old.csv", row.names=FALSE, quote=FALSE)
}

if (PROCESS.NEW) {
  # Process final new coding
  old <- read.csv("~/Desktop/Higher_Ed/Data/PNC-2014-09-16.csv")
  old2 <- droplevels(subset(olda, !VClass %in% c('ae', 'aeh', 'aeBR')))
  newa <- read.csv("shorta-new-10-07-14.csv")
  # stitch the new short-a tokens to the old data so we can normalize
  newa <- rbind(newa, olda2)
  newa2 <- merge(newa, info, all.x=TRUE, all.y=FALSE)
  newa3 <- droplevels(subset(newa2, Ethnicity == 'w' & Age >= 18))
  newa4 <- droplevels(subset(newa3, !Word %in% STOPWORDS))
  newa5 <- ddply(newa4, .(Subject), transform, Z1=scale(F1), Z2=scale(F2), DOB=Year-Age)
  newa6 <- transform(newa5, M1=mel(F1), M2=mel(F2))
  newa7 <- ddply(newa6, .(Subject), transform, MZ1=scale(M1), MZ2=scale(M2))
  newa8 <- droplevels(subset(newa7, VClass %in% c('ae','aeh','aeBR')))
  # Remove speakers with less than 5 tokens in each category  
  newaf <- droplevels(subset(newa8, !Subject %in% c("PH10-2-11", "PH74-00-6", "PH77-5-1", "PH78-5-3", 
                                                    "PH79-3-8", "PH80-2-8", "PH84-2-10", "PH85-3-8",
                                                    "PH86-3-3", "PH90-2-2", "PH91-2-19", "PH91-2-21",
                                                    "PH91-2-3")))
  write.csv(newaf, "shorta-new.csv", row.names=FALSE, quote=FALSE)
}