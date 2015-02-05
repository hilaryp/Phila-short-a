#!/usr/bin/env Rscript

library(plyr)
library(ggplot2)
library(reshape)

mel <- function(x) {
  2595*log(1+(x/700))
}
inv <- function(X) solve(X)

newa <- read.csv("shorta-new-10-03-14.csv")
newa2 <- droplevels(subset(newa, VClass %in% c('ae', 'aeh', 'aeBR')))

olda <- read.csv("~/Desktop/Higher_Ed/Data/PNC-2014-09-16.csv")
olda2 <- droplevels(subset(olda, !VClass %in% c('ae', 'aeh', 'aeBR')))

# stitch the new short-a tokens to the old data so we can normalize
new.a <- rbind(newa2, olda2)

# do all the same preprocessing
info <- read.csv("~/Desktop/Higher_Ed/Data/SpkInfo.csv")
new.a <- merge(new.a, info, all.x=TRUE, all.y=FALSE)
new.a2 <- droplevels(subset(new.a, Ethnicity == 'w' & Age >= 18))
new.a2$Speaker <- NULL
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

new.a3 <- droplevels(subset(new.a2, !Word %in% STOPWORDS))
new.a3 <- transform(new.a3, M1=mel(F1), M2=mel(F2))
new.a3 <- ddply(new.a3, .(Subject), transform, MZ1=scale(M1), MZ2=scale(M2))
new.a4 <- droplevels(subset(new.a3, VClass %in% c('ae','aeh','aeBR')))

write.csv(new.a4, "shorta-new.csv", row.names=FALSE, quote=FALSE)

if (MAKE.PDF) {
  pdf(file = "newa.pdf", width=6, height=5, onefile=TRUE)
  for (spk in levels(new.a4$Subject)) {
    print(ggplot(data=subset(new.a4, Subject==spk), 
                 aes(MZ2, MZ1, label=Word, color=VClass)) +
            geom_text(size=2) +
            scale_color_manual(values=c('aeh'='red', 'ae'='blue', 
                                        'aeBR'='green'))+
            scale_x_reverse() +
            scale_y_reverse() +
            labs(title=spk) +
            theme_bw())
  }
  dev.off()
}

# more processing for doing final lex exception check

#for (speaker in levels(new.a4$Subject)) {
#  data <- subset(new.a4, Subject==speaker)
#  ae.d <- data[data$VClass=="ae", ]
#  aeh.d <- data[data$VClass=="aeh", ]
#  if(nrow(ae.d) < 5 | nrow(aeh.d) < 5) print(speaker)
#}

new.a5 <- droplevels(subset(new.a4, !Subject %in% c("PH10-2-11", "PH74-00-6",
                     "PH77-5-1", "PH78-5-3", "PH79-3-8", "PH80-2-8",
                     "PH84-2-10", "PH85-3-8", "PH86-3-3", "PH90-2-2",
                     "PH91-2-19", "PH91-2-21", "PH91-2-3") & Stress==1))

philasys <- droplevels(subset(new.a5, !Subject %in% c('IHP1-1', 'IHP1-2',
                                                     'IHP1-4', 'IHP1-5', 'IHP2-1', 'IHP2-11', 'IHP2-12',
                                                     'IHP2-13', 'IHP2-14', 'IHP2-15', 'IHP2-16', 'IHP2-17', 
                                                     'IHP2-19', 'IHP2-2', 'IHP2-22', 'IHP2-24', 'IHP2-25', 
                                                     'IHP2-26', 'IHP2-3', 'IHP2-30', 'IHP2-35', 'IHP2-38', 
                                                     'IHP2-39', 'IHP2-4', 'IHP2-40', 'IHP2-42', 'IHP2-44', 
                                                     'IHP2-45', 'IHP2-48', 'IHP2-6', 'IHP2-7', 'IHP2-8',
                                                     'IHP2-9', 'PH00-1-1', 'PH00-1-5', 'PH04-3-2', 
                                                     'PH04-3-3', 'PH06-1-2', 'PH06-2-6', 'PH10-1-4', 
                                                     'PH10-1-5', 'PH10-1-7', 'PH10-2-11', 'PH10-2-6',
                                                     'PH10-2-8', 'PH74-0-1', 'PH74-0-4', 'PH74-0-5', 
                                                     'PH74-00-6', 'PH74-1-2', 'PH74-2-4', 'PH74-2-8', 
                                                     'PH76-4-6', 'PH77-3-4', 'PH78-5-3', 'PH80-2-7', 
                                                     'PH84-2-1', 'PH84-2-4', 'PH85-2-2', 'PH87-1-3', 
                                                     'PH91-2-18', 'PH91-2-19', 'PH91-2-20', 'PH91-2-21',
                                                     'PH91-2-4', 'PH91-2-8', 'PH92-2-1', 'PH97-3-2',
                                                     'PH97-3-5', 'PHI-M-1', 'PHI-M-2', 'PHI-M-4', 'PHI-R-1', 
                                                     'PHI-R-2', 'PHI-R-4', 'PHI-R-5', 'PHI-R-6', 'PHI-R-7')))

# calculate mahalanobis distance for all the words
out <- data.frame(mahal.ae=1.0, mahal.aeh=1.0)
for (speaker in levels(philasys$Subject)) {
  data <- subset(philasys, Subject==speaker)
  words <- subset(philasys, Subject==speaker, select=c(MZ1,MZ2))
  # get means & covariances for the speaker's ae and aeh
  ae.d <- data[data$VClass=="ae", ]
  ae <- cbind(ae.d$MZ1, ae.d$MZ2)
  ae.mu <- colMeans(ae)
  ae.icov <- inv(cov(ae))
  aeh.d <- data[data$VClass=="aeh", ]
  aeh <- cbind(aeh.d$MZ1, aeh.d$MZ2)
  aeh.mu <- colMeans(aeh)
  aeh.icov <- inv(cov(aeh))
  for (i in 1:nrow(words)) {
    word <- words[i, ]
    mahal.ae <- mahalanobis(x=word, center=ae.mu, cov=ae.icov, 
                            inverted=TRUE)
    mahal.aeh <- mahalanobis(x=word, center=aeh.mu, cov=aeh.icov, 
                             inverted=TRUE)
    mahals <- cbind(mahal.ae, mahal.aeh)
    out <- rbind(out, mahals)
  }
}

out2 <- out[-1,]
all.mahal <- cbind(philasys, out2, row.names=NULL)
all.mahal$closer <- as.factor(with(all.mahal, 
                                    ifelse(mahal.ae < mahal.aeh, "ae", "aeh")))

# Print list of words and code frequency
table <- as.data.frame(with(all.mahal, table(Word, closer, VClass)))
table2 <- cast(table, Word+VClass~closer)
# Remove words with less than 5 tokens
d <- subset(table2, ae + aeh >= 5)
# Compute log ratio
d$log.ratio <- with(d, log2(ae) - log2(aeh))
# Recode
d$new.code <- as.factor(with(d, ifelse(log.ratio <= -1, 'aeh',
                                       ifelse(log.ratio >= 1, 'ae', 'aeBR'))))
d <- subset(d, VClass!=new.code)

sink('aeBR.mahal2.txt')
d
sink()