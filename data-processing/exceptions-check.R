#!/usr/bin/env Rscript

##### Analysis Step 2: check old variable class for exceptions #####
# 1. Import the old short-a data
# 2. Use Mahalanobis to verify or recategorize FAVE 'aeBR' class

library(ggplot2)
# setwd('~/Desktop/Phila-short-a/')
oldaf <- read.csv("shorta-old.csv")
MAKE.PDF <- FALSE
# how we invert things
inv <- function(X) solve(X)

philasys <- droplevels(subset(oldaf, !Subject %in% c('IHP1-1', 'IHP1-2',
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
                       'PHI-R-2', 'PHI-R-4', 'PHI-R-5', 'PHI-R-6', 'PHI-R-7',
                       # This last group has the Philly system, we thought,
                       # but not enough tokens (5) to estimate stable means
                       # for the per-speaker Mahalanobis distance stuff.
                       # If we end up NOT using the per-speaker measures,
                       # these folks should probably be added back in. --KG
                       'PH79-3-8', 'PH79-4-5', 'PH85-3-8', 'PH91-2-3',
                       "PH10-2-10", "PH80-2-8", "PH84-2-10",
                       "PH86-3-3", "PH90-2-2")))

aeBR <- droplevels(subset(philasys, VClass=='aeBR'))
# calculate mahalanobis distance for all the words
out <- data.frame(mahal.ae=1.0, mahal.aeh=1.0)
for (speaker in levels(aeBR$Subject)) {
  data <- subset(philasys, Subject==speaker)
  words <- subset(aeBR, Subject==speaker, select=c(MZ1,MZ2))
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
aeBR.mahal <- cbind(aeBR, out2, row.names=NULL)
aeBR.mahal$closer <- as.factor(with(aeBR.mahal, 
                                   ifelse(mahal.ae < mahal.aeh, "ae", "aeh")))

# Print list of words and code frequency
table <- as.data.frame(with(aeBR.mahal, table(Word, closer)))
table2 <- cast(table, Word~closer)
# Remove words with less than 5 tokens
d <- subset(table2, ae + aeh >= 5)
# Remove sC cluster tokens already analyzed
d <- subset(d, !Word %in% clusters)
# Compute log ratio
d$log.ratio <- with(d, log2(ae) - log2(aeh))
# Recode
d$new.code <- as.factor(with(d, ifelse(log.ratio <= -1, 'aeh',
                                       ifelse(log.ratio >= 1, 'ae', 'aeBR'))))

sink('aeBR.mahal.txt')
d
sink()

if (MAKE.PDF) {
  pdf(file = "aeBRwords.pdf", width=6, height=5, onefile=TRUE)
  for (spk in levels(aeBR.mahal$Subject)) {
    print(ggplot(data=subset(aeBR.mahal, Subject==spk), 
                 aes(MZ2, MZ1, label=Word, color=closer)) +
            geom_text(size=2) +
            geom_text(data=subset(philasys, Subject==spk & VClass!='aeBR'), 
                      aes(MZ2, MZ1, label=Word, color=VClass), alpha=.25, size=2)+
            scale_color_manual(values=c('aeh'='red', 'ae'='blue'))+
            scale_x_reverse() +
            scale_y_reverse() +
            labs(title=spk) +
            theme_bw())
  }
  dev.off()
}

