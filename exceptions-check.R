#!/usr/bin/env Rscript

##### Analysis Step 2: check for lexical exceptions #####
# 1. Import the NEW short-a data
# 2. Use Mahalanobis to look for new lexical exceptions

library(ggplot2)
# setwd('~/Desktop/Phila-short-a/')
newaf <- read.csv("shorta-new.csv")
MAKE.PDF <- FALSE
# how we invert things
inv <- function(X) solve(X)

newaf2 <- droplevels(subset(newaf, VClass != "aeBR"))
philasys <- droplevels(subset(newaf2, !Subject %in% c('IHP1-1', 'IHP1-2',
                              'IHP1-4', 'IHP1-5', 'IHP2-1', 'IHP2-11', 'IHP2-12',
                              'IHP2-13', 'IHP2-14', 'IHP2-15', 'IHP2-16', 
                              'IHP2-17', 'IHP2-19', 'IHP2-2', 'IHP2-22', 
                              'IHP2-24', 'IHP2-25', 'IHP2-26', 'IHP2-3', 
                              'IHP2-30', 'IHP2-35', 'IHP2-38', 'IHP2-39', 
                              'IHP2-4', 'IHP2-40', 'IHP2-42', 'IHP2-44', 
                              'IHP2-45', 'IHP2-48', 'IHP2-6', 'IHP2-7', 'IHP2-8',
                              'IHP2-9', 'PH04-3-2', 'PH04-3-3', 'PH06-2-6', 
                              'PH10-1-4', 'PH10-1-5', 'PH10-1-7', 'PH10-2-6',
                              'PH10-2-8', 'PH74-0-4', 'PH74-0-5', 'PH74-00-6',
                              'PH74-2-4', 'PH74-2-8', 'PH76-4-6', 'PH78-5-3',
                              'PH84-2-1', 'PH87-1-3', 'PH91-2-19', 'PH91-2-20', 
                              'PH91-2-21','PH91-2-8', 'PH92-2-1', 'PH97-3-2',
                              'PHI-M-1', 'PHI-M-2', 'PHI-M-4', 'PHI-R-1', 
                              'PHI-R-2', 'PHI-R-4', 'PHI-R-5', 'PHI-R-6', 'PHI-R-7',
                              # This last group has the Philly system, we thought,
                              # but not enough tokens (5) to estimate stable means
                              # for the per-speaker Mahalanobis distance stuff.
                              # If we end up NOT using the per-speaker measures,
                              # these folks should probably be added back in. --KG
                              'PH79-3-8', 'PH79-4-5', 'PH85-3-8', 'PH91-2-3',
                              "PH10-2-10", "PH10-2-11", "PH80-2-8", "PH84-2-10",
                              "PH86-3-3", "PH90-2-2")))

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
all.mahal$exceptions <- as.factor(with(all.mahal, 
                                       ifelse(VClass==closer, as.character(VClass),
                                              ifelse(VClass=='aeh' & closer=='ae', 'xlax',
                                                     'xtense'))))
# Find tokens coded differently by Mahal and FAVE
diff <- droplevels(subset(all.mahal, VClass!=closer))
# Get subset of words with variable coding
except <- droplevels(subset(all.mahal, Word %in% levels(diff$Word)))
# Remove words with less than 5 tokens
counts <- as.data.frame(table(except$Word))
except2 <- merge(except, counts, by.x="Word", by.y="Var1")
except3 <- droplevels(subset(except2, Freq >=5))
table <- with(except3, table(Word, exceptions))
sink('all.mahal.txt')
cbind(table, round(prop.table(table, 1), 2))
sink()

if (MAKE.PDF) {
  pdf(file = "allwords.pdf", width=6, height=5, onefile=TRUE)
  for (spk in levels(all.mahal$Subject)) {
    print(ggplot(data=subset(all.mahal, Subject==spk), 
                 aes(MZ2, MZ1, label=Word, color=exceptions)) +
            geom_text(size=2) +
            scale_color_manual(values=c('aeh'='red', 'ae'='blue',
                                        'xlax'='orange', 'xtense'='purple'))+
            scale_alpha_manual(values=c('aeh'=.25, 'ae'=.25,
                                        'xlax'=1, 'xtense'=1))+
            scale_x_reverse() +
            scale_y_reverse() +
            labs(title=spk) +
            theme_bw())
  }
  dev.off()
}

