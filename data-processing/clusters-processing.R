#!/usr/bin/env Rscript

# Analysis Step 1: re-code the sC clusters
# 1. Identify & subset speakers with the traditional short-a system.
# 2. Pull out their sC cluster words.
# 3. Calculate Mahalanobis distance between each cluster word & its
#    speaker's ae and aeh means.
# 4. Recode each word as 'ae' or 'aeh' by which distance is smaller.
# 5. Tabulate new code by word & identify patterns.
# 6. Update shorta.py exception lists & re-run corpus.

library(ggplot2)
# setwd('~/Desktop/Phila-short-a/')
oldaf <- read.csv("shorta-old.csv")
MAKE.PDF <- FALSE
# how we invert things
inv <- function(X) solve(X)

# make a giant pdf of every speaker's short-a system to identify tradish
if (MAKE.PDF) {
    pdf(file = "PNC-shorta-old.pdf", width=6, height=5, onefile=TRUE)
    for (spk in levels(oldaf$Subject)) {
    print(ggplot(data=subset(oldaf, Subject==spk), 
                 aes(F2, F1, color=VClass, label=Word)) +
                 geom_text(size=2) +
                 scale_x_reverse() +
                 scale_y_reverse() +
                 labs(title=spk) +
                 theme_bw())
    }
    dev.off()
}

# remove the speakers who clearly don't have the traditional system
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

# pull out s-cluster words
clusters <- c("ALABASTER", "ALASKA", "ALASKA'S", "ALASKAN", "ALASKANS",
              "ANGIOPLASTY", "ASCII", "ASCOT", "ASKED", "ASKER", "ASKER'S", 
              "ASKERS", "ASKERS'", "ASKERS'S", "ASKIN'", "ASKIN'S", 
              "ASKING", "ASPARTAME", "ASPECT", "ASPECTS", "ASPEN", 
              "ASPEN'S", "ASPENS", "ASPIRANT", "ASPIRANTS", "ASPIRATE", 
              "ASPIRATED", "ASPIRATES", "ASPIRATION", "ASPIRATIONAL", 
              "ASPIRATIONS", "ASPIRIN", "ASPIRIN'S", "ASPIRINS", "ASPLUND", 
              "ASTER", "ASTERISK", "ASTERISKS", "ASTEROID", "ASTEROID'S", 
              "ASTEROIDS", "ASTERS", "ASTORIA", "ASTRA", "ASTRA'S", 
              "ASTRAL", "ASTRO", "ASTRO'S", "ASTRODOME", "ASTROLOGICAL", 
              "ASTRONAUT", "ASTRONAUT'S", "ASTRONAUTIC", "ASTRONAUTICAL", 
              "ASTRONAUTICS", "ASTRONAUTS", "ASTRONAUTS'", "ASTRONOMICAL", 
              "ASTRONOMICALLY", "ASTROPHYSICIST", "ASTROPHYSICS", "ASTROS", 
              "ASTROTECH", "ASTROTURF", "BASKERVILLE", "BASKET", 
              "BASKETBALL", "BASKETBALL'S", "BASKETBALLS", "BASKETMAKER", 
              "BASKETMAKING", "BASKETRY", "BASKETS", "BASKING", "BASTARD", 
              "BASTARDS", "BASTILLE", "BLASTED", "BLASTER", "BLASTIN'", 
              "BLASTING", "BLASTOFF", "BOMBASTIC", "BREADBASKET", 
              "BROADCASTER", "BROADCASTER'S", "BROADCASTERS", 
              "BROADCASTERS'", "BROADCASTING", "BROADCASTING'S", 
              "CANASTA", "CARDIOVASCULAR", "CASCADE", "CASCADE'S", 
              "CASCADED", "CASCADES", "CASCADES'", "CASCADING", "CASKET", 
              "CASKETS", "CASPAR", "CASPER", "CASPERS", "CASPIAN", 
              "CASTAWAY", "CASTAWAYS", "CASTER", "CASTERS", "CASTIGATE", 
              "CASTIGATED", "CASTIGATING", "CASTILLE", "CASTIN'", 
              "CASTING", "CASTINGS", "CASTOFF", "CASTOFFS", "CASTOR", 
              "CASTRATE", "CASTRATED", "CASTRATES", "CASTRATING", 
              "CASTRATION", "CASTRATIONS", "CASTRO", "CASTRO'S", "CASTS", 
              "CATASTROPHE", "CATASTROPHES", "CHASTISE", "CHASTISED", 
              "CHASTISES", "CHASTISING", "CHASTITY", "CHLOROPLASTS", 
              "COMCAST'S", "CONCERTMASTER", "CONTRASTED", "CONTRASTING", 
              "DIASPORA", "DIASTOLE", "DIASTROPHISM", "DISASTER", 
              "DISASTERS", "DISASTROUS", "DISASTROUSLY", "DRASTIC", 
              "DRASTICALLY", "DYNASTIC", "ECCLESIASTIC", "ECCLESIASTICAL", 
              "ELASTIC", "ELASTICITY", "ELASTOMER", "ELASTOMERS", 
              "EMASCULATE", "EMASCULATED", "ENTHUSIASTIC", 
              "ENTHUSIASTICALLY", "ENTHUSIASTS", "EVERLASTING", 
              "EVERLASTINGS", "EXASPERATE", "EXASPERATED", "EXASPERATING", 
              "EXASPERATION", "FANTASTIC", "FANTASTICALLY", "FASTED", 
              "FASTER", "FASTEST", "FASTIDIOUS", "FASTING", "FIASCO", 
              "FIASCO'S", "FIASCOS", "FIGHTMASTER", "FLABBERGASTED",
              "FLAMEMASTER", "FORECASTED", "FORECASTER", "FORECASTERS", 
              "FORECASTING", "GASKELL", "GASKET", "GASKETS", "GASPED", 
              "GASPER", "GASPIN'", "GASPING", "GASTRIC", "GASTRITIS", 
              "GASTROINTESTINAL", "GASTRONOMY", "GASTROSCOPE", 
              "GASTROVASCULAR", "GASTRULATE", "GASTRULATION", "GHASTLINESS", 
              "GRANDMASTER", "GRASPING", "GYMNASTIC", "GYMNASTICS",
              "HEADMASTER", "ICONOCLASTIC", "INELASTIC", "INTERSCHOLASTIC", 
              "JASPER", "JASPER'S", "JASPERS", "JEWELMASTER", "JEWELMASTERS", 
              "KASPAR", "KASPER", "LAMBASTED", "LANCASTER", "LANCASTRIAN", 
              "LASTED", "LASTER", "LASTEST", "LASTING", "LASTINGER",
              "MADAGASCAR", "MASCARA", "MASCOT", "MASCOTS", "MASCULINE", 
              "MASCULINITY", "MASKER", "MASKIN'", "MASKING", "MASQUERADE", 
              "MASQUERADING", "MASTECTOMIES", "MASTECTOMY", "MASTED", 
              "MASTER", "MASTER'S", "MASTERCARD", "MASTERCARD'S", 
              "MASTERCARDS", "MASTERED", "MASTERFUL", "MASTERFULLY", 
              "MASTERING", "MASTERLY", "MASTERMAN", "MASTERMIND", 
              "MASTERMINDED", "MASTERMINDING", "MASTERMINDS", "MASTERPIECE", 
              "MASTERPIECES", "MASTERS", "MASTERS'", "MASTERWORK", 
              "MASTERWORKS", "MASTERY", "MASTHEAD", "MASTIFF", "MASTODON", 
              "MASTURBATE", "MASTURBATED", "MASTURBATES", "MASTURBATING", 
              "MASTURBATION", "METASTASIZE", "METASTASIZED", "MONASTIC", 
              "MONASTICISM", "MULTITASKING", "NASCAR", "NASTIER", "NASTIEST", 
              "NASTINESS", "NASTY", "NEBRASKA", "NEBRASKA'S", "NEBRASKAN", 
              "NEBRASKANS", "NEWSCASTER", "NEWSCASTERS", "NEWSCASTS", 
              "ONOMASTIC", "ONOMASTICS", "OUTLASTED", "PASTEL", "PASTELS", 
              "PASTICHE", "PASTIME", "PASTIMES", "PASTOR", "PASTOR'S", 
              "PASTORAL", "PASTORALISM", "PASTORS", "PILASTER", 
              "PILASTERS", "PLASTER", "PLASTERBOARD", "PLASTERED", 
              "PLASTERER", "PLASTERING", "PLASTERS", "PLASTERWORK", 
              "PLASTIC", "PLASTICINE", "PLASTICIZER", "PLASTICS", 
              "POSTMASTER", "POSTMASTERS", "PROCRASTINATE", 
              "PROCRASTINATING", "PROCRASTINATION", "PUZZLEMASTER", 
              "QUARTERMASTER", "RASCAL", "RASCALS", "RASPY", "RASTER", 
              "RECASTING", "RINGMASTER", "ROADMASTER", "SANDBLASTED", 
              "SARCASTIC", "SARCASTICALLY", "SCHOLASTIC", "SCHOOLMASTER", 
              "SCOUTMASTER", "SERVICEMASTER", "SPORTSCASTER", 
              "SPORTSCASTERS", "STAINMASTER", "STRATOCASTER", "TABASCO", 
              "TASKER", "TASKING", "TASKMASTER", "THERMOPLASTIC", 
              "THERMOPLASTICS","TICKETMASTER", "TICKETMASTER'S", 
              "TOASTMASTER", "TYPECASTING", "UNENTHUSIASTIC", "VASCULAR", 
              "WASTEBASKET", "WASTEBASKETS", "WAYCASTER", "WEBMASTER", 
              "PASTURE", "PASTURES", "PASTEURIZE", "PASTEURIZED", 
              "PASTEURIZATION", "BASTION")

# make sure we're only getting the relevant tokens in these words
clusters2 <- droplevels(subset(philasys, Word %in% clusters & 
                               Manner=="fricative"))

# make a giant pdf of traditional speakers' s-clusters
if (MAKE.PDF) {
    pdf(file = "PNC-sclusters.pdf", width=6, height=5, onefile=TRUE)
    for (spk in levels(clusters2$Subject)) {
    print(ggplot(data=subset(philasys, Subject==spk), 
                 aes(F2, F1, label=Word, color=VClass)) +
                 geom_text(size=2, alpha=.5) +
                 geom_text(data=subset(clusters2, Subject==spk), 
                           color='black', size=2) +
                 scale_x_reverse() +
                 scale_y_reverse() +
                 labs(title=spk) +
                 theme_bw())
    }
    dev.off()
}

# Malanobis distances
# all tense/lax tokens are in philasys
# cluster tokens are in clusters2

# create data frame with a dummy row because sigh, R
out <- data.frame(mahal.ae=1.0, mahal.aeh=1.0)

# calculate mahalanobis distances for cluster words on a by-speaker basis
# un-normalized gives same result as normalized
for (speaker in levels(clusters2$Subject)) {
  data <- subset(philasys, Subject==speaker)
  words <- subset(clusters2, Subject==speaker, select=c(MZ1,MZ2))
  # get means & covariances for the speaker's ae and aeh
  ae.d <- data[data$VClass=="ae", ]
  ae <- cbind(ae.d$MZ1, ae.d$MZ2)
  ae.mu <- colMeans(ae)
  ae.icov <- inv(cov(ae))
  aeh.d <- data[data$VClass=="aeh", ]
  aeh <- cbind(aeh.d$MZ1, aeh.d$MZ2)
  aeh.mu <- colMeans(aeh)
  aeh.icov <- inv(cov(aeh))
  # for each of the speaker's s-cluster words, calculate mahalanobis 
  # distance, append to output
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

# get rid of that dummy row
out2 <- out[-1,]
# new dataframe with mahal distances
sC.mahal <- cbind(clusters2, out2, row.names=NULL)
# calculate which vowel mean each word is closer to
sC.mahal$closer <- as.factor(with(sC.mahal, ifelse(mahal.ae < mahal.aeh, "ae", "aeh")))
# make a table of words by new code
with(sC.mahal, table(Word, closer))


# calculate mahalanobis distances for cluster words on group means
# get means & covariances for the group's ae and aeh means
#out <- data.frame(mahal.ae=1.0, mahal.aeh=1.0)
#ae.d <- philasys[philasys$VClass=="ae", ]
#ae <- cbind(ae.d$MZ1, ae.d$MZ2, row.names=NULL)
#ae.mu <- colMeans(ae)
#ae.icov <- inv(cov(ae))
#aeh.d <- philasys[philasys$VClass=="aeh", ]
#aeh <- cbind(aeh.d$MZ1, aeh.d$MZ2)
#aeh.mu <- colMeans(aeh)
#ae.icov <- inv(cov(aeh))
#words <- cbind(clusters2$MZ1, clusters2$MZ2)
# for each s-cluster word, calculate mahalanobis distance, append to output
#for (i in 1:nrow(words)) {
#    word <- words[i, ]
#    mahal.ae <- mahalanobis(x=word, center=ae.mu, cov=ae.icov, 
#                            inverted=TRUE)
#    mahal.aeh <- mahalanobis(x=word, center=aeh.mu, cov=aeh.icov, 
#                             inverted=TRUE)
#    mahals <- cbind(mahal.ae, mahal.aeh)
#    out <- rbind(out, mahals)
#}

# get rid of that dummy row
#out2 <- out[-1,]
# add distances to dataframe
#sC.mahal2 <- cbind(clusters2, out2, row.names=NULL)
# calculate which vowel mean each word is closer to
#sC.mahal2$closer <- as.factor(with(sC.mahal2, ifelse(mahal.ae < mahal.aeh, "ae", "aeh")))
# make a table of words by new code
#with(sC.mahal2, table(Word, closer))

# giant pdf of new codes and mahalanobis distances
if (MAKE.PDF) {
    pdf(file = "sc-mahal.pdf", width=6, height=5, onefile=TRUE)
    for (spk in levels(sC.mahal$Subject)) {
        print(ggplot(data=subset(philasys, Subject==spk), 
                     aes(MZ2, MZ1, label=Word)) +
                     geom_text(size=2, alpha=.35) +
                     geom_text(data=subset(sC.mahal, Subject==spk), 
                     aes(color=closer, 
                         label=paste0(Word, "\n", round(mahal.aeh), ", ", 
                         round(mahal.ae))), vjust=.8, size=2) +
                     scale_color_manual(values=c('aeh'='red', 'ae'='blue'))+
                     scale_x_reverse() +
                     scale_y_reverse() +
                     labs(title=spk) +
                     theme_bw())
    }
    dev.off()
}
