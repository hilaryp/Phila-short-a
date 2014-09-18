# First pass analysis of old coding to reassign sC clusters

setwd('~/Desktop/Phila-short-a/')
oldaf <- read.csv("shorta-old.csv")

# make a giant pdf of every PNC speaker's short-a system
library(ggplot2)
pdf(file = "PNC-shorta-old.pdf", width=6, height=5, onefile=TRUE)
for (spk in levels(oldaf$Subject)) {
  print(ggplot(data=subset(oldaf, Subject==spk), 
               aes(F2, F1, color=VClass, label=Word))+
          geom_text(size=2)+
          scale_x_reverse()+
          scale_y_reverse()+
          labs(title=spk)+
          theme_bw())
}
dev.off()

# remove the speakers who clearly don't have the traditional system
philasys <- droplevels(subset(oldaf, !Subject %in% c('IHP1-1', 'IHP1-2',
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
                       'PHI-R-2', 'PHI-R-4', 'PHI-R-5', 'PHI-R-6', 'PHI-R-7')))

# pull out s-cluster words
clusters <- droplevels(subset(philasys, Word %in% c("ALABASTER", "ALASKA", 
                       "ALASKA'S", "ALASKAN", "ALASKANS", "ANGIOPLASTY", 
                       "ASCII", "ASCOT", "ASKED", "ASKER", "ASKER'S", "ASKERS", 
                       "ASKERS'", "ASKERS'S", "ASKIN'", "ASKIN'S", "ASKING", 
                       "ASPARTAME", "ASPECT", "ASPECTS", "ASPEN", 
                       "ASPEN'S", "ASPENS", "ASPIRANT", "ASPIRANTS", 
                       "ASPIRATE", "ASPIRATED", "ASPIRATES", "ASPIRATION", 
                       "ASPIRATIONAL", "ASPIRATIONS", "ASPIRIN", "ASPIRIN'S", 
                       "ASPIRINS", "ASPLUND", "ASTER", "ASTERISK", "ASTERISKS", 
                       "ASTEROID", "ASTEROID'S", "ASTEROIDS", "ASTERS", 
                       "ASTORIA", "ASTRA", "ASTRA'S", "ASTRAL", "ASTRO", 
                       "ASTRO'S", "ASTRODOME", "ASTROLOGICAL", "ASTRONAUT", 
                       "ASTRONAUT'S", "ASTRONAUTIC", "ASTRONAUTICAL", 
                       "ASTRONAUTICS", "ASTRONAUTS", "ASTRONAUTS'", 
                       "ASTRONOMICAL", "ASTRONOMICALLY", "ASTROPHYSICIST", 
                       "ASTROPHYSICS", "ASTROS", "ASTROTECH", "ASTROTURF", 
                       "BASKERVILLE", "BASKET", "BASKETBALL", 
                       "BASKETBALL'S", "BASKETBALLS", "BASKETMAKER", 
                       "BASKETMAKING", "BASKETRY", "BASKETS", "BASKING", 
                       "BASTARD", "BASTARDS", "BASTILLE", 
                       "BLASTED", "BLASTER", "BLASTIN'", "BLASTING", 
                       "BLASTOFF", "BOMBASTIC", "BREADBASKET", 
                       "BROADCASTER", "BROADCASTER'S", "BROADCASTERS", 
                       "BROADCASTERS'", "BROADCASTING", "BROADCASTING'S", 
                       "CANASTA", "CARDIOVASCULAR", "CASCADE", 
                       "CASCADE'S", "CASCADED", "CASCADES", "CASCADES'", 
                       "CASCADING", "CASKET", "CASKETS", "CASPAR", 
                       "CASPER", "CASPERS", "CASPIAN", "CASTAWAY", "CASTAWAYS", 
                       "CASTER", "CASTERS", "CASTIGATE", "CASTIGATED", 
                       "CASTIGATING", "CASTILLE", "CASTIN'", "CASTING", 
                       "CASTINGS", "CASTOFF", "CASTOFFS", "CASTOR", "CASTRATE", 
                       "CASTRATED", "CASTRATES", "CASTRATING", "CASTRATION", 
                       "CASTRATIONS", "CASTRO", "CASTRO'S", "CASTS", 
                       "CATASTROPHE", "CATASTROPHES", "CHASTISE", "CHASTISED", 
                       "CHASTISES", "CHASTISING", "CHASTITY", "CHLOROPLASTS", 
                       "COMCAST'S", "CONCERTMASTER", "CONTRASTED", 
                       "CONTRASTING", "DIASPORA", "DIASTOLE", 
                       "DIASTROPHISM", "DISASTER", "DISASTERS", "DISASTROUS", 
                       "DISASTROUSLY", "DRASTIC", "DRASTICALLY", "DYNASTIC", 
                       "ECCLESIASTIC", "ECCLESIASTICAL", "ELASTIC", "ELASTICITY", 
                       "ELASTOMER", "ELASTOMERS", "EMASCULATE", "EMASCULATED", 
                       "ENTHUSIASTIC", "ENTHUSIASTICALLY", "ENTHUSIASTS", 
                       "EVERLASTING", "EVERLASTINGS", "EXASPERATE", 
                       "EXASPERATED", "EXASPERATING", "EXASPERATION", 
                       "FANTASTIC", "FANTASTICALLY", "FASTED", 
                       "FASTER", "FASTEST", "FASTIDIOUS", "FASTING", 
                       "FIASCO", "FIASCO'S", "FIASCOS", 
                       "FIGHTMASTER", "FLABBERGASTED", "FLAMEMASTER", 
                       "FORECASTED", "FORECASTER", "FORECASTERS", "FORECASTING", 
                       "GASKELL","GASKET", "GASKETS", "GASPED", "GASPER", 
                       "GASPIN'", "GASPING", "GASTRIC", "GASTRITIS", 
                       "GASTROINTESTINAL", "GASTRONOMY", "GASTROSCOPE", 
                       "GASTROVASCULAR", "GASTRULATE", "GASTRULATION", 
                       "GHASTLINESS", "GRANDMASTER", "GRASPING", "GYMNASTIC", 
                       "GYMNASTICS", "HEADMASTER", "ICONOCLASTIC", 
                       "INELASTIC", "INTERSCHOLASTIC", "JASPER", "JASPER'S", 
                       "JASPERS", "JEWELMASTER", "JEWELMASTERS", "KASPAR", 
                       "KASPER", "LAMBASTED", "LANCASTER", 
                       "LANCASTRIAN", "LASTED", "LASTER", "LASTEST", 
                       "LASTING", "LASTINGER",
                       "MADAGASCAR", "MASCARA", "MASCOT", 
                       "MASCOTS", "MASCULINE", "MASCULINITY",
                       "MASKER", "MASKIN'", "MASKING", "MASQUERADE", 
                       "MASQUERADING", "MASTECTOMIES", "MASTECTOMY", "MASTED", 
                       "MASTER", "MASTER'S", "MASTERCARD", "MASTERCARD'S", 
                       "MASTERCARDS", "MASTERED", "MASTERFUL", "MASTERFULLY", 
                       "MASTERING", "MASTERLY", "MASTERMAN", "MASTERMIND", 
                       "MASTERMINDED", "MASTERMINDING", "MASTERMINDS", 
                       "MASTERPIECE", "MASTERPIECES", "MASTERS", "MASTERS'", 
                       "MASTERWORK", "MASTERWORKS", "MASTERY", "MASTHEAD", 
                       "MASTIFF", "MASTODON", "MASTURBATE", 
                       "MASTURBATED", "MASTURBATES", "MASTURBATING", 
                       "MASTURBATION", "METASTASIZE", "METASTASIZED", "MONASTIC", 
                       "MONASTICISM", "MULTITASKING", "NASCAR", "NASTIER", 
                       "NASTIEST", "NASTINESS", "NASTY", "NEBRASKA", "NEBRASKA'S", 
                       "NEBRASKAN", "NEBRASKANS", "NEWSCASTER", "NEWSCASTERS", 
                       "NEWSCASTS", "ONOMASTIC", "ONOMASTICS", 
                       "OUTLASTED", "PASTEL", "PASTELS", 
                       "PASTICHE", "PASTIME", "PASTIMES", "PASTOR", "PASTOR'S", 
                       "PASTORAL", "PASTORALISM", "PASTORS", "PILASTER", 
                       "PILASTERS", "PLASTER", "PLASTERBOARD", "PLASTERED", 
                       "PLASTERER", "PLASTERING", "PLASTERS", "PLASTERWORK", 
                       "PLASTIC", "PLASTICINE", "PLASTICIZER", "PLASTICS", 
                       "POSTMASTER", "POSTMASTERS", "PROCRASTINATE", 
                       "PROCRASTINATING", "PROCRASTINATION", "PUZZLEMASTER", 
                       "QUARTERMASTER", "RASCAL", "RASCALS",  
                       "RASPY", "RASTER", "RECASTING", "RINGMASTER", "ROADMASTER", 
                       "SANDBLASTED", "SARCASTIC", "SARCASTICALLY", "SCHOLASTIC", 
                       "SCHOOLMASTER", "SCOUTMASTER", "SERVICEMASTER", 
                       "SPORTSCASTER", "SPORTSCASTERS", "STAINMASTER", 
                       "STRATOCASTER", "TABASCO", 
                       "TASKER", "TASKING", "TASKMASTER", 
                       "THERMOPLASTIC", "THERMOPLASTICS", 
                       "TICKETMASTER", "TICKETMASTER'S", "TOASTMASTER", 
                       "TYPECASTING", "UNENTHUSIASTIC", "VASCULAR", 
                       "WASTEBASKET", "WASTEBASKETS", 
                       "WAYCASTER", "WEBMASTER", "PASTURE", "PASTURES", "PASTEURIZE",
                       "PASTEURIZED", "PASTEURIZATION", "BASTION"
)))

# make sure we're only getting the relevant tokens in these words
clusters2 <- droplevels(subset(clusters, Manner=="fricative"))

# make a giant pdf of traditional speakers' s-clusters
pdf(file = "PNC-sclusters.pdf", width=6, height=5, onefile=TRUE)
for (spk in levels(clusters2$Subject)) {
  print(ggplot(data=subset(philasys, Subject==spk), 
               aes(F2, F1, label=Word, color=VClass))+
          geom_text(size=2, alpha=.5)+
          geom_text(data=subset(clusters2, Subject==spk), color='black', size=2)+
          scale_x_reverse()+
          scale_y_reverse()+
          labs(title=spk)+
          theme_bw())
}
dev.off()

##### malanobis distances
# all tense/lax tokens are in philasys
# cluster tokens are in clusters2

# create data frame with a dummy row because sigh, R
out <- data.frame(mahal_ae=1.0, mahal_aeh=1.0)

# calculate mahalanobis distances for cluster words
for (speaker in levels(clusters2$Subject)){
  data <- subset(philasys, Subject==speaker)
  words <- subset(clusters2, Subject==speaker, select=c(F1,F2))
  ae_means <- c(mean(data[data$VClass=='ae',]$F1), mean(data[data$VClass=='ae',]$F2))
  aeh_means <- c(mean(data[data$VClass=='aeh',]$F1), mean(data[data$VClass=='aeh',]$F2))
  icov <- solve(cov(cbind(data$F1, data$F2)))
  for (i in 1:nrow(words)) {
    word <- words[i, ]
    mahal_ae <- mahalanobis(x=word, center = ae_means, cov=icov, inverted=TRUE)
    mahal_aeh <- mahalanobis(x=word, center = aeh_means, cov=icov, inverted=TRUE)
    mahals <- cbind(mahal_ae, mahal_aeh)
    out <- rbind(out, mahals)
  }
}

# get rid of that dummy row
out2 <- out[-1,]
# add distances to dataframe
sC.mahal <- cbind(clusters2, out2)
# calculate which vowel mean each word is closer to
sC.mahal$closer <- with(sC.mahal, ifelse(mahal_ae < mahal_aeh, "ae", "aeh"))
# make a table of words by new code
with(sC.mahal, table(Word, closer))

# giant pdf of new codes and mahalanobis distances
pdf(file = "sc-mahal.pdf", width=6, height=5, onefile=TRUE)
for (spk in levels(sC.mahal$Subject)) {
  print(ggplot(data=subset(philasys, Subject==spk), aes(F2, F1, label=Word))+
          geom_text(size=2, alpha=.35)+
          geom_text(data=subset(sC.mahal, Subject==spk), 
                    aes(color=closer, 
                        label=paste0(Word, "\n", round(mahal_aeh), ", ", 
                                     round(mahal_ae))), 
                    vjust=.8, size=2)+
          scale_x_reverse()+
          scale_y_reverse()+
          labs(title=spk)+
          theme_bw())
}
dev.off()