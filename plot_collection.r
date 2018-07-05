## =====
## ===== Code to fetch user collection from BGG XML API2
## =====

source("functions.r")


DATA <- Load.DATA()



str(DATA)
levels(DATA$Acquired.From)
table(addNA(DATA$Acquired.From))

table(addNA(DATA$Type))
table(addNA(DATA$Subtype))
table(addNA(DATA$Own))
table(addNA(DATA$Acquired.From))





head(DATA)
HIDE <- -1*c(2,5,7)
head(DATA[,HIDE])

subset(DATA[,HIDE],Own==FALSE)
(subset(DATA[,HIDE],Own==FALSE & Plays<=0))
(subset(DATA[,HIDE],Own==FALSE & Plays>0))
(subset(DATA[,HIDE],Acquired.From==""))
(subset(DATA[,HIDE],Acquired.From=="BGG"))

xtabs( ~ is.na(Rating) + (Plays>0), data=DATA )
xtabs( ~ Own + (Plays>0), data=DATA )




                





