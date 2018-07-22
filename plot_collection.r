## =====
## ===== Code to fetch user collection from BGG XML API2
## =====

## see
## rsvg
## svglite
## svg(), part of cario device


source("functions.r")


DATA <- Load.DATA()

DATA$Year.original <- DATA$Year
DATA$Year[is.na(DATA$Year)] <- 1980
DATA$Year[(DATA$Year<1980)] <- 1980

DATA$Release <- as.POSIXct(sprintf("%04i-01-01 00:00:00",DATA$Year))
summary(DATA$Release)

DATA$Date <- as.POSIXct(ifelse(!is.na(DATA$Acquired.Date),
                    as.POSIXct(DATA$Acquired.Date),
             ifelse(DATA$Release<DATA$Status.DateTime,
                    DATA$Release,
                    DATA$Status.DateTime)
             ),origin="1970-01-01")

                    

hist(DATA$Date,breaks="months",freq=TRUE)
hist(DATA$Acquired.Date,breaks="months",freq=TRUE)
hist(DATA$Release,breaks="year",freq=TRUE)
                         


subset(DATA[,HIDE],Own==FALSE)
(subset(DATA[,HIDE],Own==FALSE & Plays<=0))
(subset(DATA[,HIDE],Own==FALSE & Plays>0))
(subset(DATA[,HIDE],Acquired.From==""))
(subset(DATA[,HIDE],Acquired.From=="BGG"))

xtabs( ~ is.na(Rating) + (Plays>0), data=DATA )
xtabs( ~ Own + (Plays>0), data=DATA )




                





