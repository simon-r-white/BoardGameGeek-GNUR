## =====
## ===== Code to fetch user collection from BGG XML API2
## =====

## see
## rsvg
## svglite
## svg(), part of cario device


source("functions.r")
library("scales")


DATA <- Load.DATA()

DATA$Year.original <- DATA$Year
DATA$Year[is.na(DATA$Year)] <- 1980
DATA$Year[(DATA$Year<1980)] <- 1980

DATA$Release.Date <- as.Date(sprintf("%04i-01-01",DATA$Year))
DATA$Status.Date <- as.Date( DATA$Status.DateTime )

DATA$Date <- as.Date(ifelse(!is.na(DATA$Acquired.Date), DATA$Acquired.Date, DATA$Release.Date ), origin="1970-01-01")

                    
if( 0 ) {
    layout(matrix(1:3,ncol=1))

    H1 <- hist(DATA[which(DATA$Own==TRUE),"Date"],
               breaks="quarter",freq=TRUE,main="Games (Own,All,All)",xlab="Time")
    H2 <- hist(DATA[which(DATA$Own==TRUE & is.na(DATA$Acquired.Date)),"Date"],
               breaks="quarter",freq=TRUE,plot=FALSE)
    plot(H2,add=TRUE,col=alpha("red",0.5))
    H3 <- hist(DATA[which(DATA$Own==TRUE & is.na(DATA$Acquired.From)),"Date"],
               breaks="quarter",freq=TRUE,plot=FALSE)
    plot(H3,add=TRUE,col=alpha("blue",0.5))

    H1 <- hist(DATA[which(DATA$Own==TRUE & DATA$Subtype=="boardgame" ),"Date"],
               breaks="quarter",freq=TRUE,main="Games (Own,Base,All)",xlab="Time")
    H2 <- hist(DATA[which(DATA$Own==TRUE & DATA$Subtype=="boardgame" & is.na(DATA$Acquired.Date)),"Date"],
               breaks="quarter",freq=TRUE,plot=FALSE)
    plot(H2,add=TRUE,col=alpha("red",0.5))
    H3 <- hist(DATA[which(DATA$Own==TRUE & DATA$Subtype=="boardgame" & is.na(DATA$Acquired.From)),"Date"],
               breaks="quarter",freq=TRUE,plot=FALSE)
    plot(H3,add=TRUE,col=alpha("blue",0.5))

    H1 <- hist(DATA[which(DATA$Own==TRUE & DATA$Subtype=="boardgame" ),"Date"],
               breaks="quarter",freq=TRUE,main="Games (Own,Base,Played)",xlab="Time")
    H2 <- hist(DATA[which(DATA$Own==TRUE & DATA$Subtype=="boardgame" & (DATA$Plays>0)),"Date"],
               breaks="quarter",freq=TRUE,plot=FALSE)
    plot(H2,add=TRUE,col=alpha("red",0.5))

}



                         


subset(DATA[,HIDE],Own==FALSE)
(subset(DATA[,HIDE],Own==FALSE & Plays<=0))
(subset(DATA[,HIDE],Own==FALSE & Plays>0))
(subset(DATA[,HIDE],Acquired.From==""))
(subset(DATA[,HIDE],Acquired.From=="BGG"))

xtabs( ~ is.na(Rating) + (Plays>0), data=DATA )
xtabs( ~ Own + (Plays>0), data=DATA )




                





