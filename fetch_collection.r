## =====
## ===== Code to fetch user collection from BGG XML API2
## =====

## See following threads/links
##
## https://boardgamegeek.com/thread/1531606/log-play-api
## https://boardgamegeek.com/thread/1438065/trouble-retrieving-login-cookies


library("httr")
library("xml2")

if( !file.exists("login_details.r") ) {stop("Must create login details file. WARNING: do not commit this file (will reveal password)")}
source("login_details.r")
## ^^^ must create this file, it contains a single line as below
## DETAILS <- list("username"="BGG Account Name","password"="Clear text password (be careful)")
##

source("functions.r")

if( !dir.exists("data") ) {
    dir.create("data")
}


if( 1 ) {
    ##
    ## This block makes a fresh call to BGG API
    ##

    QUERIES <- list("boardgames-own"=list("username"=DETAILS$username,
                                          "subtype"="boardgame",
                                          "excludesubtype"="boardgameexpansion",
                                          "showprivate"="1",
                                          "own"="1",
                                          "stats"="1"),
                    "expansions-own"=list("username"=DETAILS$username,
                                          "subtype"="boardgameexpansion",
                                          "showprivate"="1",
                                          "own"="1",
                                          "stats"="1"),
                    "boardgames-other"=list("username"=DETAILS$username,
                                            "subtype"="boardgame",
                                            "excludesubtype"="boardgameexpansion",
                                            "showprivate"="1",
                                            "own"="0",
                                            "stats"="1"),
                    "expansions-other"=list("username"=DETAILS$username,
                                            "subtype"="boardgameexpansion",
                                            "showprivate"="1",
                                            "own"="0",
                                            "stats"="1")
                    )



    CALLS <- Calls.to.bgg( Details=DETAILS, Queries=QUERIES )

    CALLS.XML <- lapply( CALLS, function(X){ read_xml( content(X,as="text") ) } )
    attr(CALLS.XML,"TAG") <- format(Sys.time(),"%Y%m%d-%H%M")

    Save.XML( CALLS.XML )

} else {
    ##
    ## This code block can be used to re-load a previously saved set of XML files
    ##

    ## NOT CURRENTLY WRITTEN
    stop("Code to load saved XML files not written")

}


DATA <- Extract.Collection( CALLS.XML )


Save.DATA( DATA )
