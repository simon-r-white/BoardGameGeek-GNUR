Calls.to.bgg <- function( Details, Queries, delay=120 ) {
    Details=DETAILS
    Queries=QUERIES
    
    cat(sprintf("Making login attempt for %s\n",Details$username))
    Login <- POST(url="https://boardgamegeek.com/login",
                  body=list("username"=Details$username,"password"=Details$password),
                  encode="form")

    Login.Cookies <- cookies(Login)$value
    names(Login.Cookies) <- cookies(Login)$name
    cat("Storing session cookies (needed to retrieve private information fields)\n")
    
    CALLS <- list()
    
    WAITING <- TRUE

    while ( WAITING ) {
        cat("Waiting while BGG API generates XML files...\n")

        for( qNAME in names(Queries) ) {
        
            CALLS[[qNAME]] <- GET(url="https://www.boardgamegeek.com/xmlapi2/collection",
                                  query=Queries[[qNAME]],
                                  set_cookies(.cookies=Login.Cookies)
                                  )
        }

        Sys.sleep(delay)
        
        if( all(sapply( CALLS, FUN=function(X){X$status} ) == 200) ) {
            WAITING <- FALSE
            cat("XML received\n")
        }
        cat( sapply( CALLS, FUN=function(X){X$status} ),"\n" )
    }
    return( CALLS )
}


Save.XML <- function( Contents ) {

    DIR <- file.path( "data", attr(Contents,"TAG") )
    if( !dir.exists(DIR) ) {
        dir.create(DIR,recursive=TRUE)
        cat("Created new data folder:",DIR,"\n")
    } else {
        cat("Data folder already exists. WARNING: old files have been over-written (oops?)\n")
    }
    
    for( cNAME in names(Contents) ) {
        write_xml( x=Contents[[cNAME]], file=file.path(DIR,sprintf("%s.xml",cNAME)) )
    }

}



Extract.Collection <- function( Contents ) {
    
    ALL.DATA <- list()

    for( cNAME in names(Contents) ) {

        Collection <- Contents[[cNAME]]

        DATA <- data.frame(Game=character(),
                           Game.ID=character(),
                           Year=numeric(),
                           Plays=numeric(),
                           Type=character(),
                           Subtype=character(),
                           Collection.ID=character(),
                           Own=logical(),
                           Want=logical(),
                           WantToPlay=logical(),
                           WantToBuy=logical(),
                           Wishlist=logical(),
                           Status.DateTime=character(),
                           Acquired.Date=character(),
                           Price=numeric(),
                           Currency=character(),
                           Acquired.From=character(),
                           Rating=numeric(),
                           stringsAsFactors=FALSE
                           )

        Total.Items <- as.numeric( xml_attr( xml_find_all( Collection, "/items" ), attr="totalitems" ) )

        if( Total.Items>0 ) {
            for ( IDX in 1:Total.Items ) {
                if(0){
                    IDX <- 1
                }
                ## xml_children( Collection )
                ITEM <- xml_children( Collection )[[IDX]]
                gID.LIST <- as.list(xml_attrs( ITEM ))
                gNAME <- xml_text( xml_find_all( ITEM ,".//name") )
                gYEAR <- xml_integer( xml_find_all( ITEM ,".//yearpublished") )
                gPLAYS <- xml_integer( xml_find_all( ITEM ,".//numplays") )
                gSTATUS.LIST <- as.list(xml_attrs( xml_find_all( ITEM ,".//status") ))
                gPRIVATE.LIST <- as.list(xml_attrs( xml_find_all( ITEM ,".//privateinfo") ))
                gSTATS.LIST <- as.list(xml_attrs( xml_find_all( ITEM ,".//stats") ))
                gRATING.LIST <- as.list(xml_attrs( xml_find_all( ITEM ,".//stats/rating") ))

                cat( sprintf("%15s [%04i] %s\n",cNAME,IDX,gNAME) )
                
                DATA[IDX,"Game"] <- gNAME
                DATA[IDX,"Year"] <- if(length(gYEAR)==0){NA}else{gYEAR}
                DATA[IDX,"Plays"] <- gPLAYS
                DATA[IDX,"Game.ID"] <- gID.LIST[["objectid"]]
                DATA[IDX,"Type"] <- gID.LIST[["objecttype"]]
                DATA[IDX,"Subtype"] <- gID.LIST[["subtype"]]
                DATA[IDX,"Collection.ID"] <- gID.LIST[["collid"]]

                DATA[IDX,"Own"] <- if(gSTATUS.LIST[[1]]["own"]=="1"){TRUE}else{FALSE}
                DATA[IDX,"Want"] <- if(gSTATUS.LIST[[1]]["want"]=="1"){TRUE}else{FALSE}
                DATA[IDX,"WantToPlay"] <- if(gSTATUS.LIST[[1]]["wanttoplay"]=="1"){TRUE}else{FALSE}
                DATA[IDX,"WantToBuy"] <- if(gSTATUS.LIST[[1]]["wanttobuy"]=="1"){TRUE}else{FALSE}
                DATA[IDX,"Wishlist"] <- if(gSTATUS.LIST[[1]]["wishlist"]=="1"){TRUE}else{FALSE}
                DATA[IDX,"Status.DateTime"] <- gSTATUS.LIST[[1]]["lastmodified"]

                DATA[IDX,"Rating"] <- if(length(gRATING.LIST)==1){as.numeric(gRATING.LIST[[1]]["value"])}else{NA}
                DATA[IDX,"Acquired.Date"] <- if(length(gPRIVATE.LIST)==1){gPRIVATE.LIST[[1]]["acquisitiondate"]}else{NA}
                DATA[IDX,"Acquired.From"] <- if(length(gPRIVATE.LIST)==1){gPRIVATE.LIST[[1]]["acquiredfrom"]}else{NA}
                DATA[IDX,"Price"] <- if(length(gPRIVATE.LIST)==1){as.numeric(gPRIVATE.LIST[[1]]["pricepaid"])}else{NA}
                DATA[IDX,"Currency"] <- if(length(gPRIVATE.LIST)==1){gPRIVATE.LIST[[1]]["pp_currency"]}else{NA}

            }
            
            ALL.DATA[[cNAME]] <- DATA
        }
    }


    DATA <- Reduce( rbind, ALL.DATA )

    DATA$Type <- factor(DATA$Type)
    DATA$Subtype <- factor(DATA$Subtype)
    DATA$Acquired.Date <- as.Date(DATA$Acquired.Date)
    DATA$Acquired.From <- factor(DATA$Acquired.From)
    DATA$Currency <- factor(DATA$Currency)

    DATA$Status.DateTime <- as.POSIXct(DATA$Status.DateTime)
    
    attr(DATA,"TAG") <- attr(Contents,"TAG")
    return(DATA)
}


Save.DATA <- function( Data ) {

    DIR <- file.path( "data", attr(Data,"TAG") )
    if( !dir.exists(DIR) ) {
        dir.create(DIR,recursive=TRUE)
        cat("Created new data folder:",DIR,"\n")
    } else {
        cat("Data folder already exists. WARNING: old files have been over-written (oops?)\n")
    }

    saveRDS( Data, file=file.path( DIR, "data.rds" ) )
    write.csv( Data, file=file.path( DIR, "data.csv" ) )
            
}


Load.DATA <- function( Tag ) {

    if( !missing(Tag) ) {
        DATA <- ( readRDS( file=file.path( "data", Tag, "data.rds" ) ) )
    } else {

        OPTIONS <- dir("data",full.name=TRUE)
        INFO <- file.info(OPTIONS)

        DATA <- ( readRDS( file.path( OPTIONS[which.max(INFO$mtime)], "data.rds" ) ) )
    }
    cat(sprintf("Loading data[%s].\n",attr(DATA,"TAG")))
    return(DATA)
}
