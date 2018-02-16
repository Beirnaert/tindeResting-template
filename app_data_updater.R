# get the database
sqlitePath <- "swiperespons.sqlite"
mydb <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
if("swipes" %in% DBI::dbListTables(mydb) ){
    swipedata <- DBI::dbReadTable(mydb, "swipes")
    data.exists = TRUE
    DBI::dbRemoveTable(mydb, "swipes")
} else{
    data.exists = FALSE
}
DBI::dbDisconnect(mydb)

if(data.exists){
    
    swipedata <- subset(swipedata, select = - iter )
    swipedata$date = as.character(Sys.time())
    
    historyfile = paste("./data/SwipeHistory.sqlite",sep = "")
    
    Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
    DBI::dbWriteTable(Db, name="history", value=swipedata, row.names=FALSE, append=TRUE)
    DBI::dbDisconnect(Db)

}
