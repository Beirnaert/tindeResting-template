#App is a simple card with some content and a little output below that represents the last swipes result.
# devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinysense)
library(ggplot2)
library(randomForest)
library(stats)

#file_extensions = tools::file_ext(list.files("data/"))

#library(DBI)
#library(RSQLite)
#library(grid)


sqlitePath <- "swiperespons.sqlite"


saveData <- function(input, output, iter) {
    
    data2 = cbind(iter, input$useRname, output[1,,drop = FALSE])
    colnames(data2) = c("iter", "user", "index", "file", "swipe")
    
    db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)

    DBI::dbWriteTable(db, name="swipes", value=data2, row.names=FALSE, append=TRUE)
    
    DBI::dbDisconnect(db)
}

getHistory <- function(input) {
    historyfile = "./data/SwipeHistory.sqlite"
    if(file.exists(historyfile)){
            # new to me
            Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
            if(DBI::dbExistsTable(Db, name = "history")){
                history_names <- as.character(DBI::dbGetQuery(Db, paste("SELECT file FROM history WHERE user='", as.character(input$useRname),"'", sep = "") )[[1]] )
            } else{
                history_names = NULL
            }
            DBI::dbDisconnect(Db)
    } else {
        history_names = NULL
    }
    return(history_names)
}

OopsieDaisyRemoveDbEntry <- function(input, output, iter){
    Db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
    
    if("swipes" %in% DBI::dbListTables(Db) ){
        DBI::dbExecute(Db, paste("DELETE FROM swipes WHERE user='", as.character(input$useRname),"' AND iter=", as.character(iter), sep = "") )
    }
    DBI::dbDisconnect(Db)
}


ui <- fluidPage(
    headerPanel('This is the tindeResting template.'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(8,
                       textInput("useRname", "Your name", "Mr Meeseeks"),
                       offset = 2
                )
            ),
            fluidRow(
                column(8,
                       selectInput("onlyNew", "Only new images", c("yes" = "yes", "no" = "no")),
                       offset = 2
                )
            ),
            fluidRow(
                column(8, selectInput("swipeOrder", "Swipe Order", c("Top first" = "tfrst", "Random" = "rnd")),
                       offset = 2
                       )
            ),
            fluidRow(
                column(8, actionButton("undo", 
                                       "Oopsie daisy",  
                                       style="color:#fff; background-color:Crimson"),
                       align = "center", 
                       offset = 2 ))
            
        ),
        mainPanel(
            h4("Swipe Me! Or use the arrows"),
            p("Swipe the plot to the right if it is interesting. Left if not. Up is unknown"),
            hr(),
            fluidRow(
                column(2, actionButton(inputId = "buttonLeft", label = "boring", icon = icon("arrow-left") ), align = "left", offset = 3),
                column(2, actionButton(inputId = "buttonUp", label = "other", icon = icon("arrow-up") ), align = "center", offset = 0),
                column(2, actionButton(inputId = "buttonRight", label = "interesting", icon = icon("arrow-right")), align = "right" , offset = 0)
            ),
            br(),
            shinyswiprUI( "quote_swiper",
                          imageOutput("ReviewImage")
            ),
            hr(),
            h4("Swipe History"),
            tableOutput("resultsTable")
        )
    ),
    hr(),
    print("Template aggregated by Charlie Beirnaert. Feel free to change this completely" )
    
)

server <- function(input, output, session) {
    
    # update data in the beginning, also do this when session ends to speed it up
     source("app_data_updater.R")
    
    card_swipe <- callModule(shinyswipr, "quote_swiper")
    
    
    selection.vector <- reactive({
        # the getHistory function knows which selection is to be made (because we give it input): user or natura based
        files <- list.files("data/images/")
        subset.selection <- rep(TRUE, length(files))
        if(input$onlyNew == "yes"){
            subset.selection[ files %in% getHistory(input) ] <- FALSE
        }
        subset.selection = which(subset.selection)
        
        if(input$swipeOrder == "tfrst"){
            showorder <- order(subset.selection)
        } else if(input$swipeOrder == "rnd"){
            showorder <- order(runif( length(subset.selection) ))
        }
        subset.selection = subset.selection[showorder]
        subset.selection
    })
    
    everything_reviewed <- reactive({
        if (length(selection.vector()) < 1) {
            showModal(
                modalDialog(
                    title = "No more images",
                    "There are no more images for you to review. Either there are none in the 'images' subfoler or you have reviewed every single one.",
                    easyClose = TRUE
                )
            )
        }
    })
    
    output$ReviewImage        <- renderImage({
        
        input$useRname # to get the shit started
        kk <- selection.vector()[as.numeric(appVals$k)]
        Images <- list.files("data/images/")
        CurrentImage <- magick::image_read(paste("data/images/",Images[kk],sep=""))
        tmpfile <- magick::image_write(CurrentImage, tempfile(fileext='jpg'), format = 'jpg')
        list(src = tmpfile, 
             width = 400,
             contentType = "image/jpeg",
             style="display: block; margin-left: auto; margin-right: auto; margin-top: auto; margin-bottom: auto;")

    })
    ####
    output$resultsTable <- renderDataTable({appVals$swipes})
    
    
    appVals <- reactiveValues(
        k  =  1,
        swipes = data.frame(index = character(), swipe = character())
    )
    
    
    observeEvent( card_swipe(),{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(selection.vector()[as.numeric(appVals$k)]),
                       file = list.files("data/images/")[selection.vector()[as.numeric(appVals$k)]],
                       swipe  = card_swipe()
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        if(appVals$k == length(selection.vector())){
            showModal(modalDialog(
                title = "No more images",
                "This is the last image of the user's selection to be reviewed.",
                easyClose = TRUE
            ))
        }
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
    }) #close event observe.
    
    observeEvent( input$buttonLeft,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(selection.vector()[as.numeric(appVals$k)]),
                       file = list.files("data/images/")[selection.vector()[as.numeric(appVals$k)]],
                       swipe  = "Left"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        if(appVals$k == length(selection.vector())){
            showModal(modalDialog(
                title = "No more images",
                "This is the last image of the user's selection to be reviewed.",
                easyClose = TRUE
            ))
        }
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
    }) #close event observe.
    
    observeEvent( input$buttonUp,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(selection.vector()[as.numeric(appVals$k)]),
                       file = list.files("data/images/")[selection.vector()[as.numeric(appVals$k)]],
                       swipe  = "Up"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        if(appVals$k == length(selection.vector())){
            showModal(modalDialog(
                title = "No more images",
                "This is the last image of the user's selection to be reviewed.",
                easyClose = TRUE
            ))
        }
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
    }) #close event observe.
    
    observeEvent( input$buttonRight,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(selection.vector()[as.numeric(appVals$k)]),
                       file = list.files("data/images/")[selection.vector()[as.numeric(appVals$k)]],
                       swipe  = "Right"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        if(appVals$k == length(selection.vector())){
            showModal(modalDialog(
                title = "No more images",
                "This is the last image of the user's selection to be reviewed.",
                easyClose = TRUE
            ))
        }
       
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
        
        
    }) #close event observe.
    observeEvent( input$undo,{
        
        OopsieDaisyRemoveDbEntry(input, appVals$swipes, appVals$k)
        
        showModal(modalDialog(
            title = "Reaction deleted",
            "The last reaction you submitted to the database has been deleted.",
            easyClose = TRUE
        ))
        
    }) #close event observe.
    
    session$onSessionEnded(function() {
        source("app_data_updater.R")
        
    })
}

shinyApp(ui, server)
