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
    colnames(data2) = c("iter", "user", "swipe")
    
    db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)

    DBI::dbWriteTable(db, name="swipes", value=data2, row.names=FALSE, append=TRUE)
    
    DBI::dbDisconnect(db)
}

getHistory <- function(input) {
    historyfile = "./data/SwipeHistory.sqlite"
    if(file.exists(historyfile)){
            # new to me
            historyfile = "./data/SwipeHistory.sqlite"
            Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
            if(DBI::dbExistsTable(Db, name = "history")){
                history_indexes <- as.integer(DBI::dbGetQuery(Db, paste("SELECT ind FROM history WHERE user='", as.character(input$useRname),"'", sep = "") )[[1]] )
            } else{
                history_indexes = NULL
            }
            DBI::dbDisconnect(Db)
    } else {
        history_indexes = NULL
    }
    return(history_indexes)
}

OopsieDaisyRemoveDbEntry <- function(input, output, iter){
    Db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
    
    if("swipes" %in% DBI::dbListTables(Db) ){
        DBI::dbExecute(Db, paste("DELETE FROM swipes WHERE user='", as.character(input$useRname),"' AND iter=", as.character(iter), sep = "") )
    }
    DBI::dbDisconnect(Db)
}

UpdateModelPredictions <- function(input, xtraVar){
    
    #step 1: get reviewed indexes and their swipe response 
    historyfile = "./data/SwipeHistory.sqlite"
    Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
    history_swipe_pos <- DBI::dbGetQuery(Db, "SELECT ind, swipe FROM history WHERE polarity='Pos' AND NatuRA=1" ) 
    history_swipe_neg <- DBI::dbGetQuery(Db, "SELECT ind, swipe FROM history WHERE polarity='Neg' AND NatuRA=1" ) 
    history_swipe_current <- DBI::dbGetQuery(Db, paste("SELECT ind FROM history WHERE polarity='",input$Polarity,"' AND NatuRA=1", sep = "") )
    DBI::dbDisconnect(Db)
    
    history_swipe_pos <- history_swipe_pos[!duplicated(history_swipe_pos$ind) & history_swipe_pos$swipe != "Up", ]
    history_swipe_neg <- history_swipe_neg[!duplicated(history_swipe_neg$ind) & history_swipe_neg$swipe != "Up", ]
    
    if (nrow(history_swipe_pos) > 0) rownames(history_swipe_pos) <- paste("Pos_", history_swipe_pos$ind, sep = "")  
    if (nrow(history_swipe_neg) > 0) rownames(history_swipe_neg) <- paste("Neg_", history_swipe_neg$ind, sep = "") 
    
    #step 2: get the time profiles matching the indices of history_swipe to train the model
    dat_pos <- read.table(file = "./data/shinyPosData.txt", sep = ",", header = TRUE)
    dat_neg <- read.table(file = "./data/shinyNegData.txt", sep = ",", header = TRUE)
    dat_current <- read.table(file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
    
    trainingData_pos <- dat_pos[dat_pos$index %in% history_swipe_pos$ind ,]
    if (nrow(trainingData_pos) > 0) rownames(trainingData_pos) <- paste("Pos_", trainingData_pos$index, sep = "") 
    trainingData_neg <- dat_neg[dat_neg$index %in% history_swipe_neg$ind ,]
    if (nrow(trainingData_neg) > 0) rownames(trainingData_neg) <- paste("Neg_", trainingData_neg$index, sep = "") 
    # reorder to match with history_swipe
    trainingData_pos <- trainingData_pos[match(history_swipe_pos$ind, trainingData_pos$index), (xtraVar+1):ncol(dat_pos)]
    trainingData_neg <- trainingData_neg[match(history_swipe_neg$ind, trainingData_neg$index), (xtraVar+1):ncol(dat_neg)]
    trainingData = rbind(trainingData_pos,trainingData_neg)
    
    trainingLabels_pos <- history_swipe_pos$swipe == "Right"
    trainingLabels_neg <- history_swipe_neg$swipe == "Right"
    trainingLabels = as.factor(c(trainingLabels_pos,trainingLabels_neg))
    
    RF.model <- randomForest::randomForest(x = trainingData,
                                           y = trainingLabels, 
                                           ntree = 500, 
                                           importance = TRUE)
    
    # step 3: use model to predict unseen data
    PredictData <- dat_current[dat_current$qSvsMB <= input$qValue & 
                                   dat_current$qSvsNC <= input$qValue & 
                                   !dat_current$index %in% history_swipe_current$ind, (xtraVar+1):ncol(dat_current)]
    
    if(any(!colnames(PredictData) == colnames(trainingData))){
        stop("Something is wrong with the training data and testing data columns. They do not match properly.")
    }
    
    predicted.probs <- stats::predict(object = RF.model, 
                                      newdata = PredictData, 
                                      type = "prob")[,2]
    
    # set the 'modelPredicted' variable of the Predicted Data to TRUE
    dat_current$modelPredicted[dat_current$qSvsMB <= input$qValue & 
                                   dat_current$qSvsNC <= input$qValue & 
                               !dat_current$index %in% history_swipe_current$ind] <- TRUE
    
    # change the 'predictVal' variable of the Predicted Data to the corresponding probability
    dat_current$predictVal[dat_current$qSvsMB <= input$qValue & 
                               dat_current$qSvsNC <= input$qValue & 
                           !dat_current$index %in% history_swipe_current$ind] <- as.numeric(predicted.probs)
    # write the new data
    write.table(dat_current, file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",")
}


ui <- fluidPage(
    headerPanel('This is the tindeResting template.'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                       textInput("useRname", "Your name", "Mr Meeseeks")
                ),
                column(6, 
                       radioButtons("Preference", "Preference", choiceNames = list(
                           icon("github-alt"),
                           icon("venus"),
                           icon("mars")
                       ),
                       choiceValues = list(
                           "kitten" , "female",  "male"
                       ), 
                       inline = TRUE
                       )
                )
            ),
            fluidRow(
                column(6,
                       selectInput("onlyNew", "Only new images", c("yes" = "yes", "no" = "no"))
                ),
                column(6, selectInput("swipeOrder", "Swipe Order", c("Top first" = "tfrst", "Random" = "rnd")))
            ),
            fluidRow(
                column(6, actionButton("undo", 
                                       "Oopsie daisy",  
                                       style="color:#fff; background-color:Crimson"),
                       align = "center", 
                       offset = 0 ))
            
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
        subset.selection <- rep(TRUE, length(list.files("data/images/")))
        if(input$onlyNew == "yes"){
            subset.selection[ getHistory(input) ] <- FALSE
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
                       swipe  = card_swipe()
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        
        # if(appVals$k %% nswipeReward == 0){
        #     nmales = length(list.files("www/male_celebs"))
        #     nfemales = length(list.files("www/female_celebs"))
        #     if(input$Preference == "male"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "kitten"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "female"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     }
        # }
        
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
        
    }) #close event observe.
    observeEvent( input$buttonLeft,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(selection.vector()[as.numeric(appVals$k)]),
                       swipe  = "Left"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        
        # if(appVals$k %% nswipeReward == 0){
        #     nmales = length(list.files("www/male_celebs"))
        #     nfemales = length(list.files("www/female_celebs"))
        #     if(input$Preference == "male"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "kitten"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "female"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     }
        # }
        
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
        
        
    }) #close event observe.
    observeEvent( input$buttonUp,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]),
                       swipe  = "Up"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        # if(appVals$k %% nswipeReward == 0){
        #     nmales = length(list.files("www/male_celebs"))
        #     nfemales = length(list.files("www/female_celebs"))
        #     if(input$Preference == "male"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "kitten"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "female"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     }
        # }
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({selection.vector()[as.numeric(appVals$k)]})
        
        
    }) #close event observe.
    observeEvent( input$buttonRight,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]),
                       swipe  = "Right"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        
        # if(appVals$k %% nswipeReward == 0){
        #     nmales = length(list.files("www/male_celebs"))
        #     nfemales = length(list.files("www/female_celebs"))
        #     if(input$Preference == "male"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "kitten"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     } else if(input$Preference == "female"){
        #         showModal(modalDialog(
        #             modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
        #             easyClose = TRUE
        #         ))
        #     }
        # }
        
        
        
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
    observeEvent( input$ModelPredict,{
        
        showModal(modalDialog(
            title = "Updating model predictions",
            "The model predictions have been updated. You will no longer see time profiles that the model flagged as uninteresting.",
            easyClose = TRUE
        ))
        
        UpdateModelPredictions(input, xtraVar)
    }) #close event observe.
    
    session$onSessionEnded(function() {
        source("app_data_updater.R")
        
    })
}

shinyApp(ui, server)
