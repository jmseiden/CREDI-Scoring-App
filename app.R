# setwd("C:/Users/Jonat/OneDrive - Harvard University/Documents/Git Hub/CREDI Scoring App")
library(jmscredi)
library(tidyverse)
library(ggplot2)
library(shinybusy)
library(shinyalert)

  ui <- fluidPage(
    
    useShinyalert(),

    titlePanel(
        title=div(img(src="credi_logo.jpg", width = "300px"),
                  "Scoring Application")
      ),

    sidebarLayout(
      sidebarPanel(
        
        #Some CSS 
        tags$head(
          tags$style(type="text/css", "select {width: 250px; }"),
          tags$style(type="text/css", ".span4 {width: 290px; }"),
          tags$style(type="text/css", ".well {width: 300px; }")
        ),
        #Reverse scoring option
        checkboxInput("reverse", "My data is NOT reverse-coded", TRUE),
        checkboxInput("itemlevel", "I want to preserve item-level data", FALSE),

        #File upload box
        fileInput("file1", "Choose CSV File",
                  accept = ".csv"
        ),

        #Download button
        conditionalPanel(
          condition = "output.success",
          downloadButton("scores", "Download processed data")
        ),

        conditionalPanel(
          condition = "output.run",
          downloadButton("log", "Download Log")
        )

      ),
      #The main panel will simply display the processed output
      mainPanel(
        
        textOutput("folder"),

        add_busy_bar(timeout = 1000, color = "#112446", centered = FALSE,
                     height = "8px"),

        conditionalPanel(
          condition = "!output.run",
          br(),
          p(tags$body("Please upload a CSV using the sidebar. Ensure that your CSV has a unique ID variable, an AGE variable, and CREDI variables.")),
          p(tags$body("You can specify if your data is already reverse-coded or not, and if you want to include your item-level data after processing.")),
          p(tags$body("Please see the",
            tags$a(href="https://cdn1.sph.harvard.edu/wp-content/uploads/sites/2435/2016/05/CREDI-Scoring-Manual-8-Jun-2018.pdf",
                   "CREDI scoring guide")),
            tags$body("for more information."))
          ),

        conditionalPanel(
          condition = "output.success",
          plotOutput("contents")
          ),

        conditionalPanel(
          condition = "output.failure",
          mainPanel("Error processing data. Please see log for details")
        )

      )
    )
  )


  server <- function(input, output) {
    
    #Create a dictionary for CREDI variable names
    load("environment.rda")
    
    vecQnames <- c(mest_df$Item, mest_df$CREDI_code, mest_df$CREDI_code_Apr17)

    #Confirm user will not upload PII
      shinyalert(
        title = "Usage Agreement",
        text = "This app is only to be used with data that DOES NOT include personally identifiable information (PII). By clicking \"I agree\" you are confirming that any data uploaded has been anonymized and does not include any PII or sensitive information." ,
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "I agree",
        confirmButtonCol = "#228B22",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
        
    #Clear data upon session end (maybe not necessary)
    # session$onSessionEnded(function() {
    #   if (!is.null(input$file1)) {
    #     file.remove(input$file1$datapath)
    #   }
    # })
    
    
    
    preprocessed <- reactive({
        #ShowDon't throw an error if nothing is uploaded yet.
        validate(
          need(input$file1 != "", "")
        )

        #Use the input file name to upload a raw CSV of the results
        inFile <- input$file1
        preprocessed <- readr::read_csv(inFile$datapath)
    })

    
    output$folder <- renderText({
      inFile <- input$file1
      inFile$datapath
    })
    
    ###Process the data and run the CREDI code, returning a list with the log and the scores (if successful)
    processed <- reactive({
        dat <- jmscredi::score(data = preprocessed(), interactive = FALSE, reverse_code = input$reverse)
        list(scores = dat$scores, log = dat$log)
    })

    log <- reactive({
      processed()$log
    })

    scores <- reactive({
      processed()$scores
    })

    #Check success of scoring
    output$success <- reactive({
      !is.null(scores())
    })
    outputOptions(output, "success", suspendWhenHidden = FALSE)

    output$run <- reactive({
      !is.null(log())
    })
    outputOptions(output, "run", suspendWhenHidden = FALSE)

    output$failure <- reactive({
      !is.null(log()) & is.null(scores())
    })
    outputOptions(output, "failure", suspendWhenHidden = FALSE)

    #Write the contents to a processing program to show when it's running
    output$contents <- renderPlot({
      scores() %>%
        mutate(age_band = ifelse(AGE < 6, "0-5",
                                 ifelse(AGE < 11, "6-11",
                                        ifelse(AGE < 17, "12-17",
                                               ifelse(AGE < 24, "18-24",
                                                      ifelse(AGE < 29, "25-29",
                                                             ifelse(AGE < 36, "30-36", "Overage"))))))) %>%
        pivot_longer(cols = c(OVERALL, SEM, MOT, LANG, COG),
                     values_to = "Score",
                     names_to = "Domain") %>%
        group_by(Domain, age_band) %>%
        summarise(Score = mean(Score, na.rm=TRUE), .groups = "keep") %>%
        ggplot(aes(x = factor(Domain), y=Score, fill = age_band)) +
        geom_bar(stat="identity", position="dodge") +
        xlab("CREDI domain score averages")
    })

    #Write a downloadable csv of processed dataset
    output$scores <- downloadHandler(
      filename = "Scored_CREDI_Data.csv",
      content = function(file) {
        #We print out all variables if item-level was selected
        if(input$itemlevel){
          write.csv(scores(), file, row.names = FALSE)
        }
        #Otherwise we remove the item-level data
        else {
          scores <- scores()[,!(names(scores()) %in% vecQnames)]
          write.csv(scores, file, row.names = FALSE)
        }
      }
    )

    #Write out the log in a nice .txt using code copied from CREDI package.
    output$log <- downloadHandler(
      filename = "log.txt",
      content = function(file) {
        sink(file, append = TRUE)
        for (l in 1:length(log())){
          if (is.character(log()[[l]])){
            writeLines(log()[[l]])
          } else {
            print(log()[[l]])
          }
        }
        sink()
      }
    )
  }
#
#   #Write out the log in a nice .txt using code copied from CREDI package.
#   output$errorlog <- reactive{(
#     errorlog <- function(file) {
#       sink(file, append = TRUE)
#       for (l in 1:length(log())){
#         if (is.character(log()[[l]])){
#           writeLines(log()[[l]])
#         } else {
#           print(log()[[l]])
#         }
#       }
#       sink()
#       return(errorlog)
#     }
#   )
#   }


shinyApp(ui, server)
