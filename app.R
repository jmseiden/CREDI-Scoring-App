
library(jmscredi)
library(tidyverse)
library(ggplot2)

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        #Logo
        img(src='credi_logo.jpg', align = "middle", width='250px'),

        #Reverse scoring option
        checkboxInput("reverse", "Check here if your data IS NOT already reverse-coded", TRUE),

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
        conditionalPanel(
          condition = "output.success",
          plotOutput("contents")
          ),
        conditionalPanel(
          condition = "output.uploadfailure",
          mainPanel("Error uploading CSV. Please ensure file format is CSV.")
        ),
        conditionalPanel(
          condition = "output.failure",
          mainPanel("Error processing data. Please see log for details")
        )

      )
    )
  )

  server <- function(input, output) {

    preprocessed <- reactive({
        #ShowDon't throw an error if nothing is uploaded yet.
        validate(
          need(input$file1 != "", "")
        )

        #Use the input file name to upload a raw CSV of the results
        inFile <- input$file1
        preprocessed <- readr::read_csv(inFile$datapath)

    })

    #Run the CREDI code and retrieve the log
    log <- reactive({
        jmscredi::score(data = preprocessed(), interactive = FALSE, reverse_code = input$reverse)$log
    })

    #Run the CREDI code and retrieve the scores
    scores <- reactive({
      jmscredi::score(data = preprocessed(), interactive = FALSE, reverse_code = input$reverse)$scores
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
        pivot_longer(cols = c(OVERALL, SEM, MOT, LANG, COG),
                     values_to = "Score",
                     names_to = "Domain") %>%
        group_by(Domain) %>%
        summarise(Score = mean(Score)) %>%
        ggplot(aes(x = factor(Domain), y=Score, fill = Domain)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=round(Score,2)), vjust = 1.5) +
        theme(legend.position='none') +
        xlab("CREDI domain score averages")
    })

    #Write a downloadable csv of processed dataset
    output$scores <- downloadHandler(
      filename = "Scored_CREDI_Data.csv",
      content = function(file) {
        write.csv(scores(), file, row.names = FALSE)
      }
    )


    #Write a downloadable csv of processed dataset
    output$log <- downloadHandler(
      filename = "log.txt",
      content = function(file) {
        write.table(log(), file)
      }
    )

  }

shinyApp(ui, server)


