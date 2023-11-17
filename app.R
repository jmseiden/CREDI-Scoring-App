library(credi)
library(tidyverse)
library(ggplot2)
library(shinybusy)
library(shinyalert)
library(writexl)
library(readxl)
library(scales)
library(DT)
library(dscore)

  ui <- fluidPage(
    
    titlePanel(
        title=div(img(src="credi_logo.jpg", width = "300px"),
                  "Scoring Application (version 0.2)"),
        
        windowTitle= "CREDI Scoring App"
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
        checkboxInput(inputId = "reverse", 
                      label = HTML(paste0("My data are ", "<b>","not ","</b>", "already reverse coded.", "<i>"," Selecting this option will change LF9 codes so that LF102 so that 1 = No and 0 = Yes.","</i>")),
                      value = TRUE),
        
        checkboxInput(inputId = "itemlevel", 
                      label = HTML(paste0("I want to preserve item-level data.", "<i>", " Select this option if you want to keep the data from each CREDI question. By default, the program will automatically discard item-level responses in the scored data.","</i>")),
                      value = TRUE),
        
        checkboxInput(inputId = "dscore", 
                      label = HTML(paste0("Generate GSED d-scores in addition to CREDI scores")),
                      value = FALSE),
        
        #File upload box
        fileInput("file1", "Choose .xslx or .csv File",
                  accept = c(".xlsx", ".xls", ".csv")
        ),

        #Download button
        # conditionalPanel(
        #   condition = "output.success",
        #   downloadButton("scores", "Download processed data")
        # ),
        # 
        # conditionalPanel(
        #   condition = "output.run",
        #   downloadButton("log", "Download Log")
        # )

      ),
      #The main panel will simply display the processed output
      mainPanel(
        
        add_busy_bar(timeout = 1000, color = "#112446", centered = FALSE,
                     height = "8px"),

        conditionalPanel(
          condition = "!output.run",
          br(),
          p(HTML("<ul style='color:red'><h2>WARNING!</ul></h2>",
                 "<h3>WE HAVE RECENTLY UNCOVERED AN ISSUE WITH SHORT FORM CREDI SCORES</h3>",
                 "<i>CREDI Short Form scores generated before June 29, 2023 did not use all available information. 
                 We have fixed this bug, but also have noted that scores are underdispersed, meaning that the minimum and maximum Z-scores are higher and lower than they should be. 
                 We are currently considering how to solve this issue but do not recommend using Z-scores for the Short Form at the moment.</i>")),
          p(tags$body("Please upload an Excel spreadsheet or CSV file using the sidebar. Ensure that your file (.xslx or .csv) has a unique ID variable, an AGE variable, and CREDI variables.")),
          p(tags$body("You can specify if your data are already reverse-coded or not, and if you want to include your item-level data after processing.")),
          p(tags$body("Please see the",
            tags$a(href="https://credi.gse.harvard.edu/sites/projects.iq.harvard.edu/files/credi/files/credi_data_scoring_manual_31oct2023_0.pdf",
                   "CREDI Scoring Manual")),
            tags$body("for more information."))
          ),

        conditionalPanel(
          condition = "output.success",
          fluidRow(
            column(width = 12,
                   mainPanel("Success! Download scored data and log below.", style="color:green"))),
          fluidRow(
            column(width = 12,
                   downloadButton("scores", "Download processed data"))),
          fluidRow(
            column(width = 12,
                   downloadButton("logsuccess", "Download Log"))),
          fluidRow(
            column(width = 12,
                   tableOutput("logsuccessdisp"))),
          fluidRow(
            column(width = 12,
                   mainPanel("Below is a summary of scored responses by age band"))),  
          fluidRow(
            column(width = 12,
                   tableOutput("scoretable"))),
          fluidRow(
            column(width = 12,
                   mainPanel("Flagged observations have a low number of responses in the particular domain and are calculated but may be innacurate."))),
          fluidRow(
            column(width = 12,
                   tableOutput("flaggedobs"))),
          fluidRow(
            column(width = 12,
                   plotOutput("avgscores"))),    
          fluidRow(
            column(width = 12,
                   plotOutput("zscores"))),
          ),

        conditionalPanel(
          condition = "output.failure",
          fluidRow(
            column(width = 12,
                   mainPanel("Error processing data. Please see log for details. If you are unable to resolve the errors by examining the below, please contact jseiden@g.harvard.edu for assistance. ", 
                             style="color:red"))),
          fluidRow(
            column(width = 12,
                   tableOutput("logfailuredisp"))),
          fluidRow(
            column(width = 12,
                   downloadButton("logfailure", "Download Log"))),
          
        )
      )
    )
  )

  server <- function(input, output, session) {
    
    #Max upload size
    options(shiny.maxRequestSize=50*1024^2)
    
    #Create a dictionary for CREDI variable names
    load("environment.rda")
    
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

        #Use the input file name to upload a raw xlsx/csv of the results
        inFile <- input$file1
        
        datapath <- inFile$datapath

        ifelse(grepl(".csv", datapath),
               preprocessed <- read_csv(datapath),
               preprocessed <- readxl::read_excel(datapath)
        )
          
        preprocessed <- preprocessed %>%
          mutate_at(vars(intersect(varnames, names(preprocessed))), as.numeric)

    })
        
        datapath <- "something.csv" 
        
    ###Process the data and run the CREDI code, returning a list with the log and the scores (if successful)
    processed <- reactive({
        dat <- credi::score(data = preprocessed(), interactive = FALSE, reverse_code = input$reverse, dscore = input$dscore)
        list(scores = dat$scores, log = dat$log, dscores = dat$dscores)
    })

    log <- reactive({
      processed()$log
    })

    scores <- reactive({
      if(input$dscore) { #Attach d-score if the option is selected
        dscore <- processed()$dscores %>% 
          dplyr::select(dscore, DAZ, ID, dscore_sem)
        
        processed()$scores %>% 
          left_join(dscore, by = "ID")
      } else {
        processed()$scores
      }
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
    
    #Check if shortform or longform data were input
    longform <- reactive({
      "LANG" %in% names(scores())
    })
    
    #Clean scores data a bit for plotting
    cleanscores <- reactive({
      scores() %>%
      rename_all(toupper) %>%
        mutate(`Age Band` = ifelse(AGE < 6, "0-5", ""),
               `Age Band` = ifelse(AGE >= 6 & AGE < 12, "6-11", `Age Band`),
               `Age Band` = ifelse(AGE >= 12 & AGE < 18, "12-17", `Age Band`),
               `Age Band` = ifelse(AGE >= 18 & AGE < 24, "18-23", `Age Band`),
               `Age Band` = ifelse(AGE >= 24 & AGE < 30, "24-29", `Age Band`),
               `Age Band` = ifelse(AGE >= 30 & AGE <= 36, "30-36", `Age Band`),
               `Age Band` = ifelse(AGE > 36, "Overage", `Age Band`),
               `Age Band` = ifelse(is.na(AGE), "Missing age", `Age Band`),
               `Age Band` = ordered(`Age Band`, levels = c("0-5", "6-11", "12-17","18-23","24-29", "30-36", "Overage", "Missing age")))
      })

    #Create a table of the number of scores by age band
    output$scoretable <- renderTable({
      if( longform() ){
        cleanscores() %>%
          group_by(`Age Band`) %>%
          summarize(`Total obs` = n(),
                    `% scored` = paste(round(sum(!is.na(OVERALL)) / n(),3)*100,"%", sep = ""),
                    `Average Overall Score` = mean(OVERALL, na.rm = TRUE))
      }
      else {
        cleanscores() %>%
          group_by(`Age Band`) %>%
          summarize(`Total obs` = n(),
                    `% scored` = paste(round(sum(!is.na(OVERALL)) / n(),3)*100,"%", sep = ""),
                    `Average Overall Score` = mean(OVERALL, na.rm = TRUE))
      }
    })


    output$flaggedobs <- renderTable({
      if( longform() ){
        cleanscores() %>%
          rename(Overall = OVERALL_FLAG,
                 `Soc. Emo.` = SEM_FLAG,
                 Motor = MOT_FLAG,
                 Language = LANG_FLAG,
                 Cognitive = COG_FLAG) %>%
          pivot_longer(cols = c(Overall, `Soc. Emo.`, Motor, Language, Cognitive),
                       values_to = "Flagged",
                       names_to = "Domain") %>% 
          mutate(Domain = factor(Domain, levels = c("Soc. Emo.", "Motor", "Language", "Cognitive", "Overall"))) %>%
          group_by(Domain) %>% 
        summarize(`Flagged observations` = paste(round(mean(Flagged),3)*100,"%", sep = ""))
      }
      else{
        cleanscores() %>%
          mutate(Flagged = NOTES != "Only responses to short form items detected. Therefore, scoring will produce only a CREDI-SF score.") %>% 
          summarize(`Flagged observations` = paste(round(mean(Flagged),3)*100,"%", sep = ""))
      }
      }, digits = 2)
    
    #Create a plot of average scores
    output$avgscores <- renderPlot({
      if( longform() ){
        cleanscores() %>%
          rename(Overall = OVERALL,
                 `Soc. Emo.` = SEM,
                 Motor = MOT,
                 Language = LANG,
                 Cognitive = COG) %>%
          pivot_longer(cols = c(Overall, `Soc. Emo.`, Motor, Language, Cognitive),
                       values_to = "Score",
                       names_to = "Domain") %>%
          group_by(Domain, `Age Band`) %>%
          mutate(Domain = factor(Domain, levels = c("Soc. Emo.", "Motor", "Language", "Cognitive", "Overall"))) %>%
          summarise(Score = mean(Score, na.rm=TRUE), .groups = "keep") %>%
          ggplot(aes(x = Domain, y=Score, fill = `Age Band`)) +
          geom_bar(stat="identity", position="dodge") + 
          scale_y_continuous(limits=c(35,55), oob = rescale_none) +
          xlab("CREDI Overall and domain score averages") +
          labs(fill = "Age Band")
      }
      else {
        cleanscores() %>%
        group_by(`Age Band`) %>%
        summarize(Overall = mean(OVERALL)) %>%
        ggplot(aes(x = `Age Band`, y = Overall, fill = `Age Band`)) +
        geom_bar(stat = "identity") +
        xlab("Average CREDI Overall scores by Age Band") 
      }
    })

    output$zscores <- renderPlot({
      if( longform() ){
      cleanscores() %>%
          rename(`Soc. Emo.` = Z_SEM,
                 Motor = Z_MOT,
                 Language = Z_LANG,
                 Cognitive = Z_COG,
                 Overall = Z_OVERALL) %>%
        pivot_longer(cols = c(`Soc. Emo.`, Motor, Language, Cognitive, Overall),
                     values_to = "Scores",
                     names_to = "Domain") %>%
        mutate(Domain = factor(Domain, levels = c("Soc. Emo.", "Motor", "Language", "Cognitive", "Overall"))) %>%
        group_by(Domain) %>%
          mutate(average = mean(Scores)) %>% 
          ggplot(aes(x = Scores, group = Domain, fill = Domain)) +
            geom_density(alpha = .5) +
            geom_vline(aes(xintercept = average, color = Domain), show.legend = FALSE) +
            xlab("Distribution of normed CREDI Overall and domain Z-scores")
      }
      else {
        cleanscores() %>%
          ggplot(aes(x = Z_OVERALL)) +
          geom_density(alpha = .5) +
          xlab("Distribution of normed CREDI Overall Z-scores")
        
      }
    })    
    
    #Write a downloadable .xlsx of processed dataset
    output$scores <- downloadHandler(
      filename = "Scored_CREDI_Data.xlsx",
      content = function(file) {
        #We print out all variables if item-level was selected
        if(input$itemlevel){
          write_xlsx(scores(), 
                     file)
        }
        #Otherwise we remove the item-level data
        else {
          scores <- scores()[,!(names(scores()) %in% varnames)]
          write_xlsx(scores(), 
                     file)
        }
      }
    )

    #Write out the log in a nice .txt using code copied from CREDI package.
    output$logsuccess <- output$logfailure <- downloadHandler(
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

    
#Make the log legible in the the output
    output$logfailuredisp <- output$logsuccessdisp <-renderTable(unlist(log()),
                                    bordered = FALSE,
                                    striped = FALSE,
                                    rownames = FALSE,
                                    colnames = FALSE)
    
# Delete the temporary directory when the session ends (I am not sure if this is necessary)
  # session$onSessionEnded(
  #   function(){ 
  #     unlink(tempdir(), recursive = TRUE)
  #     })
  }  

shinyApp(ui, server)