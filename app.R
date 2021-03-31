# installr::uninstall.packages("jmscredi")
# devtools::install("C:/Users/Jonat/OneDrive - Harvard University/Documents/Git Hub/jms_credi")
# devtools::install_github("jmseiden/jms_credi")

library(jmscredi)
library(tidyverse)
library(ggplot2)
library(shinybusy)
library(shinyalert)
library(writexl)
library(readxl)
library(scales)
library(DT)

  ui <- fluidPage(
    
    useShinyalert(),

    titlePanel(
        title=div(img(src="credi_logo.jpg", width = "300px"),
                  "Scoring Application (version 0.1)"),
        
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
                      label = HTML(paste0("My data is ", "<b>","not","</b>", " reverse coded.", "<i>"," Selecting this option will have the program automatically reverse-code your data.","</i>")),
                      value = TRUE),
        
        checkboxInput(inputId = "itemlevel", 
                      label = HTML(paste0("I want to preserve item-level data.", "<i>", " Select this option if you want to keep the data from each CREDI question. By default, the program will automatically discard item-level responses in the scored data.","</i>")),
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
          p(tags$body("Please upload an Excel spreadsheet using the sidebar. Ensure that your file (.xslx or .csv) has a unique ID variable, an AGE variable, and CREDI variables.")),
          p(tags$body("You can specify if your data is already reverse-coded or not, and if you want to include your item-level data after processing.")),
          p(tags$body("Please see the",
            tags$a(href="https://cdn1.sph.harvard.edu/wp-content/uploads/sites/2435/2016/05/CREDI-Scoring-Manual-8-Jun-2018.pdf",
                   "CREDI scoring guide")),
            tags$body("for more information."))
          ),

        conditionalPanel(
          condition = "output.success",
          mainPanel("Success! Download scored data and log below.", style="color:green"),
          br(),
          br(),
          downloadButton("scores", "Download processed data"),
          downloadButton("log", "Download Log"),
          br(),
          br(),
          tableOutput("scoretable"),
          tableOutput("flaggedobs"),
          plotOutput("avgscores"),
          plotOutput("zscores")
          ),

        conditionalPanel(
          condition = "output.failure",
          mainPanel("Error processing data. Please see log for details", style="color:red"),
          br(),
          downloadButton("logfailure", "Download Log"),
          br(),
          tableOutput("logfailuredisp")
        )
      )
    )
  )

  server <- function(input, output, session) {
    
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
    
    #Check if shortform or longform scores were generated
    longform <- reactive({
      "OVERALL" %in% names(scores())
    })
    
    #Clean scores data a bit for plotting
    cleanscores <- reactive({
      scores() %>%
      rename_all(toupper) %>%
        mutate(`Age Band` = ifelse(AGE < 6, "0-5", ""),
               `Age Band` = ifelse(AGE >= 6 & AGE < 12, "6-11", `Age Band`),
               `Age Band` = ifelse(AGE >= 12 & AGE < 18, "12-17", `Age Band`),
               `Age Band` = ifelse(AGE >= 18 & AGE < 25, "18-24", `Age Band`),
               `Age Band` = ifelse(AGE >= 25 & AGE < 30, "25-29", `Age Band`),
               `Age Band` = ifelse(AGE >= 30 & AGE <= 36, "30-36", `Age Band`),
               `Age Band` = ifelse(AGE > 36, "Overage", `Age Band`),
               `Age Band` = ifelse(is.na(AGE), "Missing age", `Age Band`),
               `Age Band` = ordered(`Age Band`, levels = c("0-5", "6-11", "12-17","18-24","25-29", "30-36", "Overage", "Missing age")))
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
                    `% scored` = paste(round(sum(!is.na(SF)) / n(),3)*100,"%", sep = ""),
                    `Average SF Score` = mean(SF, na.rm = TRUE))
      }
    })

    output$flaggedobs <- renderTable({
        cleanscores() %>%
        filter(NOTES != "") %>%
        mutate(NOTES = gsub(pattern = "T.*:", replacement = "", x = NOTES)) %>% 
        mutate(NOTES = gsub(pattern = "\\.", replacement = "", x = NOTES)) %>% 
        rename(`Domains with fewer than 5 observations:` = NOTES) %>% 
        dplyr::select(ID, `Domains with fewer than 5 observations:`)
    }, digits = 0)    
        
    #Create a plot of average scores
    output$avgscores <- renderPlot({
      if( longform() ){
        cleanscores() %>%
          rename(Overall = OVERALL,
                 `Soc. Emo.` = SEM,
                 Motor = MOT,
                 Language = LANG,
                 Cognitive = COG,
                 `Short Form` =  SF) %>%
          pivot_longer(cols = c(Overall, `Soc. Emo.`, Motor, Language, Cognitive, `Short Form`),
                       values_to = "Score",
                       names_to = "Domain") %>%
          group_by(Domain, `Age Band`) %>%
          mutate(Domain = factor(Domain, levels = c("Soc. Emo.", "Motor", "Language", "Cognitive", "Overall", "Short Form"))) %>%
          summarise(Score = mean(Score, na.rm=TRUE), .groups = "keep") %>%
          ggplot(aes(x = Domain, y=Score, fill = `Age Band`)) +
          geom_bar(stat="identity", position="dodge") + 
          scale_y_continuous(limits=c(35,55), oob = rescale_none) +
          xlab("CREDI domain score averages") +
          labs(fill = "Age Band")
      }
      else {
        cleanscores() %>%
        rename(`Short Form` = SF) %>%
        group_by(`Age Band`) %>%
        ggplot(aes(x = `Age Band`, y = `Short Form`, fill = `Age Band`)) +
        geom_bar(stat = "identity") +
        xlab("Average CREDI Short Form scores by Age Band") 
      }
    })

    output$zscores <- renderPlot({
      if( longform() ){
      cleanscores() %>%
          rename(`Soc. Emo.` = Z_SEM,
                 Motor = Z_MOT,
                 Language = Z_LANG,
                 Cognitive = Z_COG,
                 Overall = Z_OVERALL,
                 `Short Form` = Z_SF) %>%
        pivot_longer(cols = c(`Soc. Emo.`, Motor, Language, Cognitive, Overall, `Short Form`),
                     values_to = "Scores",
                     names_to = "Domain") %>%
        mutate(Domain = factor(Domain, levels = c("Soc. Emo.", "Motor", "Language", "Cognitive", "Overall", "Short Form"))) %>%
        group_by(Domain) %>%
        ggplot(aes(x = Scores, group = Domain, fill = Domain)) +
        geom_density(alpha = .5) +
        xlab("Distribution of normed CREDI Z-scores")
      }
      else {
        cleanscores() %>%
          ggplot(aes(x = Z_SF)) +
          geom_density(alpha = .5) +
          xlab("Distribution of normed CREDI Short Form Z-scores")
        
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
    output$logfailuredisp <- outputlogsuccessdisp <-renderTable(unlist(log()),
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