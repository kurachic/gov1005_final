library(shiny)
library(shinythemes)
library(DT)
library(knitr)
library(kableExtra)
library(htmlTable)
library(xtable)
library(janitor)
library(tidyverse)

load("Data/workspace.RData")

# Define UI for application
ui <- fluidPage(theme = shinytheme("yeti"),
   
  # title
  titlePanel("Access to Justice Lab - Philadelphia Divorce Study"),
  
  # tabs
  tabsetPanel(
    # summary tab gives p values produced by t-tests
    tabPanel("Summary",
             tabsetPanel(
               tabPanel("About",
                        # explanations of what the data is about
                        h3("What is the Access to Justice Lab?"),
                        h5("The Access to Justice Lab is a research clinic at Harvard Law School. The Lab runs randomized
                           control trials in the legal system in order to assess how accessible various components are
                           to pro se litigants - people without laywers."),
                        h3("What is the Philadelphia Divorce Study?"),
                        h5("The Philadelphia Divorce Study randomized people looking for divorce into two groups - having a lawyer,
                           and not having a lawyer but having self-help materials. The Lab partnered with Philadelphia Legal
                           Services (PLA) and Philadelphia VIP to offer representation. Approximately a year into the study, budgetary 
                           restricts resulted in PLA terminating representation for divorce cases other than those
                           involving domestic violence or other special circumstances, resulting in increased referrals to 
                           Philadelphia VIP."),
                        h3("The Data"),
                        h5("The data here includes all 378 people who went through a 45 minute intake interview.
                           67 of these people were ultimately excluded from the study because their spouse had already filed for
                           divorce. For the purposes of this project, these people have been included in order to examine
                           the demographic and other information collected in the interview.")
               ),
               tabPanel("Comparison Table",
                 sidebarLayout(
                   sidebarPanel(
                     # allow users to only show rows where theres a significant difference
                     checkboxInput(inputId = "signif", label = "Significant Differences Only", value = FALSE),
                     # allow users to compare the variable to compare across
                     selectInput(inputId = "comp", label = "Select how to compare:", 
                                 choices = c(`PLA divorce practice status` = "pla",
                                             `Divorce filing status` = "fs",
                                             `Interpreter` = "int",
                                             `Treatment` = "trt"),
                                 selected = "pla")
                   ),
                   mainPanel(
                     tableOutput("mainTable")
                     )
                  )
               ),
               tabPanel("Comparison Plot",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "comps", label = "Select how to compare:", 
                                        choices = c(`PLA divorce practice status` = "legaiddiv_p_val",
                                                    `Divorce filing status` = "filing_p_val",
                                                    `Interpreter` = "interp_p_val",
                                                    `Treatment` = "trted_p_val",
                                                    `Include all` = "all"),
                                        selected = "legaiddiv_p_val"),
                            checkboxGroupInput(inputId = "plotvars", label = "Select variables to display:",
                                               choices = table_data$var,
                                               selected = table_data$var),
                            actionButton("selectall", label="Select/Deselect all")
                          ),
                          mainPanel(
                            plotOutput("mainPlot")
                          )
                        )
                )
             )
    ),
    tabPanel("Demographics", 
             sidebarLayout(
               # allow selection of bins
               sidebarPanel(width = 3, sliderInput("ageBins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 30,
                                                   value = 15)
               ),
               
               mainPanel(
                 h2("Race"),
                 # static table with race percentages, not sure there's anything too interesting here
                 h5("The percentage of participants belonging to each racial category is displayed below.", pct_blck, "of clients were black."),
                 plotOutput("racePlot"),
                 h2("Age"),
                 # age histogram
                 h5("Below is a histogram of participant's ages."),
                 plotOutput("agePlot")
               )
             )
    ),
    tabPanel("Income",
             sidebarLayout(
               sidebarPanel(width = 3, sliderInput("wageBins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 30,
                                                   value = 15)
               ),
               
               mainPanel(
                 # notice participant wages lower than opposition party wages
                 h2("Participant Wages"),
                 # report percent unemployed
                 h5(paste0(round((number_zero_cl$n / 311) * 100), "%", " of participants are unemployed and do not earn monthly wages. For those participants who do earn monthly wages, a histogram of their wages is presented below")),
                 plotOutput("wagePlotCl"),
                 # report percent on government benefits
                 h5(paste0(round((ben_data$n / 311) * 100), "%", " have other sources of support, such as welfare, SSI, SSDI, spousal support, unemployment, food stamps, or other benefits.")),
                 h2("Opposition Party Wages"),
                 # report percent unemployed
                 h5(paste0(round((number_zero_op$n / 311) * 100), "%", " of opposition parties are unemployed and do not earn monthly wages. Below is a histogram of the opposition party's approximate monthly income.")),
                 plotOutput("wagePlotOP")
               )
             )
    ),
    tabPanel("Assets",
      mainPanel(
        h2("Assets"),
        plotOutput("assetPlot")
      )
    ),
    tabPanel("Marriage",
             sidebarLayout(
               # allow selection of bins
               sidebarPanel(width = 3, sliderInput("marrBins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 30,
                                                   value = 15)
               ),
             
             mainPanel(
               # some people have been married a whiiiile but most people are around 10 years or less
               h2("Marriage Length"),
               h5("Below is a histogram of the length of the participants' marriages in years."),
               plotOutput("marrPlot")
             )
      )
    ),
    tabPanel("Children",
      mainPanel(
        h5("A bar graph of how many children clients have is
           displayed below.",none, "have no children."),
        plotOutput("childPlot")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="plotvars",
                                 choices = table_data$var,
                                 selected = table_data$var)
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="plotvars",
                                 choices = table_data$var,
                                 selected = c())
        
      }}
  })
  # this is a funky chunk of nested if statements because for some fo them if i tried to
  # do if else or whatever it would only look at the very last one i.e. only one
  # of 3 options would actually result in the table displayed
  # basically im saying there's a reason that these if statements are nested the way
  # they are

  # also the reason that I can't just filter the data is that the grouping of rows needs to
  # be manually changed. Unless I'm missing some way to do it automatically, but I'm not to my
  # knowledge
  
  output$mainTable <- function() {
    
    # if they want to compare by PLA status
    if (input$comp == "pla") {
      # get just the legal aid div data and then set the appropriate column names
        filteredData <- table_data %>%
          mutate(p_val = legaiddiv_p_val) %>%
          select(var, pre, post, mean_diff_legaiddiv, p_val)
        columnnames <- c("Variable", "Pre-cessation", "Post-cessation", "Mean Difference", "P Value")
        
        # if they don't want to only see significant rows
        if(input$signif == FALSE) {
          filteredData %>%
            knitr::kable("html", col.names = columnnames) %>%
            kable_styling("striped", full_width = F) %>%
            group_rows("Demographics", start_row = 1, end_row = 13) %>%
            group_rows("Income", start_row = 14, end_row = 25) %>%
            group_rows("Assets", start_row = 26, end_row = 54) %>%
            group_rows("Marriage", start_row = 55, end_row = 68) %>%
            group_rows("Family", start_row = 69, end_row = 73) %>%
            add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                           has been removed because there are no clients who are the payee of a spousal support order."))
        }
        # else show them only significant rows
        else{
          filteredData <- filter(filteredData, p_val <= 0.05)
          filteredData %>%
            knitr::kable("html", col.names = columnnames) %>%
            kable_styling("striped", full_width = F) %>%
            group_rows("Demographics", start_row = 1, end_row = 4) %>%
            group_rows("Income", start_row = 5, end_row = 8) %>%
            group_rows("Assets", start_row = 9, end_row = 26) %>%
            group_rows("Marriage", start_row = 27, end_row = 31) %>%
            group_rows("Family", start_row = 32, end_row = 35) %>%
            add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                           has been removed because there are no clients who are the payee of a spousal support order."))
        }
    }
    # if they want to compare by filing status
    else {
      if (input$comp == "fs"){
        filteredData <- table_data %>%
          mutate(p_val = filing_p_val) %>%
          select(var, none_filed, spouse_filed, mean_diff_filing, p_val)
        columnnames <- c("Variable", "Nothing Filed", "Spouse Filed", "Mean Difference", "P Value")
        
        # if they want to see everything show it
        if(input$signif == FALSE) {
          filteredData %>%
            knitr::kable("html", col.names = columnnames) %>%
            kable_styling("striped", full_width = F) %>%
            group_rows("Demographics", start_row = 1, end_row = 13) %>%
            group_rows("Income", start_row = 14, end_row = 25) %>%
            group_rows("Assets", start_row = 26, end_row = 54) %>%
            group_rows("Marriage", start_row = 55, end_row = 68) %>%
            group_rows("Family", start_row = 69, end_row = 73) %>%
            add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                           has been removed because there are no clients who are the payee of a spousal support order."))
        }
        # else show only significant
        else{
          filteredData <- filter(filteredData, p_val <= 0.05)
          filteredData %>%
            knitr::kable("html", col.names = columnnames) %>%
            kable_styling("striped", full_width = F) %>%
            group_rows("Demographics", start_row = 1, end_row = 2) %>%
            group_rows("Income", start_row = 3, end_row = 8) %>%
            group_rows("Assets", start_row = 9, end_row = 32) %>%
            group_rows("Marriage", start_row = 33, end_row = 37) %>%
            add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                           has been removed because there are no clients who are the payee of a spousal support order."))
        }
      }
      # else if they want to see it by interpreter
      else {
        if (input$comp == "int") {
          filteredData <- table_data %>%
            mutate(p_val = interp_p_val) %>%
            select(var, no_interp, yes_interp, mean_diff_interp, p_val)
          columnnames <- c("Variable", "No Interpreter", "Interpreter", "Mean Difference", "P Value")
          
          if(input$signif == FALSE) {
            filteredData %>%
              knitr::kable("html", col.names = columnnames) %>%
              kable_styling("striped", full_width = F) %>%
              group_rows("Demographics", start_row = 1, end_row = 13) %>%
              group_rows("Income", start_row = 14, end_row = 25) %>%
              group_rows("Assets", start_row = 26, end_row = 54) %>%
              group_rows("Marriage", start_row = 55, end_row = 68) %>%
              group_rows("Family", start_row = 69, end_row = 73) %>%
              add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                             has been removed because there are no clients who are the payee of a spousal support order."))
          }
          else{
            filteredData <- filter(filteredData, p_val <= 0.05)
            filteredData %>%
              knitr::kable("html", col.names = columnnames) %>%
              kable_styling("striped", full_width = F) %>%
              group_rows("Demographics", start_row = 1, end_row = 7) %>%
              group_rows("Assets", start_row = 8, end_row = 18) %>%
              group_rows("Marriage", start_row = 19, end_row = 24) %>%
              add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                             has been removed because there are no clients who are the payee of a spousal support order."))
          }
        }
        else {
          if (input$comp == "trt") {
            filteredData <- table_data %>%
              mutate(p_val = trted_p_val) %>%
              select(var, no_trted, yes_trted, mean_diff_trted, p_val)
            columnnames <- c("Variable", "Control", "Experimental", "Mean Difference", "P Value")
            
            if(input$signif == FALSE) {
              filteredData %>%
                knitr::kable("html", col.names = columnnames) %>%
                kable_styling("striped", full_width = F) %>%
                group_rows("Demographics", start_row = 1, end_row = 13) %>%
                group_rows("Income", start_row = 14, end_row = 25) %>%
                group_rows("Assets", start_row = 26, end_row = 54) %>%
                group_rows("Marriage", start_row = 55, end_row = 68) %>%
                group_rows("Family", start_row = 69, end_row = 73) %>%
                add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                               has been removed because there are no clients who are the payee of a spousal support order."))
            }
            else{
              filteredData <- filter(filteredData, p_val <= 0.05)
              filteredData %>%
                knitr::kable("html", col.names = columnnames) %>%
                kable_styling("striped", full_width = F) %>%
                group_rows("Demographics", start_row = 1, end_row = 1) %>%
                group_rows("Assets", start_row = 2, end_row = 6) %>%
                group_rows("Marriage", start_row = 7, end_row = 7) %>%
                add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                               has been removed because there are no clients who are the payee of a spousal support order."))
            }
          }
        }
      }
    }
  }
  
  # render the comparison plot
  
  output$mainPlot <- renderPlot ({
    filteredData <- plot_data %>%
      filter(var %in% input$plotvars)
    
    if (input$comps != "all") {
      filteredData <- filteredData %>%
        filter(key == input$comps)
    }
    
    if(length(input$plotvars) > 0) {
      ggplot(filteredData, aes(x=value, y = 1, col = key)) + geom_jitter() + geom_vline(xintercept = 0.05) + labs(x = "P Value", y = "")
    }
    else {
      h5()
    }
  })
  
  # wage of client
  output$wagePlotCl <- renderPlot({
    # generate bins based on input$wageBins from ui.R
    x    <- wage_data$MonthWageCl
    bins <- seq(0, 7000, length.out = input$wageBins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Participant Wages", xlab = "Wages")
  })
   
  # wage of opposing party
  output$wagePlotOP <- renderPlot({
    # generate bins based on input$wageBins from ui.R
    x    <- wage_data$AmtMnthIncOP
    bins <- seq(0, 7000, length.out = input$wageBins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Opposition Party Wages", xlab = "Wages")
  })
   
  # race histogram
  output$racePlot = renderPlot ({
    ggplot(race_data, aes(x = race, y = n)) + geom_bar(stat = "identity") + labs(x = "Race", y = "")
  })
   
  # age histogram
  output$agePlot <- renderPlot({
    # generate bins based on input$ageBins from ui.R
    x    <- as.numeric(all_data$age)
    bins <- seq(0, max(x, na.rm = TRUE), length.out = input$ageBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Participant Age", xlab = "Age")
  })
   
  # marriage histogram
  output$marrPlot = renderPlot({
    # generate bins based on input$marrBins from ui.R
    x    <- marr_data$lengthmar
    bins <- seq(0, max(x), length.out = input$marrBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Marriage Length", xlab = "Marriage Length in Years")
  })
  
  # child bar graph
  output$childPlot = renderPlot ({
    ggplot(all_data, aes(x = as.factor(num_chld))) + geom_bar() + labs(x = "Number of Children", y = "")
        
  })
  
  output$assetPlot = renderPlot ({
    ggplot(asset_tab, aes(x = var, y = n)) + geom_bar(stat = "identity") + coord_flip()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

