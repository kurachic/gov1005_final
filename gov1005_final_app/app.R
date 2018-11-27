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
   
  titlePanel("Access to Justice Lab - Philadelphia Divorce Study"),
  
  tabsetPanel(
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput(inputId = "signif", label = "Significant Differences Only", value = FALSE),
                 selectInput(inputId = "comp", label = "Select how to compare:", 
                             choices = c(`PLA divorce practice status` = "pla",
                                         `Divorce filing status` = "fs",
                                         `Interpreter` = "int"),
                             selected = "pla")
               ),
               mainPanel(
                 h3("What is the Access to Justice Lab?"),
                 h5("The Access to Justice Lab is a research clinic at Harvard Law School. The Lab runs randomized
                    control trials in the legal system in order to assess how accessible various components are
                    to pro se litigants - people without laywers."),
                 h3("What is the Philadelphia Divorce Study?"),
                 h5("The Philadelphia Divorce Study randomized people looking for divorce into two groups - having a lawyer,
                    and not having a lawyer but having self-help materials."),
                 h3("The Data"),
                 h5("The data here includes all 378 people who went through a 45 minute intake interview.
                    67 of these people were ultimately excluded from the study because their spouse had already filed for
                    divorce. For the purposes of this project, these people have been included in order to examine
                    the demographic and other information collected in the interview."),
                 br(),
                 tableOutput("mainTable")
               )
             )
             
    ),
    tabPanel("Demographics", 
             sidebarLayout(
               sidebarPanel(width = 3, sliderInput("ageBins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 30,
                                                   value = 15)
               ),
               
               mainPanel(
                 h2("Race"),
                 h5("The percentage of participants belonging to each racial category is displayed below."),
                 tableOutput("raceTable"),
                 h2("Age"),
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
                 h2("Participant Wages"),
                 h5(paste0(round((number_zero_cl$n / 311) * 100), "%", " of participants are unemployed and do not earn monthly wages. For those participants who do earn monthly wages, a histogram of their wages is presented below")),
                 plotOutput("wagePlotCl"),
                 h5(paste0(round((ben_data$n / 311) * 100), "%", " have other sources of support, such as welfare, SSI, SSDI, spousal support, unemployment, food stamps, or other benefits.")),
                 h2("Opposition Party Wages"),
                 h5(paste0(round((number_zero_op$n / 311) * 100), "%", " of opposition parties are unemployed and do not earn monthly wages. Below is a histogram of the opposition party's approximate monthly income.")),
                 plotOutput("wagePlotOP")
               )
             )
    ),
    tabPanel("Assets",
      mainPanel(
        h2("Assets")
      )
    ),
    tabPanel("Marriage",
             sidebarLayout(
               sidebarPanel(width = 3, sliderInput("marrBins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 30,
                                                   value = 15)
               ),
             
             mainPanel(
               h2("Marriage Length"),
               h5("Below is a histogram of the length of the participants' marriages in years."),
               plotOutput("marrPlot")
             )
      )
    ),
    tabPanel("Children",
      mainPanel("Children")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$mainTable <- function() {
    
    
    if (input$comp == "pla") {
        filteredData <- table_data %>%
          mutate(p_val = legaiddiv_p_val) %>%
          select(var, pre, post, mean_diff_legaiddiv, p_val)
        columnnames <- c("Variable", "Pre-cessation", "Post-cessation", "Mean Difference", "P Value")

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
            group_rows("Demographics", start_row = 1, end_row = 4) %>%
            group_rows("Income", start_row = 5, end_row = 8) %>%
            group_rows("Assets", start_row = 9, end_row = 26) %>%
            group_rows("Marriage", start_row = 27, end_row = 31) %>%
            group_rows("Family", start_row = 32, end_row = 35) %>%
            add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                           has been removed because there are no clients who are the payee of a spousal support order."))
        }
    }
    else {
      if (input$comp == "fs"){
        filteredData <- table_data %>%
          mutate(p_val = filing_p_val) %>%
          select(var, none_filed, spouse_filed, mean_diff_filing, p_val)
        columnnames <- c("Variable", "Nothing Filed", "Spouse Filed", "Mean Difference", "P Value")
        
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
            group_rows("Demographics", start_row = 1, end_row = 2) %>%
            group_rows("Income", start_row = 3, end_row = 8) %>%
            group_rows("Assets", start_row = 9, end_row = 32) %>%
            group_rows("Marriage", start_row = 33, end_row = 37) %>%
            add_footnote(c("The variable measuring whether the client is the current payee of a spousal support order
                           has been removed because there are no clients who are the payee of a spousal support order."))
        }
      }
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
      }
    }
  }
  
  # wage of client
  output$wagePlotCl <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- wage_data$MonthWageCl
    bins <- seq(0, 7000, length.out = input$wageBins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Participant Wages", xlab = "Wages")
  })
   
  # wage of opposing party
  output$wagePlotOP <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- wage_data$AmtMnthIncOP
    bins <- seq(0, 7000, length.out = input$wageBins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Opposition Party Wages", xlab = "Wages")
  })
   
  # race histogram
  output$raceTable <- function () {
    race_data %>%
      knitr::kable("html", col.names = c("Race", "Percent")) %>%
      kable_styling("striped", full_width = F)
  }
   
  # age histogram
  output$agePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- as.numeric(all_data$age)
    bins <- seq(0, max(x, na.rm = TRUE), length.out = input$ageBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Participant Age", xlab = "Age")
  })
   
  # marriage histogram
  output$marrPlot = renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- marr_data$lengthmar
    bins <- seq(0, max(x), length.out = input$marrBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Marriage Length", xlab = "Marriage Length in Years")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

