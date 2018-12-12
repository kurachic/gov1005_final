library(shiny)
library(shinythemes)
library(knitr)
library(kableExtra)
library(tidyverse)

load("Data/workspace.RData")

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
   
  # title
  titlePanel("Access to Justice Lab - Philadelphia Divorce Study"),
  
  # tabs. i did navbarPage here after seeing someones app with this theme because the nice dark bar looks pretty
  navbarPage("A2J",
    # summary tab gives summary info, table and plot of p values produced by t-tests
    tabPanel("Summary",
             tabsetPanel(
               tabPanel("About",
                        # explanations of what the data is about. a lot of this stuff is already in the README, but this can be good
                        # if the user skipped the README
                        h3("What is the Access to Justice Lab?"),
                        h5("The Access to Justice Lab is a research clinic at Harvard Law School. The Lab runs randomized
                           control trials in the legal system in order to assess how accessible various components are
                           to pro se litigants - people without laywers."),
                        h3("What is the Philadelphia Divorce Study?"),
                        h5("The Philadelphia Divorce Study randomized people looking for divorce into two groups - having a lawyer,
                           and not having a lawyer but having self-help materials. The Lab partnered with Philadelphia Legal
                           Assistance (PLA) and Philadelphia VIP to offer representation. Approximately a year into the study, budgetary 
                           restricts resulted in PLA terminating representation for divorce cases other than those
                           involving domestic violence or other special circumstances, resulting in increased referrals to 
                           Philadelphia VIP."),
                        h3("The Data"),
                        h5("The data here includes all 378 people who went through a 45 minute intake interview.
                           67 of these people were ultimately excluded from the study because their spouse had already filed for
                           divorce. For the purposes of this project, these people have been included in order to examine
                           the demographic and other information collected in the interview.")
               ),
               # comparison table with means, mean difference, and p-values of variables. the table is nice for
               # seeing the direction of the differences and lets you also see which differences are 
               # statistically significant. 
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
                     h2("How to interpret the table"),
                     h5("The variables shown below are mostly dummy variables, coded as 1 or 0, so the means shown below reflect the percentage of respondents that were female,
                        of various races, etc. For variables that were not dummy variables, such as income, the mean is simply the mean. The mean difference is calculated
                        as the third column minus the second column and shows the direction of the difference."),
                      h5("The final column, P Value, shows whether that difference is 
                        statistically significant. A difference is considered statistically significant if the p-value is lower than 0.05. Some
                        statistically significant values make sense or are obvious. For example, there are significantly more Hispanic clients 
                        in the group who need interpreters compared to the group that did not need interpreters, which makes sense - they
                        probably speak Spanish. Other differences are harder to explain. Why do you think people who needed interpreters had longer marriages?"),
                     tableOutput("mainTable")
                     )
                  )
               ),
               # comparison plot displaying jittered plot of p-values. This doesn't let you see the direction of
               # the differences, but it's nice because it lets you visualize how many dots are significant differences
               # versus insignificant. The "all" option is especially nice because you can see whether a variable 
               # was significantly different across various comparisons.
               tabPanel("Comparison Plot",
                        sidebarLayout(
                          sidebarPanel(
                            # user can select which comparison groups to plot
                            selectInput(inputId = "comps", label = "Select how to compare:", 
                                        choices = c(`PLA divorce practice status` = "PLA Status",
                                                    `Divorce filing status` = "Filing Status",
                                                    `Interpreter` = "Interpreter",
                                                    `Treatment` = "Treatment",
                                                    `Include all` = "all"),
                                        selected = "PLA Status"),
                            # user can select which variables to plot
                            checkboxGroupInput(inputId = "plotvars", label = "Select variables to display:",
                                               choices = table_data$var,
                                               selected = table_data$var),
                            # user can easily select or deselect all variables
                            actionButton("selectall", label="Select/Deselect all")
                          ),
                          mainPanel(
                            h2("How to interpret the plot"),
                            h5("The vertical line is drawn at the point of statistical significance - 0.05. That means all values
                               to the left of this line are statistically significant, while those to the right of the line
                               are not statistically significant. The placement of the point on the y axis has no meaning."),
                            plotOutput("mainPlot")
                          )
                        )
                )
             )
    ),
    # demographics tab with race and age information
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
                 # bar graph of race, notice large percentage of black clients
                 h5("The percentage of participants belonging to each racial category is displayed below.", pct_blck, "of clients were black."),
                 plotOutput("racePlot"),
                 h2("Age"),
                 # age histogram
                 h5(paste0("Below is a histogram of participant's ages. The median age is ", median(all_data$age, na.rm = TRUE), ".")),
                 plotOutput("agePlot")
               )
             )
    ),
    # income tab that includes histograms and summary statistics of client and opposition wages. the bins
    # and the scale for the histogram is the same so that you can easily compare the wages
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
                 h5(paste0(round((number_zero_cl$n / 378) * 100), "%", " of participants are unemployed and do not earn monthly wages. For those participants who do earn monthly wages, a histogram of their wages is presented below.")),
                 plotOutput("wagePlotCl"),
                 # report percent on government benefits
                 h5(paste0(round((ben_data$n / 378) * 100), "%", " have other sources of support, such as welfare, SSI, SSDI, spousal support, unemployment, food stamps, or other benefits.")),
                 h2("Opposition Party Wages"),
                 # report percent unemployed
                 h5(paste0(round((number_zero_op$n / 378) * 100), "%", " of opposition parties are unemployed and do not earn monthly wages. Below is a histogram of the opposition party's approximate monthly income. Notice that
                           the opposition party wages tend to be higher.")),
                 plotOutput("wagePlotOP")
               )
             )
    ),
    # asset tab with asset plot, grouped by asset type. It's surprising that so many have houses considering
    # their low incomes.
    tabPanel("Assets",
      mainPanel(
        h2("Assets and Liabilities"),
        h5(paste0("Real estate, pensions/annuities, businesses, bank accounts, and automobiles are considered assets, while loans and credit cards are considered debts/liabilities. ", round((asset_tab$n[asset_tab$var == "Client has sole or joint ownership of any asset"] / 378) * 100), "%", " of participants
                  own some asset.")),
        plotOutput("assetPlot")
      )
    ),
    # marriage tab with histogram of marriage lengths
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
               h5(paste0("Below is a histogram of the length of the participants' marriages in years. The median marriage length is ", median(marr_data$lengthmar, na.rm = TRUE), ".
                         ")),
               plotOutput("marrPlot")
             )
      )
    ),
    # family tab with graph of number of children each client has
    tabPanel("Family",
      mainPanel(
        h2("Children"),
        h5("A bar graph of how many children clients have is
           displayed below.",none, "have no children."),
        plotOutput("childPlot")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # this allows users to select or deselect all variables
  
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
  # knowledge.
  
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
  
  # render the comparison plot, pretty simple
  
  output$mainPlot <- renderPlot ({
    filteredData <- plot_data %>%
      filter(var %in% input$plotvars)
    
    # it the input is all, no filtering is needed
    if (input$comps != "all") {
      filteredData <- filteredData %>%
        filter(key == input$comps)
    }
    
    if(length(input$plotvars) > 0) {
      ggplot(filteredData, aes(x=value, y = 1, col = key)) + geom_jitter() + geom_vline(xintercept = 0.05) + 
        labs(x = "P Value", y = "", col = "Comparison Group") +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }
    else {
      # this just results in showing nothing, which is fine. I've found that adding text doesn't show, and I'm
      # not terribly bothered by that. I just didn't want an error message.
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
   
  # wage of opposing party. same number of bins as previous for comparative purposes
  output$wagePlotOP <- renderPlot({
    # generate bins based on input$wageBins from ui.R
    x    <- wage_data$AmtMnthIncOP
    bins <- seq(0, 7000, length.out = input$wageBins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Opposition Party Wages", xlab = "Wages")
  })
   
  # race histogram
  output$racePlot = renderPlot ({
    ggplot(race_data, aes(x = race, y = n)) + geom_bar(stat = "identity") + labs(x = "Race", y = "Number of Participants") + 
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank()) 
  })
   
  # age histogram
  output$agePlot <- renderPlot({
    # generate bins based on input$ageBins from ui.R
    x    <- as.numeric(all_data$age)
    bins <- seq(0, max(x, na.rm = TRUE), length.out = input$ageBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = "Age", main = NA)
  })
   
  # marriage histogram with marriage length
  output$marrPlot = renderPlot({
    # generate bins based on input$marrBins from ui.R
    x    <- marr_data$lengthmar
    bins <- seq(0, max(x), length.out = input$marrBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = "Marriage Length in Years", main = NA)
  })
  
  # child bar graph, number of clients with x number of children
  output$childPlot = renderPlot ({
    ggplot(all_data, aes(x = as.factor(num_chld))) + geom_bar() + labs(x = "Number of Children", y = "Number of Participants") +
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank()) 
  })
  
  output$assetPlot = renderPlot ({
    # excluding this because this percent is included in the text above the plot
    data <- asset_tab %>%
      filter(var != "Client has sole or joint ownership of any asset")
    
    # in order to allow easier comparisons between various types of assets, the graph has been organized
    # by the type of asset. Because the labs are long, the graph has been flipped to be horizontal
    ggplot(data, aes(group, n)) + geom_bar(aes(fill = type), 
      width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
      theme(legend.position="top", legend.title = 
            element_blank(),
            axis.title.x=element_blank(), 
            axis.title.y=element_blank(),
            axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank()) + 
    coord_flip()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

