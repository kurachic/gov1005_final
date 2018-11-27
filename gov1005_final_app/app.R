library(shiny)
library(shinythemes)
library(DT)
library(knitr)
library(kableExtra)
library(htmlTable)
library(xtable)

load("Data/workspace.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
   
  titlePanel("Access to Justice Lab - Philadelphia Divorce Study"),
  
  tabsetPanel(
    tabPanel("Summary",
             mainPanel(
               tableOutput("mainTable")
             )
    ),
    # tabPanel("Summary",
    #          mainPanel(
    #            htmlOutput("mainTable")
    #          )
    # ),
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
                 dataTableOutput("raceTable"),
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
               plotOutput("marrPlot"),
               h2("Domestic Abuse")
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
  
  # output$mainTable <- renderUI({
  #   kable(table_data)
  # }, sanitize.text.function = function(x) x)
  # 
  output$mainTable <- renderTable({
    htmlTable(table_data,
              header = c("Variable", "Nothing Filed", "Spouse Filed", "Mean Difference", "P Value"),
              rgroup = (c("Demographic", "Income", "Assets", "Marriage", "Family")),
              n.rgroup = (c(13, 12, 30, 14, 5))
              )
    }, sanitize.text.function = function(x) x)
  
  # wage of client
  output$wagePlotCl <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- wage_data$MonthWageCl.x
    bins <- seq(0, max(x), length.out = input$wageBins + 1)
    # nb - try doing geom hist to see if it looks nicer
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Participant Wages", xlab = "Wages")
  })
   
  # wage of opposing party
  output$wagePlotOP <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- wage_data$AmtMnthIncOP.x
    bins <- seq(0, max(x), length.out = input$wageBins + 1)
    # nb - remove 20000 income from op graph and change scales to match
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Opposition Party Wages", xlab = "Wages")
  })
   
  # race histogram
  output$raceTable = renderDT({
    datatable(race_data, options = list(bPaginate = FALSE, bFilter = FALSE, bInfo = FALSE))
  })
   
  # age histogram
  output$agePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- combo_included$age.x
    bins <- seq(0, max(x), length.out = input$ageBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Participant Age", xlab = "Age")
  })
   
  # marriage histogram
  output$marrPlot = renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- marr_data$lengthmar.x
    bins <- seq(0, max(x), length.out = input$marrBins + 1)
     
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Histogram of Marriage Length", xlab = "Marriage Length in Years")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

