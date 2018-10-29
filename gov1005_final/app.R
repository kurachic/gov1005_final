library(shiny)

included_only <- read_csv("SelectCovariates_13 Oct 2016.csv")

all_data <- read_csv("philly_vars.csv")

labels <- read_excel("Covariate_labels.xlsx")

combo <- included_only %>%
  left_join(all_data, by = "RefNum")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 25,
                     value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("wagePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$wagePlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- combo_data[, 9] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

