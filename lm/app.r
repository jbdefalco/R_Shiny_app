#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Regression"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),
            actionButton("go", label = "Plot Linear Model")   
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
           tags$h3("Slope:"),
           verbatimTextOutput("slope"),
           tags$h3("Intercept:"),
           verbatimTextOutput("intercept"),
           tags$h3("Correlation Coefficient:"),
           verbatimTextOutput("correlation")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    lmdata <- reactiveValues()

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$go, {
        update_lm()
        })

    update_lm <- function() {
        lmdata$model <- lm(y ~ x, data = dataInput())
        }
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = "X", ylab = "Y")
    })
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = "X", ylab = "Y")
        if(!is.null(lmdata$model)) {
            abline(lmdata$model)
        }
    })
    
    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })

    output$slope <- renderText({
        if(!is.null(lmdata$model)) {
            coef(lmdata$model)[2]
        } else {
            "No model available"
        }
    })
    
    output$intercept <- renderText({
        if(!is.null(lmdata$model)) {
            coef(lmdata$model)[1]
        } else {
            "No model available"
        }
    })
    
    output$correlation <- renderText({
        if(!is.null(lmdata$model)) {
            cor(dataInput()$x, dataInput()$y)
        } else {
            "No model available"
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
