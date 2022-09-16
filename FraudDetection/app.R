setwd("C:/Users/farom/Downloads/School/Final Year Projects/New Project Latest Codes")

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(rpart)
library(randomForest)


# Build model
model <- randomForest(Is_Fraud ~ ., data = dataset, ntree = 500, mtry = 4, importance = TRUE)

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Page header
                headerPanel("Credit Card Fraud Detection.ai"),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input Parameters</h3>"),
                  
                  selectInput("Month", label = "Month:", 
                              choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12), 
                              selected = "July"),
                  sliderInput("Transaction_Time", "Transaction Time:",
                              min = 0.000, max = 23.000,
                              value = 12.000),
                  sliderInput("Amount", "Amount:",
                              min = -499.00, max = 1409.00,
                              value = 80.85),
                  selectInput("Use_Chip", label = "Use Chip:", 
                              choices = list("Swipe Transaction" = 1, "Online Transaction" = 2, "Chip Transaction" = 3), 
                              selected = "Online Transaction"),
                  selectInput("Merchant_State", label = "Merchant State:", 
                              choices = list("CA" = 1, "NE" = 2, "IL" = 3, "MO" = 4, "Switzerland" = 5, "IA" = 6, "TX" = 7, "Estonia" = 8, "NJ" = 9, "NV" = 10, "NY" = 11, "Japan" = 12, "AZ" = 13, "UT" = 14, "FL" = 15, "MI" = 16, "Mexico" = 17, "WA" = 18, "OH" = 19, "Dominican Republic" = 20, "NM" = 21, "China" = 22, "SC" = 23, "AK" = 24, "PA" = 25, "VA" = 26, "Portugal" = 27, "HI" = 28, "CT" = 29, "MA" = 30, "MN" = 31, "CO" = 32, "Italy" = 33, "GA" = 34, "Philippines" = 35, "Jamaica" = 36, "AR" = 37, "Canada" = 38, "OR" = 39, "WI" = 40), 
                              selected = "CA"),
                  sliderInput("MCC", "Merchant Category Codes:",
                              min = 1711, max = 9402,
                              value = 5499),

                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Month",
               "Transaction_Time",
               "Amount",
               "Use_Chip",
               "Merchant_State",
               "MCC"),
      Value = as.character(c(input$Month,
                             input$Transaction_Time,
                             input$Amount,
                             input$Use_Chip,
                             input$Merchant_State,
                             input$MCC)),
      stringsAsFactors = FALSE)
    
    Is_Fraud <- "Is Fraud"
    df <- rbind(df, Is_Fraud)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 2))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.")
      return("0 = Not Fraud : 1 = Fraud")
    } else {
      return("Server is ready for calculation.")
      return("0 = Not Fraud : 1 = Fraud")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
