library(shiny)
library(httr)
library(DT)
library(jsonlite)
source("global.R")
# Define UI 
ui <- fluidPage(

h1("Pull Data from OpenSea Storefront", style = "text-align: center"),
fluidRow(
  column(12, align = "center",
actionButton(inputId = "clear_defaults", 
             label = "Clear All",
             style = "text-align:center")
)),
wellPanel(style = "text-align:center",
textInput(inputId = "asset_contract_address", 
          label = "Asset Contract Address",
          value = "0x495f947276749ce646f68ac8c248420045cb7b5e",
          width = "100%"),
textInput(inputId = "collection_name",
          label = "OS Collection Name (case sensitive)",
          value = "emotion-check-in",
          width = "100%"),
numericInput(inputId = "collection_size", 
             label = "Number of NFTs in Collection", 
             value = "300", 
             min = 1, 
             max = 20000,
             step = 100,
             width = "100%"),
fluidRow(
  column(12, align = "center",
         actionButton(inputId = "get_owners",
                      label = "Get Current Owners")
  ))
),
hr(),
h2("NFT Owner Table"),
fluidRow(
  column(12, align = "center",
         downloadButton(outputId = "download_owners", label = "Download Table")
  )),
"note: By default Microsoft Excel breaks token ids by treating them as rounded numbers.",
"Use another tool or change your Excel settings to avoid losing your token IDs.",
dataTableOutput("owner_tbl"),
hr(),
h2("Top Collectors"),
fluidRow(
  column(12, align = "center",
         downloadButton(outputId = "download_top", label = "Download Table")
  )),
dataTableOutput("top_tbl")
)

# Define server logic 
server <- function(input, output, session) {

  observeEvent(input$clear_defaults, { 
    updateTextInput(inputId = "asset_contract_address", value = "")
    updateTextInput(inputId = "collection_name", value = "")
    updateNumericInput(inputId = "collection_size", value = "")
    })
  
  owner <- eventReactive(input$get_owners, {
    
    withProgress(expr = {
    get_owners(asset_contract_address = input$asset_contract_address,
               collection_name = input$collection_name,
               collection_size = input$collection_size, 
               url = url) 
    },min = 0, max = 1, value = 0.5,
    message = "Using Free OpenSea Rate-Limited API", 
    detail = "It's working, be patient")
    
  })
  
  output$download_owners <- downloadHandler(
    filename = function(){ 
      paste(input$collection_name,"-",Sys.Date(),".csv", sep = "")
      }, 
    content = function(fname){ 
      data <- as.data.frame(owner())
      write.csv(data,  fname, row.names = FALSE)
      }, contentType = "text/csv"
  )
  
  output$owner_tbl <- renderDT(server = FALSE, {
    get_dt(owner())
  })
  
  output$download_top <- downloadHandler(
    filename = function(){ 
      paste(input$collection_name,"-top-owners-",Sys.Date(),".csv", sep = "")
    }, 
    content = function(fname){ 
      data <- top_collectors(owner())
      write.csv(data,  fname, row.names = FALSE)
    }, contentType = "text/csv"
  )
  
  output$top_tbl <- renderDT(server = FALSE, {
    get_dt( top_collectors(owner()) )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
