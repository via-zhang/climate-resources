library(shiny)
library(shinyWidgets)
library(bslib)
library(gsheet)
#mukai branch
# Define UI for app that draws a the summed raster product ----
# download spreadsheet from google
url ='https://docs.google.com/spreadsheets/d/1ofQVouiKKNqAaxeF12eVqOOTggYkknOUZFKT90sQoZY/edit?resourcekey=&gid=914033009#gid=914033009' 

df <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, header = T)
colnames(df)[c(5:7, 9:11)] <- c("Contact", "Lead_Name", "Location", "Link.to.Paper","Summary", "Key.Terms")
# if scientist doesn't want to be contacted, change email to "email not shared"
df$Email.Address <- ifelse(df$Contact != "Yes", paste0("email not shared"), df$Email.Address)

ui <- page_sidebar(
  # App title ----
  title = "Climate Advocates Resource Database",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    textInput("Input_Title", "Search by title:"),
    selectInput(inputId = "Input_KWIC",
                label = "Select keyword:",
                choices = c("Climate change", "Sustainability", "Fisheries",
                            "Social science", "Conservation", "Restoration",
                            "Pollution", "Marine"),
                multiple = T
    ),
    textInput("Input_geo", "Enter location:")
  ),
  
  submitButton(text = "Filter"),
  
  
  # Output: List of Relevant Research ----
  uiOutput('text')
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
idx_reactive <-  reactive({
  df_sub <- df
  
  if (isTruthy(input$Input_Title)) {
    # determine which rows of data contain the title 
    idx = grepl(input$Input_Title, df_sub$Name.of.paper, ignore.case = T)
    df_sub <- df_sub[idx,]
  }
  
  if (isTruthy(input$Input_KWIC)) {
    # determine which rows of data contain the key words 
    idx = grepl(input$Input_KWIC, df_sub$Key.Terms, ignore.case = T)
    df_sub <- df_sub[idx,]
  }
    
  # determine which rows of data contain correct locations 
  if (isTruthy(input$Input_geo)) {
    idx <- grepl(input$Input_geo, df_sub$Location, ignore.case = T)
    df_sub <- df_sub[idx,]
  }
  
  return(df_sub)
})

  output$text <- renderUI({
    sub <- idx_reactive()
    
    if (is.data.frame(sub) && nrow(sub) > 0){
      cards <- lapply(1:nrow(sub), function(i) {
        card(
          full_screen = TRUE,
          min_height = 180,
          card_header(sub$Name.of.paper[i]),
          card_body(sub$Summary[i]),
          card_footer(sub$Link.to.Paper[i])
        )
      })
      tagList(cards)
    } else {
      HTML("Try another search")
    }
  })
  
}

shinyApp(ui = ui, server = server)

