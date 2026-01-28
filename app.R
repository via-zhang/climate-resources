library(shiny)
library(bslib)
library(gsheet)

# Define UI for app that draws a the summed raster product ----
# download spreadsheet from google 
url ='https://docs.google.com/spreadsheets/d/1ofQVouiKKNqAaxeF12eVqOOTggYkknOUZFKT90sQoZY/edit?resourcekey=&gid=914033009#gid=914033009' 

df <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, header = T)
colnames(df)[c(5:7, 9:11)] <- c("Contact", "Lead_Name", "Location","Link.to.Paper","Summary", "Key.Terms")
# if scientist doesn't want to be contacted, change email to "email not shared"
df$Email.Address <- ifelse(df$Contact != "Yes", paste0("email not shared"), df$Email.Address)

ui <- page_sidebar(
  # App title ----
  title = "Climate Advocates Resource Center",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    textInput("Input_KWIC", "Enter 1 keyword (Required):"),
    textInput("Input_geo", "Enter location (Required):")
  ),
  
  submitButton(text = "Apply Changes"),
  
  
  # Output: List of Relevant Research ----
  uiOutput('text')
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
idx_reactive <-  reactive({
  req(input$Input_KWIC)
    # determine which rows of data contain the key words 
    idx = grepl(input$Input_KWIC, df$Key.Terms, ignore.case = T)
    df_sub <- df[idx,]
    
    # determine which rows of data contain correct locations 
    req(input$Input_geo)
      idx <- grepl(input$Input_geo, df_sub$Location, ignore.case = T)
      df_sub2 <- df_sub[idx,]
    
    if(nrow(df_sub2 == 0)){df_subs <- c("Try another search")}
    return(df_sub2)
  })


  output$text <- renderUI({
    sub <- idx_reactive()
    if (is.data.frame(sub) == T){
      HTML(paste(sub$Lead_Name, " (",sub$Email.Address, ")<br>",
                 sub$Summary, "<br>",
                 sub$Link.to.Paper, sep = ""))
    }
    else(HTML(sub))
    
    
  })
  
}

shinyApp(ui = ui, server = server)

