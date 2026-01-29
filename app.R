library(shiny)
library(shinyWidgets)
library(bslib)
library(stringr)

### Data ###
# Read data from spreadsheet (connected to Google Form)
url ='https://docs.google.com/spreadsheets/d/1ofQVouiKKNqAaxeF12eVqOOTggYkknOUZFKT90sQoZY/export?format=csv' 
df <- read.csv(url, stringsAsFactors=FALSE, header = T)

colnames(df)[c(5:7, 9:11)] <- c("Contact", "Lead_Name", "Location", "Link.to.Paper","Summary", "Key.Terms")

all_keywords <- str_to_title(unique(trimws(unlist(strsplit(df$Key.Terms, ",|;|/")))))
all_keywords <- gsub('Gis', 'GIS', all_keywords)
all_keywords <- sort(all_keywords[all_keywords != ""])

### UI ###
ui <- page_sidebar(
  tags$head(
    tags$style(HTML("
      .navbar, .bslib-page-title, [data-bs-theme='light'] .navbar {
        background-color: #000080 !important;
        color: white !important;
      }
      
      .bslib-card {
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        cursor: pointer;
      }
      
      .bslib-card:hover {
        transform: scale(1.001);
        box-shadow: 0 8px 16px rgba(0,0,0,0.2);
      }
    "))
  ),
  title = "Climate Advocates Resource Database",
  fillable = TRUE,
  sidebar = sidebar(
    bg = "#000080",
    textInput("Input_Title", "Search by title:"),
    selectInput(inputId = "Input_KWIC",
                label = "Select keyword:",
                choices = all_keywords,
                multiple = T
    ),
    textInput("Input_geo", "Enter location:"),
    actionButton("reset_btn", "Reset Filters",
                 icon = icon("rotate-left"),
                 class = "btn-secondary",
                 style = "width: 100%; margin-top: 10px; color: white; background-color: #0071bc;")
  ),
  uiOutput('text')
)

### Server ###
server <- function(input, output, session) {
  # Reset filters
  observeEvent(input$reset_btn, {
    updateTextInput(session, "Input_Title", value = "")
    updateSelectInput(session, "Input_KWIC", selected = character(0))
    updateTextInput(session, "Input_geo", value = "")
  })
  
  idx_reactive <-  reactive({
    df_sub <- df
    
    if (isTruthy(input$Input_Title)) {
      # Title
      idx = grepl(trimws(input$Input_Title), df_sub$Name.of.paper, ignore.case = T)
      df_sub <- df_sub[idx,]
    }
    
    if (isTruthy(input$Input_KWIC)) {
      # Keywords (all selected)
      for(keyword in input$Input_KWIC) {
        idx <- grepl(keyword, df_sub$Key.Terms, ignore.case = T)
        df_sub <- df_sub[idx,]
      }
    }
    
    # Locations
    if (isTruthy(input$Input_geo)) {
      idx <- grepl(trimws(input$Input_geo), df_sub$Location, ignore.case = T)
      df_sub <- df_sub[idx,]
    }
    return(df_sub)
  })
  
  output$text <- renderUI({
    sub <- idx_reactive()
    if (is.data.frame(sub) && nrow(sub) > 0){
      cards <- lapply(1:nrow(sub), function(i) {
        
        # Keywords
        keywords <- str_to_title(unlist(strsplit(sub$Key.Terms[i], ",|;|/"))) |>
          lapply(trimws)
        keywords <- gsub('Gis', 'GIS', keywords)
        keyword_pills <- lapply(keywords, function(kw) {
          tags$span(
            class = "badge rounded-pill bg-primary me-1 mb-1",
            style = "font-size: 0.85em;",
            kw
          )
        })
        
        # Location
        locations <- unlist(strsplit(sub$Location[i], ",|;|/")) |>
          lapply(trimws)
        location_pills <- lapply(locations, function(loc) {
          tags$span(
            class = "badge rounded-pill bg-success me-1 mb-1",
            style = "font-size: 0.85em;",
            loc
          )
        })
        
        # Author name and email
        author_text <- if(sub$Contact[i] == "Yes") {
          paste0(sub$Lead_Name[i], " (", str_to_lower(sub$Email.Address[i]), ")")
        } else {
          sub$Lead_Name[i]
        }
        
        # Paper details
        paper_link <- sub$Link.to.Paper[i]
        layout_columns(
          card(
            full_screen = TRUE,
            onclick = sprintf("window.open('%s', '_blank')", paper_link),
            card_header(
              sub$Name.of.paper[i]
            ),
            card_body(
              p(sub$Summary[i]),
              div(
                style = "display: flex; justify-content: space-between; align-items: flex-start; flex-wrap: wrap;",
                div(tagList(keyword_pills)),
                div(tagList(location_pills))
              )
            ),
            card_footer(
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                tags$span(author_text),
                tags$a(href = sub$Link.to.Paper[i],
                       target = "_blank",
                       onclick = "event.stopPropagation();",
                       "Read Paper")
              )
            )
          )
        )
      })
      tagList(cards)
    } else {
      HTML("No papers found; try another search.")
    }
  })
}

shinyApp(ui = ui, server = server)