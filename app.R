library(shiny)
library(shinyWidgets)
library(bslib)
library(stringr)

### Data ###
# Read data from spreadsheet (connected to Google Form)
url ='https://docs.google.com/spreadsheets/d/1ofQVouiKKNqAaxeF12eVqOOTggYkknOUZFKT90sQoZY/export?format=csv' 
df <- read.csv(url, stringsAsFactors=FALSE, header = T)

sheet_id <- "Complaints_Database" # for issues

colnames(df)[c(5:7, 9:11)] <- c("Contact", "Lead_Name", "Location", "Link.to.Paper","Summary", "Key.Terms")

all_keywords <- str_to_title(unique(trimws(unlist(strsplit(df$Key.Terms, ",|;|/")))))
all_keywords <- gsub('Gis', 'GIS', all_keywords)
all_keywords <- sort(all_keywords[all_keywords != ""])

all_locations <- unique(trimws(unlist(strsplit(df$Location, ",|;|/"))))
all_locations <- sort(all_locations[all_locations != ""])

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
      
      /* Mobile view */
      @media (max-width: 768px) {
        .bslib-sidebar-layout .sidebar {
          display: none !important;
        }
        #mobile-filters {
          position: fixed;
          bottom: 0;
          left: 0;
          right: 0;
          width: 100%;
          z-index: 1000;
          background-color: #000080;
          box-shadow: 0 -4px 6px rgba(0,0,0,0.1);
          max-height: 70vh;
          overflow-y: auto;
          transition: transform 0.3s ease;
        }
        #mobile-filters.collapsed {
          transform: translateY(calc(100% - 50px));
        }
        #mobile-filter-toggle {
          width: 100%;
          background-color: #0071bc;
          color: white;
          border: none;
          padding: 12px;
          font-size: 16px;
          font-weight: bold;
          cursor: pointer;
          border-top-left-radius: 10px;
          border-top-right-radius: 10px;
          text-align: center;
          position: sticky;
          top: 0;
          z-index: 1;
        }
        #mobile-filter-content {
          padding: 15px;
          padding-bottom: 20px;
        }
        .bslib-sidebar-layout > .main {
          width: 100% !important;
          margin-left: 0 !important;
          padding-bottom: 70px;
        }
        body.filters-open .bslib-sidebar-layout > .main {
          padding-bottom: calc(70vh + 20px);
        }
      }
      
      /* Desktop view */
      @media (min-width: 769px) {
        #mobile-filters {
          display: none !important;
        }
      }
    "))
  ),
  title = "Climate Advocates Resource Database",
  fillable = TRUE,
  sidebar = sidebar(
    id = "desktop_sidebar",
    bg = "#000080",
    textInput("Input_Title", "Search by title:"),
    selectInput(inputId = "Input_KWIC",
                label = "Select keyword:",
                choices = all_keywords,
                multiple = T
    ),
    selectInput("Input_geo", 
                "Select location:",
                choices = all_locations,
                multiple = T),
    actionButton("reset_btn", "Reset Filters",
                 icon = icon("rotate-left"),
                 class = "btn-secondary",
                 style = "width: 100%; margin-top: 10px; color: white; background-color: #0071bc;"),
    hr(style = "border-top: 1px solid #ffffff50; margin: 20px 0;"),
    p("Are you a scientist?", style = "color: white; font-weight: bold; margin-bottom: 5px;"),
    actionButton("submit_work", "Submit Your Paper", 
                 onclick = "window.open('https://docs.google.com/forms/d/e/1FAIpQLScPwdR6mCdjOce15b8A0QMyXJb0mF3M9vW9iZOv_5nGv6Umkw/viewform', '_blank')",
                 icon = icon("paper-plane"),
                 class = "btn-primary",
                 style = "width: 100%; background-color: #28a745; border: none;")
  ),
  uiOutput('text'),
  
  # Mobile filter panel
  tags$div(
    id = "mobile-filters",
    bg = "#000080",
    style = "color: white",
    class = "collapsed",
    tags$button(
      id = "mobile-filter-toggle",
      onclick = "toggleMobileFilters()",
      "Filters ",
      tags$i(class = "fa fa-chevron-up", id = "mobile-toggle-icon")
    ),
    tags$div(
      id = "mobile-filter-content",
      textInput("Input_Title_mobile", "Search by title:",
                  width="100%"),
      selectInput(inputId = "Input_KWIC_mobile",
                  label = "Select keyword:",
                  choices = all_keywords,
                  multiple = T,
                  width="100%"
      ),
      selectInput("Input_geo_mobile", 
                  "Select location:",
                  choices = all_locations,
                  multiple = T,
                  width="100%"),
      actionButton("reset_btn_mobile", "Reset Filters",
                   icon = icon("rotate-left"),
                   class = "btn-secondary",
                   style = "width: 100%; margin-top: 10px; color: white; background-color: #0071bc;"),
      # --- MERGED SUBMIT SECTION ---
      hr(style = "border-top: 1px solid #ffffff50; margin: 20px 0;"),
      p("Are you a scientist?", style = "text-align: center;"),
      actionButton("submit_work_mobile", "Submit Your Paper", 
                   onclick = "window.open('https://docs.google.com/forms/d/e/1FAIpQLScPwdR6mCdjOce15b8A0QMyXJb0mF3M9vW9iZOv_5nGv6Umkw/viewform', '_blank')",
                   icon = icon("paper-plane"),
                   class = "btn-success",
                   style = "width: 100%; background-color: #28a745; border: none;")
    )
  ),
  # Toggle mobile filter panel
  tags$script(HTML("
    function toggleMobileFilters() {
      var panel = document.getElementById('mobile-filters');
      var icon = document.getElementById('mobile-toggle-icon');
      var body = document.body;
      
      panel.classList.toggle('collapsed');
      
      if (panel.classList.contains('collapsed')) {
        icon.classList.remove('fa-chevron-down');
        icon.classList.add('fa-chevron-up');
        body.classList.remove('filters-open');
      } else {
        icon.classList.remove('fa-chevron-up');
        icon.classList.add('fa-chevron-down');
        body.classList.add('filters-open');
      }
    }
  "))
)

### Server ###
server <- function(input, output, session) {
  
  # Sync mobile and desktop inputs
  observeEvent(input$Input_Title_mobile, {updateTextInput(session, "Input_Title", value = input$Input_Title_mobile)})
  observeEvent(input$Input_Title, {updateTextInput(session, "Input_Title_mobile", value = input$Input_Title)})
  
  observeEvent(input$Input_KWIC_mobile, {updateSelectInput(session, "Input_KWIC", selected = input$Input_KWIC_mobile)})
  observeEvent(input$Input_KWIC, {updateSelectInput(session, "Input_KWIC_mobile", selected = input$Input_KWIC)})
  
  observeEvent(input$Input_geo_mobile, {updateTextInput(session, "Input_geo", value = input$Input_geo_mobile)})
  observeEvent(input$Input_geo, {updateTextInput(session, "Input_geo_mobile", value = input$Input_geo)})
  
  # Reset filters
  observeEvent(c(input$reset_btn, input$reset_btn_mobile), {
    updateTextInput(session, "Input_Title", value = "")
    updateTextInput(session, "Input_Title_mobile", value = "")
    updateSelectInput(session, "Input_KWIC", selected = character(0))
    updateSelectInput(session, "Input_KWIC_mobile", selected = character(0))
    updateTextInput(session, "Input_geo", value = "")
    updateTextInput(session, "Input_geo_mobile", value = "")
  })
  
  
  idx_reactive <-  reactive({
    df_sub <- df
    if (isTruthy(input$Input_Title)) {
      idx = grepl(trimws(input$Input_Title), df_sub$Name.of.paper, ignore.case = T)
      df_sub <- df_sub[idx,]
    }
    if (isTruthy(input$Input_KWIC)) {
      for(keyword in input$Input_KWIC) {
        idx <- grepl(keyword, df_sub$Key.Terms, ignore.case = T)
        df_sub <- df_sub[idx,]
      }
    }
    if (isTruthy(input$Input_geo)) {
      idx <- grepl(input$Input_geo, df_sub$Location, ignore.case = T)
      df_sub <- df_sub[idx,]
    }
    return(df_sub)
  })
  
  observe({
    sub <- idx_reactive()
    if (nrow(sub) == 0) return()
    lapply(1:nrow(sub), function(i) {
      observeEvent(input[[paste0("report_", i)]], {
        current_report_paper(sub$Name.of.paper[i]) # "Remember" which paper was clicked
        showModal(modalDialog(
          title = paste("Report Issue:", sub$Name.of.paper[i]),
          textAreaInput("complaint_text", "What is wrong with this summary?", rows = 3),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("send_report", "Submit Report", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
      })
    })
  })
  
  observeEvent(input$send_report, {
    req(input$complaint_text)
    new_entry <- data.frame(
      Timestamp = as.character(Sys.time()),
      Paper = current_report_paper(),
      Complaint = input$complaint_text,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      sheet_append(sheet_id, data = new_entry) # Writes the data to Google
      removeModal()
      showNotification("Thank you! Your report has been saved.", type = "message")
    }, error = function(e) {
      showNotification("Error: Could not save to Google Sheets.", type = "error")
    })
  })
  
  # Render outputs
  output$text <- renderUI({
    sub <- idx_reactive()
    if (is.data.frame(sub) && nrow(sub) > 0){
      cards <- lapply(1:nrow(sub), function(i) {
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
        locations <- unlist(strsplit(sub$Location[i], ",|;|/")) |>
          lapply(trimws)
        location_pills <- lapply(locations, function(loc) {
          tags$span(
            class = "badge rounded-pill bg-success me-1 mb-1",
            style = "font-size: 0.85em;",
            loc
          )
        })
        author_text <- if(sub$Contact[i] == "Yes") {
          paste0("Lead Author: ", sub$Lead_Name[i], " (", str_to_lower(sub$Email.Address[i]), ")")
        } else {
          paste0("Lead Author: ", sub$Lead_Name[i])
        }
        paper_link <- sub$Link.to.Paper[i]
        layout_columns(
          card(
            full_screen = TRUE,
            onclick = sprintf("window.open('%s', '_blank')", paper_link),
            card_header(sub$Name.of.paper[i]),
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
                       "Read Paper"),
                actionLink(inputId = paste0("report_", i), 
                           label = "Improve Summary", 
                           icon = icon("circle-exclamation"),
                           style = "color: #dc3545; text-decoration: none;",
                           onclick = "event.stopPropagation();")
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