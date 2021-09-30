# libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(dplyr)
    library(glue)
    library(DT)
})

# setup ----


# functions ----
source("scripts/func.R") # helper functions

# user interface ----

## tabs ----
main <- fluidRow(
    column(width = 6,
           h2("Author Info"),
           textInput("given", "Given Name(s)"),
           textInput("family", "Family Name(s)"),
           actionButton("orcid_search", "Search ORCiD"),
           uiOutput("orcid_options")

           ),
    column(width = 6,
           h2("Authorship List"),
           DTOutput("author_table"))
)

## UI ----
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Basic Template",
        titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src = "custom.js")
        ),
        main
    )
)


# server ----
server <- function(input, output, session) {
    v <- reactiveValues(
        author_list = data.frame(orcid = character(0),
                                 given = character(0),
                                 family = character(0))
    )

    # orcid_search ----
    observeEvent(input$orcid_search, {
        v$matches <- get_orcid(family = input$family,
                             given = input$given)
    })

    # orcid_options ----
    output$orcid_options <- renderUI({
        boxes <- make_orcid_match_boxes(v$matches)

        tagList(h3(glue("{length(v$matches)} matches")),
                boxes)
    })

    # orcid_to_add ----
    observeEvent(input$orcid_to_add, {
        i <- as.integer(input$orcid_to_add)

        v$author_list <- v$author_list %>%
            dplyr::add_row(orcid = v$matches[[i]]$orcid,
                           given = v$matches[[i]]$name$given,
                           family = v$matches[[i]]$name$surname)
    })

    # author_table ----
    output$author_table <- renderDT({
        v$author_list
    })

}

shinyApp(ui, server)
