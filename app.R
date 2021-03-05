library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(readr)

#load data
police = read_csv("police.csv")


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Pittsburgh Arrests")


# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",

        # Menu Items ----------------------------------------------
        menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
        menuItem("Table", icon = icon("table"), tabName = "table", badgeColor = "green"),
        sliderInput("monthSelect",
                    "Months of the Year:",
                    min = min(police$MONTH, na.rm = T),
                    max = max(police$MONTH, na.rm = T),
                    value = c(min(police$MONTH, na.rm = T), max(police$MONTH, na.rm = T)),
                    step = 1)
    )
)



# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
    
    # Plot page ----------------------------------------------
    tabItem("plot",
            
            # Input and Value Boxes ----------------------------------------------
            fluidRow(
                valueBoxOutput("age")
            ),
            
            # Plot ----------------------------------------------
            fluidRow(
                tabBox(title = "Plot",
                       width = 12,
                       tabPanel("Age", plotlyOutput("plot_age")))
            )
    ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Stats of Arrests", DT::dataTableOutput("table"), width = 12))
    )
)
)


ui <- dashboardPage(header, sidebar, body)


# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    
    # Reactive data function -------------------------------------------
    swInput <- reactive({
        police <- police %>%
        filter(MONTH >= input$monthSelect[1] & MONTH <= input$monthSelect[2])
        return(police)
    })
    
    # Plot the Age Distribution graph
    output$plot_age <- renderPlotly({
        ggplot(data = swInput(), aes(x = AGE, y = as.numeric(AGE)/27, fill = AGE)) + geom_bar(stat = "identity") + ylab("Arrests") + xlab("Age")
    })
    
    # The Average Age box
    output$age <- renderValueBox({
        sw <- swInput()
        num <- round(mean(sw$AGE, na.rm = T), 2)

        valueBox(subtitle = "Average Age", value = num, icon = icon("sort-numeric-asc"), color = "green")
    })
    
    # Data table 
    output$table <- DT::renderDataTable({
        subset(swInput(), select = c(ARRESTTIME, AGE, GENDER, RACE, INCIDENTNEIGHBORHOOD))
    })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
