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
                    step = 1),
        checkboxGroupInput("checkGroup", 
                           h3("Race"), 
                           choices = list("Black" = "B", 
                                          "White" = "W", 
                                          "Asian" = "A",
                                          "Hispanic" = "H",
                                          "Else" = "E"),
                           selected = list("B", "W", "A", "H", "E")),
        checkboxGroupInput("checkGroup2", 
                           h3("Gender"), 
                           choices = list("Male" = "M", 
                                          "Female" = "F"),
                           selected = list("M", "F"))
    )
)



# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
    
    # Plot page ----------------------------------------------
    tabItem("plot",
            
            # Input and Value Boxes ----------------------------------------------
            fluidRow(
                valueBoxOutput("age"),
                valueBoxOutput("count"),
                valueBoxOutput("district")
            ),
            
            # Plot ----------------------------------------------
            fluidRow(
                tabBox(title = "Plot",
                       width = 12,
                       tabPanel("Age", plotlyOutput("plot_age")),
                       tabPanel("District", plotlyOutput("plot_district")),
                       tabPanel("Map", plotlyOutput("plot_map")))
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
        filter(MONTH >= input$monthSelect[1] & MONTH <= input$monthSelect[2] & RACE %in% input$checkGroup & GENDER %in% input$checkGroup2)
        return(police)
    })
    
    # Plot the Age Distribution graph
    output$plot_age <- renderPlotly({
        ggplot(data = swInput(), aes(x = AGE, y = as.numeric(AGE)/27, fill = AGE)) + geom_bar(stat = "identity") + ylab("Arrests") + xlab("Age")
    })
    
    # Plot the District Distribution graph
    output$plot_district <- renderPlotly({
        ggplot(data = swInput(), aes(x = COUNCIL_DISTRICT, y = as.numeric(COUNCIL_DISTRICT)/9)) + geom_bar(stat = "identity", color = "#009E73") + ylab("Arrests") + xlab("District")
    })
    
    # Plot the Map
    output$plot_map <- renderPlotly({
        my_plot <- ggplot(swInput(), aes(x = X, y = Y))+ geom_point(size = 1, color = "red") + ylab("Latitute") + xlab("Longtitude")
        my_plot

    })
    
    # The Average Age box
    output$age <- renderValueBox({
        sw <- swInput()
        num <- round(mean(sw$AGE, na.rm = T), 2)

        valueBox(subtitle = "Average Age", value = num, icon = icon("sort-numeric-asc"), color = "green")
    })
    
    #The total count box
    output$count <- renderValueBox({
        sw <- swInput()
        num <- round(nrow(sw))
        
        valueBox(subtitle = "Total Arrests", value = num, icon = icon("balance-scale"), color = "yellow")
    })
    
    #The district box
    output$district <- renderValueBox({
        sw <- swInput()
        num <- tail(names(sort(table(sw$INCIDENTNEIGHBORHOOD))), 1)
        
        valueBox(subtitle = "The neighborhood that most arrests take place", value = tags$p(num, style = "font-size: 50%"), color = "red")
    })
    
    # Data table 
    output$table <- DT::renderDataTable({
        subset(swInput(), select = c(ARRESTTIME, AGE, GENDER, RACE, INCIDENTNEIGHBORHOOD))
    })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
