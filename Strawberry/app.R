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
    titlePanel("Chemical's Effect on Strawberry lbs/acre/application"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select_state",
                        "State:",
                        choices = c("CALIFORNIA", "FLORIDA", "MICHIGAN", "NEW JERSEY", "NEW YORK", "NORTH CAROLINA", "OREGON", "PENNSYLVANIA", "WASHINGTON", "WISCONSIN"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw 
server <- function(input, output) {
    #reactive_var <- reactive(input$select_state)
    filtered_data <- reactive({
        df %>% filter(.data[[State]] == .env$input$select_state)
    })
    
   # new_df <- df[df$State == reactive_var,]
   # dataInput <- reactive({
   #     switch(input$select_state,
   #            "CALIFORNIA" = df[df$State == "CALIFORNIA", ],
   #            "FLORIDA"= df[df$State == "FLORIDA", ],
   #            "MICHIGAN"= df[df$State == "MICHIGAN", ],
   #            "NEW JERSEY"= df[df$State == "NEW JERSEY", ],
   #            "NEW YORK"= df[df$State == "NEW YORK", ],
   #            "NORTH CAROLINA"= df[df$State == "NORTH CAROLINA", ],
   #            "OREGON"= df[df$State == "OREGON", ],
   #            "PENNSYLVANIA"= df[df$State == "PENNSYLVANIA", ],
   #            "WASHINGTON"= df[df$State == "WASHINGTON", ],
   #            "WISCONSIN"= df[df$State == "WISCONSIN", ])
   # })
    output$distPlot <- renderPlot({
        gg_point <- ggplot(df[["State" == input$select_state]]) + geom_point_interactive(aes(x = Year, y = Value, tooltip = chemical, data_id = chemical)) 
        
        plot <- girafe(ggobj = gg_point)
        
        plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
