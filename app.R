#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Illustration of network graphs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            selectInput("graph",
                        "Graph type",
                        choices = list("Ring lattice",
                                       "Preferential attachment",
                                       "Fully connected"),
                        multiple = FALSE),
            
            sliderInput("n",
                        "n(nodes):",
                        min = 2,
                        max = 100,
                        value = 15),
            
            sliderInput("p_rewire",
                        "p(rewire):",
                        min = 0,
                        max = 1,
                        value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Pre-calculated graphs:
        # Regular graph:
        graph_reg <- make_chordal_ring(15,
                                       matrix(rep(3, 3), nr = 1))
        
        # Preferrential attachment:
        graph_pa <- sample_pa(n = 30, power = 2, directed = FALSE)
        
        output$distPlot <- renderPlot({
        
        # Select plot-type:
        # tst <- "b"
        # switch(tst,
        #        a = "Hello",
        #        b = "No")
            
            # TODO: Make the ring lattice flexible!
        
        g <- switch(input$graph,
                    "Ring lattice" = make_chordal_ring(n = 15,  # input$n,
                                                       matrix(rep(3, 3), nr = 1)),
                    "Preferential attachment" = sample_pa(n = input$n, 
                                                          power = 3, directed = FALSE))
        

        
        # TODO: Allow flexible layout!
        
        # g <- graph_reg
        
        g_rewire <- rewire(g, 
                             with = each_edge(prob = input$p_rewire))
        
        # Determine layout:
        g_lay <- switch(input$graph,
                        "Ring lattice" = layout.circle(g_rewire),
                        "Preferential attachment" = layout.auto(g_rewire))
        
        plot(g_rewire, layout = g_lay)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
