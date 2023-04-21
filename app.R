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
library(ggnetwork)

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
        
        
        # Adding centralities:
        # TODO
            degree_distribution(graph_reg)
            
            betweenness(graph_reg)
            eigen_centrality(graph_pa)
            
        # Adding asssortment:
            # TODO
            
            tst <- ggplot(graph_reg,
                   aes(x = x, y = y, xend = xend, yend = yend)) +
                geom_edges(color = "darkgrey", size = 1,
                           arrow = arrow(length = unit(8, "pt"), type = "closed")
                )
            
            ggplot(ggnetwork(graph_reg, layout = layout_in_circle(graph_reg)),
                   aes(x = x, y = y, xend = xend, yend = yend)) +
                geom_edges(color = "darkgrey", size = 1,
                           arrow = arrow(length = unit(8, "pt"), type = "closed")
                )
                
            
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
        
        
        # TODO: Make conditional
        V(g_rewire)$label <- round(eigen_centrality(g_rewire)$vector, 2)
        V(g_rewire)$label <- round(betweenness(g_rewire), 2)
        
        
        # Plot:
        plot(g_rewire, layout = g_lay,
             vertex.label = V(g_rewire)$label,
             vertex.size = 25)
        
        

        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Can be passed on to users to run directly from github:
# runGitHub( "networkVisualizations", "nigradwohl")
