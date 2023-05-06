#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# It creates nodes that can be connected to build a network and returns its features.
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
    titlePanel("My network"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            sliderInput("n",
                        "n(nodes):",
                        min = 2,
                        max = 50,
                        step = 1,
                        value = 15),
            
            
            checkboxInput("show_nodelabs",
                          "Knotenlabels zeigen")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    nodes <- reactive({
        erdos.renyi.game(n = input$n, 
                         p.or.m = 0, 
                         type = "gnp")
    })
    
    output$distPlot <- renderPlot({
        
        # Select plot-type:
        # tst <- "b"
        # switch(tst,
        #        a = "Hello",
        #        b = "No")
        
        # TODO: Make the ring lattice flexible!
        
        
        g <- nodes()  # get rewired graph.
        
        # Determine layout:
        g_lay <- layout.circle(g)

    
    
    # Plot: ------------------------------
        ggplot(ggnetwork(g, layout = g_lay),
               aes(x = x, y = y, xend = xend, yend = yend),
               curvature = 15) +
            geom_edges(color = "darkgrey", size = 1
                       # arrow = arrow(length = unit(8, "pt"), type = "closed")
            ) +
            geom_nodes(aes(color = color, fill = color), size = 10) +
            # geom_nodetext_repel(aes(label = degree)) +
            scale_color_manual(values = colvec) +
            scale_fill_manual(values = colvec) +
            guides(color = "none", fill = "none") +
            # labs(caption = paste0("Durchmesser =", gr_dia,
            #                       "\n",
            #                       "Zentralisierung = ", sprintf("%.2f", round(gr_centr, 2)),
            #                       "\n",
            #                       "Dichte = ", sprintf("%.2f", round(gr_dens, 2)),
            #                       "\n",
            #                       "Assortativität = ", sprintf("%.2f", round(gr_ass, 2))
            # )
            # ) +
            theme_blank() +
            theme(plot.caption = element_text(size = 12))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
