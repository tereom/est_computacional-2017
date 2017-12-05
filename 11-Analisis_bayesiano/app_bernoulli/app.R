library(shiny)
library(tidyverse)

ui <- fluidPage(
    sidebarPanel(
        h4("Inicial"),
        numericInput("a", "a:", value = 4, min = 0, width = "100px"),
        numericInput("b", "b:", value = 4,  min = 0, width = "100px"),
        h4("Verosimilitud"),
        sliderInput("N", "Número de observaciones:", value = 20,
            min = 0, max = 150, step = 5),
        sliderInput("p", "% de éxitos observados", 
            value = 50, min = 0, max = 100)),
    mainPanel(
        h3("Beta-binomial"),
        plotOutput("plot", height="350px", width = "500px")
    )
)


server <- function(input, output){
    data <- reactive({   
        a <- input$a
        b <- input$b
        N <- input$N
        z <- N * input$p / 100
        return(list(a = a, b = b, N = N, z = z))
    })
    output$plot <- renderPlot({
        parametros <- data()
        ggplot(data_frame(x = c(0, 1)), aes(x)) + 
        stat_function(fun = dbeta, args = list(shape1 = parametros$a, 
            shape2 = parametros$b), aes(color = "inicial")) + # inicial
        stat_function(fun = dbeta, args = list(shape1 = parametros$z + 1, 
            shape2 = parametros$N - parametros$z + 1), 
            aes(color = "verosimilitud")) + # verosimilitud
        stat_function(fun = dbeta, args = list(shape1 = parametros$a + 
                parametros$z, shape2 =  parametros$N - parametros$z + 
                parametros$b), aes(color = "posterior")) +
        # geom_vline(xintercept = parametros$z / parametros$N, color = "gray", 
        #     alpha = 0.5, linetype="dotted") +
        labs(y = expression(p(theta)), colour = "", x = expression(theta))
    })
}

shinyApp(ui, server)

