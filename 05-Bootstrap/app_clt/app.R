library(shiny)

ui <- pageWithSidebar(
    h4("Parámetros"),
    sidebarPanel(
        radioButtons("dist", "Distribución que genera las observaciones:",
            list("Exponencial(1)" = "rexp",
                "Normal(0,1)" = "rnorm",
                "Log-normal(0,1)" = "rlnorm",
                "Uniforme(0,1)" = "runif",
                "Poisson(1)" = "rpois",
                "Bernoulli(0.5)" = "rbern", 
                "Elecciones" = "relec"
              )
        ),
        br(),
        sliderInput("n", "Número de observaciones en cada muestra:", value = 20,
            min = 1, max = 400),
        br(),
        sliderInput("k", "Número de muestras para aproximar distribución:", 
            value = 1000, min = 500, max = 5000),
        br()),
    mainPanel(
        plotOutput("plot_1", height="300px", width = "400px"),
        plotOutput("plot_2", height="300px", width = "400px"),
        p("La línea roja corresponde a la estimación de densidad por kernel 
            de n simulaciones de una distribución Normal."), 
        plotOutput("plot_3", height="300px", width = "400px")
    )
)


rbern <- function(n) sample(c(0, 1), n, replace = TRUE)
elec <- read.table('datos/datos_computos_casillas_presidente.txt', sep = '|',
    header = T)
relec <- function(n){
  sample_n(elec, n, replace = FALSE) %>% pull(LISTA_NOMINAL)
}

server <- function(input, output){
    data <- reactive({      
        if (input$dist == "rpois"){
            vals <- do.call(input$dist, list(n=input$n, lambda=1))
        }
        else{
            vals <- do.call(input$dist, list(n=input$n))
        }
        distname <- switch(input$dist,
            rexp = "distribución Exponecial", 
            rnorm = "distribución Normal", 
            rlnorm = "distribución Log-normal", 
            runif = "distribución Uniforme", 
            rpois = "distribución Poisson",
            rbern = "distibución Bernoulli", 
            relec = "tamaño de lista nominal")
        n <- input$n
        k <- input$k
        media <- switch(input$dist,
            rexp = 1, 
            rnorm = 0, 
            rlnorm = exp(1/2), 
            runif = 1/2, 
            rpois = 1,
            rbern = 0.5, 
            relec = mean(elec$LISTA_NOMINAL))
        varianza <- switch(input$dist,
          rexp = 1, 
          rnorm = 1, 
          rlnorm = (exp(1) - 1) * exp(1), 
          runif = 1/12, 
          rpois = 1,
          rbern = 0.25, 
          relec = (1 - n / nrow(elec)) * var(elec$LISTA_NOMINAL)
        )
        if(input$dist=="rpois"){
            x <- plyr::rdply(k, do.call(input$dist, list(n=n, lambda=1)))
        }else{
            x <- plyr::rdply(k, do.call(input$dist, list(n=n)))
        }
        x_long <- gather(x, sim, val, -.n)
        x_means <- x_long %>%
            group_by(.n) %>%
            dplyr::summarise(medias = mean(val))
        x_means$sim_norm <- rnorm(k, mean = media, sd = sqrt(varianza / n))
        sims_norm <- rnorm(2000, mean = media, sd = sqrt(varianza /n))
        return (list(distname = distname, fun = input$dist, vals = vals, k = k,
            x_plot_1 = filter(x_long, .n == 1), media = media, 
            varianza = varianza / n, x_plot_2 = x_means, sims_norm))
    })
  
    output$plot_1 <- renderPlot({
        resultados <- data()
        ggplot(resultados$x_plot_1, aes(x = val)) +
            geom_histogram(aes(y = ..density..), alpha = 1) +
            labs(title = paste(input$n, " observaciones de ", 
                resultados$distname, sep = ""), y = "")
    })
    
    output$plot_2 <- renderPlot({
        resultados <- data()
        ggplot(resultados$x_plot_2) +
            geom_histogram(aes(x = medias, y = ..density..), alpha = 1) +
            geom_density(aes(x = sim_norm), color = "red") +
            labs(title = paste("Distribución de la media usando ", resultados$k, 
                " muestras cada una \n de ", input$n, " observaciones de una ", 
                resultados$distname, sep=""), y = "")
    
    })
    output$plot_3 <- renderPlot({
        resultados <- data()
        ggplot(resultados$x_plot_2, aes(sample = medias)) +
            geom_abline(intercept = resultados$media, 
                slope = sqrt(resultados$varianza), color = "red") +
        stat_qq(alpha = 1) +
        labs(title = "qqnorm de la distribución de la media", x = "teóricos", 
            y = "muestra")
        })
}

shinyApp(ui, server)