library(shiny)

# The rdirichlet function is taken verbatim from the MCMCpack R package
rdirichlet <- function(n, alpha) {
  l <- length(alpha)
  x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  sm <- x %*% rep(1, l)
  return(x / as.vector(sm))
}

draw_noise <- function(k, myseed) {
  set.seed(myseed)
  sim_raw <- rdirichlet(1, rep(1, k))
  # To avoid floating point issues, represent rounded probabilities as integers
  # between zero and 100 (% points)
  sim <- as.integer(round(100 * drop(sim_raw)))
  
  # After rounding, probs. may not sum to one. To ensure this, choose a coordinate
  # at random and adjust it to satisfy the adding-up constraint
  adjust_me <- sample(1:k, 1)
  sim[adjust_me] <- 100 - sum(sim[-adjust_me])
  
  plot(1:k, sim, type = 'h', xlab = 'Option #', ylab = 'Percentage (%)', 
       ylim = c(0, max(sim) * 1.2))
  points(1:k, sim, pch = 20)
  text(x = 1:k, y = sim + 0.1 * max(sim), labels = paste0(sim, '%'))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rounded Dirichlet Noise"),
    textInput("seed", "Enter a random seed", value = 675689),
    numericInput("k", "Dimension of Dirichlet (k)", value = 20, min = 2, max = 30),
    actionButton("draw", "Draw!", icon = icon("dice")),
    plotOutput("distPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    sim_raw <- eventReactive(input$draw, { 
        set.seed(input$seed) 
        rdirichlet(1, rep(1, input$k)) 
    })
    
    output$distPlot <- renderPlot({
        # To avoid floating point issues, represent rounded probabilities as 
        # integers between zero and 100 (% points) 
        sim <- as.integer(round(100 * drop(sim_raw())))
        # After rounding, probs. may not sum to one. To ensure this, choose a 
        # coordinate at random and adjust it to satisfy the adding-up constraint
        adjust_me <- sample(1:length(sim), 1) 
        sim[adjust_me] <- 100 - sum(sim[-adjust_me])
        plot(sim, type = 'h', xlab = 'Option #', ylab = 'Percentage (%)',  
             ylim = c(0, max(sim) * 1.2)) 
        points(1:length(sim), sim, pch = 20)
        text(x = 1:length(sim), y = sim + 0.1 * max(sim), 
             labels = paste0(sim, '%'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
