library(shiny)

ui <- fluidPage(
  titlePanel("My first Shiny app"),
  mainPanel(
    "Hello world!"
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)