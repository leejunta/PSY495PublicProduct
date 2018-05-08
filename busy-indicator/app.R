library(shiny)
library(shinyjs)

source("helpers.R") # Load all the code needed to show feedback on a button click

ui <- fluidPage(
  useShinyjs(),
  tags$style(appCSS),
  radioButtons("select", label="Select an option",
              choices=list("This one is okay" = "ok",
                "This will give an error" = "error"),
              selected=character(0)),
  
  # Wrap the button in the function `withBusyIndicatorUI()`
  withBusyIndicatorUI(
    actionButton(
      "iat_submit",
      "Process data",
      class = "btn-primary"
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$iat_submit, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("iat_submit", {
      Sys.sleep(1)
      if (input$select == "error") {
        stop("choose another option")
      }
    })
  })
}

shinyApp(ui = ui, server = server)