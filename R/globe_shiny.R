
#' @importFrom htmltools HTML tags
#' @importFrom plotly plotlyOutput
#' @importFrom shiny fluidPage
globe_ui <- \() {
  
  fluidPage(
    
    system.file('extjs', 'shiny_userTime.js', package = 'geo') |>
      readLines() |>
      paste(collapse = '\n') |>
      sprintf(fmt = "$(document).on(\'shiny:connected\', %s);") |>
      HTML() |>
      tags$script(),
    
    #verbatimTextOutput(outputId = 'userTime'),
    plotlyOutput(outputId = 'globe')
    
  )

}


#' @importFrom shiny req
#' @importFrom plotly renderPlotly
twilight_server <- \(input, output, session) {
  
  #output$userTime <- renderPrint({
  #  req(input$userTime)
  #  input$userTime
  #}) # for debugging only
  
  output$globe <- renderPlotly(expr = {
    req(input$userTime)
    utc <- input$userTime$iso |> # ISO string
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
    add_twilight(time = utc) |>
      layout(
        title = utc |>
          as.POSIXct(tz = input$userTime$tz) |> 
          format(usetz = TRUE)
      )
  })
  
}