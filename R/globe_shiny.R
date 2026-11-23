

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