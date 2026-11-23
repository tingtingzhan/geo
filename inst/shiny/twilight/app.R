
library(shiny)

# @importFrom htmltools HTML tags
# @importFrom plotly plotlyOutput
# @importFrom shiny fluidPage

fluidPage(
  
  system.file('js', 'shiny_userTime.js', package = 'geo') |>
    readLines() |>
    paste(collapse = '\n') |>
    sprintf(fmt = "$(document).on(\'shiny:connected\', %s);") |>
    HTML() |>
    tags$script(),
  
  titlePanel(title = 'Twilight'),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 5,
      h4('This Interactive Globe Shows'),
      tags$ul(
        tags$li('Your current local time and time zone'),
        tags$li(
          'Where the Sun is directly overhead (the ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Subsolar_point', 
            'subsolar point'
          ),
          ', ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-hans/太阳直射点', 
            '太阳直射点'
          ),
          ')'
        ),
        tags$li(
          'Where the Moon is directly overhead (the sublunar point, 月下点) and the current ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://science.nasa.gov/moon/moon-phases/', 
            'Moon phase'
          ),
          ' (',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-hans/月相', 
            '月相'
          ),
          ')'
        ),
        tags$li(
          'The line separating day from night (the ', 
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Terminator_(solar)', 
            'terminator'
          ),
          ', ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-hans/晨昏圈', 
            '晨昏线'
          ),
          ')'
        ),
        tags$li(
          'The three zones of ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Twilight', 
            'twilight'
          ),
          ': ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Twilight#Civil_twilight',
            'civil'
          ),
          ', ', 
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Twilight#Nautical_twilight',
            'nautical'
          ),
          ', and ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Twilight#Astronomical_twilight',
            'astronomical'
          ),
          '.'
        )
      ),
      h4('Privacy'),
      'This app asks for permission to access your location.',
      br(),
      'If you agree, your coordinates will appear on the globe. Your location is not stored or shared.',
      br(),
      'If you decline, you can still explore everything else.'#,
      # br(),
      # ''
      #h4('Run locally'),
      #tags$pre(
      #  tags$code(
      #    '\nlibrary(remotes)\ninstall_github(\'tingtingzhan/geo\')\nadd_twilight()'
      #  )
      #)
    ),
    
    mainPanel(
      width = 7,
      plotly::plotlyOutput(outputId = 'globe', width = '100%', height = '600px')
    )
    
  )

  #verbatimTextOutput(outputId = 'userTime'),

) |>
  shinyApp(
    ui = _, 
    server = geo:::twilight_server
  )


