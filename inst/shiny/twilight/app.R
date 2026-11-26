
library(shiny)

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
      'On each refresh,',
      tags$ul(
        tags$li(
          'Your current ', 
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Local_time', 
            'local time'
          ),
          ' (当地时间) and ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Time_zone', 
            'time zone'
          ),
          ' (',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-hans/时区列表', 
            '时区'
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
          ' (',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/wiki/曙暮光', 
            '曙暮光'
          ),
          '): ',
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
      h4('It Also Shows'),
      tags$ul(
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
          '), the ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Ecliptic',
            'ecliptic'
          ),
          ' plane (',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-hans/黃道面',
            '黄道面'
          ),
          '), and the ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Obliquity_of_the_ecliptic',
            'obliquity of the ecliptic'
          ),
          ' (',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-cn/轉軸傾角',
            '黄赤交角'
          ),
          ')'
        ),
        tags$li(
          'Where the Moon is directly overhead (the sublunar point, 月下点), the current ',
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
          '), the ',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Orbit_of_the_Moon',
            'lunar orbital plane'
          ),
          ' (',
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://zh.wikipedia.org/zh-cn/白道面',
            '白道面'
          ),
          ') and the inclination of the lunar orbit to the equator (白赤交角)'
        ),
        tags$li(
          'The two points at which the lunar orbit intersects the ecliptic (', 
          tags$a(
            .noWS = c('before', 'after'),
            href = 'https://en.wikipedia.org/wiki/Lunar_node',
            'lunar nodes'
          ),
          ') and the lunar orbital inclination (黄白交角)'
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


