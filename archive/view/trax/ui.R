# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli
source('modules.R')

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(a(img(src='utelogo.png', height=19), 'ATAQ Lab',
                                  href='http://air.utah.edu'),
                                  class='uataq-head'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     .list=list(menuItem('UATAQ Homepage', href='http://air.utah.edu/', newtab=F, icon=icon('home')),
#                menuItem('Interactive Display', tabName='display', icon=icon('train')),
#                menuItem('Historic Data', href='http://meso1.chpc.utah.edu/mesotrax/', newtab=T, icon=icon('backward'))
#                # br(),
#                # apply(opts, 1, function(x){menuItem(x[1], tabName=x[2], icon=icon('minus'))}),
#                # menuItem('Diagnostics', icon=icon('flask'),
#                #          apply(opts.other, 1, function(x){menuSubItem(x[1], tabName=x[2], icon=icon('minus'))}))
#     )
#   )
# )
sidebar <- dashboardSidebar(disable=T) 

# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('styles.css')),
  # do.call(tabItems, lapply(c(opts$short, opts.other$short), function(x) tabItem(x, displayUI(x))))
  fluidRow(
    column(12, 
           do.call(function(...){tabBox(..., id='tab', width=NULL)}, 
                   apply(opts, 1,
                         function(x) tabPanel(x[1], mapUI(x[2]), value=x[2]))),
           # box(status='danger', width=NULL, collapsible=T,
           #     fluidRow(
           #       column(4, selectInput('left', label='Left', choices=opts$long)),
           #       column(4, selectInput('right', label='Right', choices=c('', opts$long)))
           #     ),
           #     dygraphOutput('ts')
           # )
           p('Measurements made in collaboration with ', 
             a('MesoWest', href='http://mesowest.utah.edu'))
    )
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ TRAX', skin='black',
              header, sidebar, body)