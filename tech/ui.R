# Ben Fasoli

library(DT)
library(shiny)
library(shinydashboard)

# Homepage text
hometext <- includeMarkdown('homepage.md')
tanktext <- includeMarkdown('cylinder_request.md')


# ---------------------------------------------------------------------------------
# Header Layout
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'),
                          dropdownMenuOutput('tankMenu'),
                          dropdownMenuOutput('newsMenu'))


# ---------------------------------------------------------------------------------
# Sidebar Layout
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Homepage', tabName='homepage', icon=icon('home'), newtab=F),
    menuItem('Data', icon = icon('area-chart'), tabName='data',
             menuSubItem('Real-time map', href='http://air.utah.edu', icon=icon('globe'), newtab=F),
             menuSubItem('Interactive timeseries', href='http://air.utah.edu/s/gasview/', icon=icon('line-chart'), newtab=F),
             menuSubItem('TRAX light-rail', href='http://air.utah.edu/s/trax/', icon=icon('train'), newtab=F),
             menuSubItem('Nerdmobile mobile lab', href='http://air.utah.edu/s/nerdmobile/', icon=icon('flask'), newtab=F)),
    menuItem('Tools', icon = icon('th'), tabName='widgets',
             menuSubItem('EZMet Arduino GUI', href='http://air.utah.edu/s/ezmet/', icon=icon('cloud')),
             menuSubItem('Concentration Dilution', tabName='tankdilution', icon=icon('hourglass-half')),
             menuSubItem('Cylindrical Volume', tabName='cylvol', icon=icon('circle-o')),
             menuSubItem('Dewpoint to PPM', tabName='dewtoppm', icon=icon('tint')),
             # menuSubItem('MiniVol Flow', tabName='minivol', selected=FALSE),
             menuSubItem('Tank Spike', tabName='tankspike', icon=icon('arrow-up'))),
    # menuItem('Nerdmobile', href='http://air.utah.edu/s/nerdmobile/', newtab=F, icon=icon('car')),
    menuItem('Site Access', href='http://air.utah.edu/sites/', icon=icon('location-arrow')),
    br(),
    # menuItemOutput('ui_auth_menu')
    menuItem('Sign in', href='http://air.utah.edu/s/auth/', icon = icon('lock'), newtab=F)
  )
  
  # uiOutput('ui_signin_status')
)

# ---------------------------------------------------------------------------------
# Main panel layout, corresponding to sidebar tabs
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'homepage',
            tags$head(includeCSS('styles.css'),
                      tags$meta(name='apple-mobile-web-app-capable', content='yes'),
                      tags$meta(name='apple-mobile-web-app-status-bar-style', content='black'),
                      tags$meta(name='viewport', content='width=device-width, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no'),
                      HTML('<link rel="apple-touch-icon" href="touch-icon-iphone.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="76x76" href="touch-icon-ipad.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="120x120" href="touch-icon-iphone-retina.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="152x152" href="touch-icon-ipad-retina.png" />')),
            box(title='Utah Atmospheric Trace gas & Air Quality Lab', width=12, status='danger',
                HTML(hometext)
            )#,
            #             box(title='Calendar', width=12,
            #                 HTML('<iframe src="https://www.google.com/calendar/embed?showTitle=0&amp;showNav=0&amp;showPrint=0&amp;showCalendars=0&amp;height=600&amp;wkst=1&amp;bgcolor=%23ffffff&amp;src=melh5460nd0m3llv175h2ipuoc%40group.calendar.google.com&amp;color=%23B1440E&amp;ctz=America%2FDenver" style=" border-width:0 " width="800" height="600" frameborder="0" scrolling="no"></iframe>')
            #             )
    ),
    
    # ---------------------------------------------------------------------------------
    # WIDGETS
    tabItem(tabName = 'tankspike',
            box(title='Tank Spike Calculation', width=12, status='danger',
                p('Estimate the required spike pressure when filling tanks. Ambient concentration',
                  'is filled with most recent WBB observation within the last hour.'),
                hr(),
                textInput('tankspike_pressure', h5('Tank Pressure'), value=2250),
                uiOutput('tankspike_ambient_wbb'),
                textInput('tankspike_spike', h5('Spike')),
                textInput('tankspike_target', h5('Target')),
                h5('Calculated'),
                verbatimTextOutput('tankspike_calculated')
            )
    ),
    
    tabItem(tabName = 'dewtoppm',
            box(title='Dewpoint to Mole Fraction Converter', width=12, status='danger',
                p('Applies the August-Roche-Magnus approximation of the Clausius-Clapeyron ',
                  'equation to estimate the mole fraction of water vapor using dewpoint',
                  'temperature and ambient atmospheric pressure.'),
                withMathJax('$$e = 6.1094 \\exp{\\frac{17.623 \\cdot T_d}{T_d + 243.04}}$$'),
                hr(),
                h5('Td : Dewpoint temperature'),
                fluidRow(column(8, textInput('dewtoppm_Td', NULL)),
                         column(4, selectInput('dewtoppm_Td_unit', NULL,
                                               choices=c('C', 'F')))),
                h5('P  : Ambient pressure'),
                fluidRow(column(8, textInput('dewtoppm_P', NULL)),
                         column(4, selectInput('dewtoppm_P_unit', NULL,
                                               choices=c('hPa', 'kPa')))),
                h5('Calculated'),
                verbatimTextOutput('dewtoppm_calculated')
            )
    ),
    
    tabItem(tabName = 'cylvol',
            box(title='Cylindrical Volume Calculator', width=12, status='danger',
                p('Calculate the volume of a cylinder given the length and diameter.'),
                withMathJax('$$V = \\pi r^2 \\cdot L$$'),
                hr(),
                h5('D : Inner diameter of tubing'),
                fluidRow(column(8, textInput('cylvol_d', NULL)),
                         column(4, selectInput('cylvol_d_unit', NULL,
                                               choices=c('inches', 'feet', 'mm')))),
                h5('L : Length of tubing'),
                fluidRow(column(8, textInput('cylvol_l', NULL)),
                         column(4, selectInput('cylvol_l_unit', NULL,
                                               choices=c('inches', 'feet', 'm')))),
                h5('Calculated'),
                verbatimTextOutput('cylvol_calculated')
            )
    ),
    
    tabItem(tabName = 'tankdilution',
            box(title='Tank Dilution Calculator', width=12, status='danger',
                p('Calculate mole fraction given a tank of known concentration, a zero air dilution',
                  'and two flow rates.'),
                withMathJax('$$ppm = S \\frac{S_{flow}}{S_{flow} + D_{flow}}$$'),
                hr(),
                fluidRow(column(8, textInput('dilution_sample', h5('Sample (ppm)'))),
                         column(4, textInput('dilution_sample_flow', h5('Flow (slpm)')))),
                fluidRow(column(8, textInput('show_zero', h5('Dilution (ppm)'))),
                         column(4, textInput('dilution_dilution_flow', h5('Flow (slpm)')))),
                h5('Calculated'),
                verbatimTextOutput('tankdilution_calculated')
            )
    ),
    
    
    tabItem(tabName = 'minivol',
            box(title='MiniVol Rotameter Setpoint Calculator', width=12,
#                 p('Applies the August-Roche-Magnus approximation of the Clausius-Clapeyron ',
#                   'equation to estimate the mole fraction of water vapor using dewpoint',
#                   'temperature and ambient atmospheric pressure.'),
#                 withMathJax('$$e = 6.1094 \\exp{\\frac{17.623 \\cdot T_d}{T_d + 243.04}}$$'),
#                 hr(),
                h5('T : Average temperature'),
                fluidRow(column(8, textInput('minivol_Tavg', NULL)),
                         column(4, selectInput('minivol_Tavg_unit', NULL,
                                               choices=c('C', 'F')))),
                h5('P : Average ambient pressure'),
                fluidRow(column(8, textInput('minivol_P', NULL)),
                         column(4, selectInput('minivol_P_unit', NULL,
                                               choices=c('hPa', 'inHg', 'mmHg')))),
                h5('Calculated'),
                verbatimTextOutput('minivol_calculated')
            )
    ),
    
    # ---------------------------------------------------------------------------------
    # USER LOGIN
    tabItem(tabName = 'signin',
            fluidRow(
              column(width=9,
                     uiOutput('ui_signin'),
                     uiOutput('ui_signin_passfail'),
                     uiOutput('ui_changepass')
              )
            )
    ),
    
    
    # ---------------------------------------------------------------------------------
    # FIELD NOTES
    tabItem(tabName = 'fieldnotes',
            fluidRow(
              column(width=8,
                     tabBox(id='note_network', width=NULL,
                            tabPanel('CO2 Network'),
                            tabPanel('Uinta Basin'),
                            tabPanel('Other'),
                            selectInput('note_site', label=h5('Choose location'), choices=c(''))
                     ),
                     box(title='Site checklist', solidHeader=TRUE, width=NULL,
                         checkboxGroupInput('null1', label=NULL,
                                            choices=c('Reference cell (N2) flow (100mL/min)',
                                                      'Sample cell flow (400mL/min)',
                                                      'Chemicals',
                                                      'All relays on Auto',
                                                      'Restart CR1000 (use this as Time out)',
                                                      'Input reference gas values in Public table')),
                         fluidRow(
                           column(6, textInput('note_time_in', h5('Time in'), 
                                               value=format(Sys.time()-60*30, '%Y-%m-%d %H:%M %Z', tz='UTC'))),
                           column(6, textInput('note_time_out', h5('Time out'), 
                                               value=format(Sys.time()+60*30, '%Y-%m-%d %H:%M %Z', tz='UTC')))
                         ),
                         uiOutput('note_nitrogen_ui'),
                         h5('Notes'),
                         tags$style(type='text/css', 'textarea {width:100%; max-width:100%}'),
                         tags$textarea(id='note_text', rows=7),
                         fluidRow(
                           column(3, actionButton('note_save', 'Submit log', icon=icon('pencil-square-o'))),
                           column(9, h5('Update reference tank pressures and locations.'))
                         )
                     ),
                     uiOutput('ui_note_passfail')
              ),
              infoBoxOutput('site_last_updated', width=4),
              infoBoxOutput('site_tank_status', width=4),
              box(title='Current tanks', solidHeader=T, width=4,
                  plotOutput('ts_note', height=222)
              )
            ),
            fluidRow(
              column(width=12,
                     uiOutput('note_tank_update'),
                     box(title='Site notes (displayed in local time)', solidHeader=T, width=NULL,
                         DT::dataTableOutput('dt_note')
                     )
              )
            )
            
    ),
    
    
    
    # ---------------------------------------------------------------------------------
    # TANK MANAGEMENT
    tabItem(tabName = 'tanktracker',
            fluidRow(
              column(width=12,
                     
                     tabBox(id='tank_function', width=NULL, title='Tank Master List',
                            tabPanel('View', uiOutput('tank_view_ui')),
                            
                            tabPanel('Add',  uiOutput('tank_add_ui')),
                            
                            tabPanel('Edit', uiOutput('tank_edit_ui')),
                            
                            tabPanel('Remove',
                                     uiOutput('tank_remove_tank_ui'),
                                     uiOutput('tank_remove_time_ui')
                            )
                     ),
                     
                     box(width=NULL, DT::dataTableOutput('dt_tank'))
                     
              )
            )
    ),
    
    # ---------------------------------------------------------------------------------
    # REQUEST TANK
    tabItem(tabName='request_tank',
            fluidRow(
              column(width=12,
                     box(title=NULL, status='danger', width=NULL,
                         h3('Filling of compressed gas cylinders'),
                         HTML(tanktext),
                         br(),
                         h3('Compressed cylinder fill request'),
                         fluidRow(
                           column(width=4, 
                                  textInput('request_name', h5('Name')),
                                  textInput('request_email', h5('Email'))),
                           column(width=4, textInput('request_date', h5('Date'), value=Sys.Date()))
                         ),
                         fluidRow(
                           column(width=4,
                                  numericInput('request_qty_1', h5('Quantity'), min=0, max=5, value=1, step=1),
                                  numericInput('request_qty_2', NULL, min=1, max=5, value=0, step=1),
                                  numericInput('request_qty_3', NULL, min=1, max=5, value=0, step=1)),
                           column(width=4,
                                  textInput('request_co2_1', h5('CO2 (ppm)')),
                                  textInput('request_co2_2', NULL),
                                  textInput('request_co2_3', NULL)),
                           column(width=4,
                                  textInput('request_ch4_1', h5('CH4 (ppm)')),
                                  textInput('request_ch4_2', NULL),
                                  textInput('request_ch4_3', NULL))
                         ),
                         h5('Notes'),
                         tags$style(type='text/css', 'textarea {width:100%; max-width:100%}'),
                         tags$textarea(id='request_comment', rows=7),
                         actionButton('request_save', 'Submit request', icon=icon('paper-plane'))
                     ),
                     uiOutput('ui_request_passfail')
              )
            )
    ),
    
    # ---------------------------------------------------------------------------------
    # DOWNLOAD TANK DATA
    tabItem(tabName = 'download',
            box(title='Download data', width=12,
                selectInput('download_type', h5('File structure'), width=300,
                            c('Serialized R dataset (.rds)', 'Tab separated (.dat)', 'Matlab structure (.mat)')),
                downloadButton('download_trigger')
            )
    )
  )
)

# ---------------------------------------------------------------------------------
dashboardPage(title='Technical Utilities', 
              skin='black',
              header,
              sidebar,
              body
)
