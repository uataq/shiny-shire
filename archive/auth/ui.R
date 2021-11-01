# Ben Fasoli

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(a(img(src='utelogo.png', height=19), 'ATAQ Lab',
                                      href='http://air.utah.edu'),
                                    class='uataq-head'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar( 
  sidebarMenu(id='sidebar_active',
              menuItemOutput('sidebar_auth'))
)


# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('www/styles.css'),
            tags$script(src = 'login_button.js'),
            tags$script(src = 'md5.js'),
            tags$script(src = 'passwdInputBinding.js'),
            HTML('<link rel="stylesheet" ',
                 'href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css">')),
  
  # Login window ---------------------------------------------------------------
  bsModal('login_window', 'Sign in', 'signin',
          p('These are restricted technical utilities for analysts associated with the Air Quality',
            ' & Trace Gas Lab at the University of Utah. For access, email', 
            HTML('<a href=mailto:b.fasoli@utah.edu>Ben Fasoli</a>'), '.'),
          # textInput('username', 'Username:', width=300),
          # passwordInput('userpass', 'Password:', width=300),
          HTML('<div class="input-group">',
               '<span class="input-group-addon"><i class="fa fa-user fa-fw"></i></span>',
               '<input class="form-control shiny-bound-input" id="username" type="text" placeholder="Username">',
               '</div>',
               '<div style="line-height:50%;"><br></div>',
               '<div class="input-group">',
               '<span class="input-group-addon"><i class="fa fa-key fa-fw"></i></span>',
               '<input class="form-control shiny-bound-input" id="userpass" type="password" placeholder="Password">',
               '</div>'),
          br(),
          actionButton('login_button', 'Sign in', icon=icon('thumbs-up')),
          bsAlert('login_alert')
  ),
  
  # Password change window -----------------------------------------------------
  bsModal('changepass_window', 'Change password', 'open_changepass_window',
          passwordInput('oldpass', 'Old password', width=300),
          passwordInput('newpass1', 'New password', width=300),
          passwordInput('newpass2', 'Repeat new password', width=300),
          actionButton('changepass_submit', 'Apply', icon=icon('hdd-o')),
          bsAlert('changepass_alert')
  ),
  
  # Site access information window ---------------------------------------------
  bsModal('siteaccess_window', 'UATAQ Site Information', 'open_siteaccess_window', size='large',
          tags$iframe(src='site_access.html',
                      onload="this.style.height=this.contentDocument.body.scrollHeight +'px';")
  ),
  
  tabItems(
    # Initial sign in tab ------------------------------------------------------
    tabItem('signin', status='danger',
            span('These utilities are restricted.', 
                 actionLink('open_login_window', 'Log in'),
                 'or contact', a('Ben Fasoli', href='mailto:b.fasoli@utah.edu'),
                 'for access.')),
    
    # Site diagnostics ---------------------------------------------------------
    # tabItem('site_diagnostics', 
    #         fluidRow(
    #           column(12,
    #                  tags$iframe(src='http://air.utah.edu/dash/',
    #                  style = 'height: 100%;'))
    #           )
    # ),
    
    # Field Form ---------------------------------------------------------------
    tabItem('field_form',
            fluidRow(
              column(width=8,
                     tabBox(id='note_network', width=NULL,
                            tabPanel('Sites',
                                     selectInput('note_site', label=h5('Choose location'), 
                                                 choices=loc_opts, selected='fru'),
                                     actionButton('open_siteaccess_window', 'Site access', icon=icon('unlock'))),
                            tabPanel('Uinta Tank Pressures',
                                     a(href='http://home.chpc.utah.edu/~bfasoli/uinta_tanks/fru.jpg', target='_blank',
                                       img(src='http://home.chpc.utah.edu/~bfasoli/uinta_tanks/fru.jpg')),
                                     a(href='http://home.chpc.utah.edu/~bfasoli/uinta_tanks/roo.jpg', target='_blank',
                                       img(src='http://home.chpc.utah.edu/~bfasoli/uinta_tanks/roo.jpg')),
                                     a(href='http://home.chpc.utah.edu/~bfasoli/uinta_tanks/hpl.jpg', target='_blank',
                                       img(src='http://home.chpc.utah.edu/~bfasoli/uinta_tanks/hpl.jpg'))),
                            tabPanel('Onsite Checklist',
                                     checkboxGroupInput('null1', label=NULL,
                                                        choices=c('Reference cell (N2) flow (100mL/min)',
                                                                  'Sample cell flow (400mL/min)',
                                                                  'Chemicals',
                                                                  'All relays on Auto',
                                                                  'Restart CR1000 (use this as Time out)',
                                                                  'Input reference gas values in Public table')))),
                     box(title='Submit Notes', solidHeader=TRUE, width=NULL,
                         fluidRow(
                           column(6, textInput('note_time_in', h5('Time in'), 
                                               value=format(Sys.time()-60*30, '%Y-%m-%d %H:%M %Z', tz='UTC'))),
                           column(6, textInput('note_time_out', h5('Time out'), 
                                               value=format(Sys.time()+60*30, '%Y-%m-%d %H:%M %Z', tz='UTC')))),
                         uiOutput('note_nitrogen_ui'),
                         h5('Notes'),
                         tags$style(type='text/css',
                                    'textarea {width:100%; max-width:100%}'),
                         tags$textarea(id='note_text', rows=7),
                         fluidRow(
                           column(3, actionButton('note_save', 'Submit log',
                                                  icon=icon('pencil-square-o'))),
                           column(9, bsAlert('ffsave_alert')))),
                     uiOutput('ui_note_passfail')),
              uiOutput('site_status'),
              box(title='Current tanks', solidHeader=T, width=4,
                  plotOutput('ts_note', height=222))),
            fluidRow(
              column(width=12,
                     uiOutput('note_tank_update_ui'),
                     box(title='Site notes (displayed in local time)',
                         solidHeader=T, width=NULL,
                         DT::dataTableOutput('dt_note'))))),
    
    # Tank Tracker -------------------------------------------------------------
    tabItem(tabName = 'tank_tracker',
            fluidRow(
              column(width=12,
                     tabBox(id='tank_function', width=NULL,
                            title='Tank Master List',
                            tabPanel('View', uiOutput('tank_view_ui')),
                            tabPanel('Add',  uiOutput('tank_add_ui')),
                            tabPanel('Rename', uiOutput('tank_rename_ui')),
                            tabPanel('Edit', uiOutput('tank_edit_ui')),
                            tabPanel('Remove',
                                     uiOutput('tank_remove_tank_ui'),
                                     uiOutput('tank_remove_time_ui'))),
                     box(width=NULL, DT::dataTableOutput('dt_tank'))))),
    
    # Request tank fill --------------------------------------------------------
    tabItem(tabName='request_tank',
            fluidRow(
              column(width=12,
                     box(title=NULL, status='danger', width=NULL,
                         h3('Filling of compressed gas cylinders'),
                         # HTML(tanktext),
                         br(),
                         h3('Compressed cylinder fill request'),
                         fluidRow(
                           column(width=4, 
                                  textInput('request_name', h5('Name')),
                                  textInput('request_email', h5('Email'))),
                           column(width=4,
                                  textInput('request_date',h5('Date'),
                                            value=Sys.Date()))),
                         fluidRow(
                           column(width=4,
                                  numericInput('request_qty_1', h5('Quantity'),
                                               min=0, max=5, value=1, step=1),
                                  numericInput('request_qty_2', NULL, min=1,
                                               max=5, value=0, step=1),
                                  numericInput('request_qty_3', NULL, min=1,
                                               max=5, value=0, step=1)),
                           column(width=4,
                                  textInput('request_co2_1', h5('CO2 (ppm)')),
                                  textInput('request_co2_2', NULL),
                                  textInput('request_co2_3', NULL)),
                           column(width=4,
                                  textInput('request_ch4_1', h5('CH4 (ppm)')),
                                  textInput('request_ch4_2', NULL),
                                  textInput('request_ch4_3', NULL))),
                         h5('Notes'),
                         tags$style(type='text/css',
                                    'textarea {width:100%; max-width:100%}'),
                         tags$textarea(id='request_comment', rows=7),
                         actionButton('request_save', 'Submit request',
                                      icon=icon('paper-plane'))),
                     bsAlert('tankrequest_alert')))),
    
    
    
    # Download tank database ---------------------------------------------------
    tabItem(tabName = 'tank_download',
            box(title='Copy tank database', status='danger', width=12,
                selectInput('download_type', h5('File structure'), width=300,
                            c('Serialized R dataset (.rds)',
                              'Tab separated (.dat)',
                              'Matlab structure (.mat)')),
                downloadButton('download_trigger'))),
    
    # Download AUTAQ data ------------------------------------------------------
    tabItem(tabName = 'data_download',
            box(title='Download UATAQ data', status='danger', width=12,
                div('Visit', a('air.utah.edu', href='http://air.utah.edu'),
                    'for historic measurements and data visualizations.')))
    
    
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Authorized', skin='black',
              header, sidebar, body)