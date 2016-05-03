# Default to signin sidebar tab
updateTabItems(session, 'sidebar_active', 'signin')
# Default login window open
observe({
  input$open_login_window
  if (!is.null(input$sidebar_active) && input$sidebar_active == 'signin') {
    toggleModal(session, 'login_window', 'open')
  } 
})
auth <- reactiveValues(logged = F,
                       name   = NA,
                       pass   = NA,
                       level  = NA)
# auth <- reactiveValues(logged = T,
#                        name   = 'benfasoli',
#                        pass   = 'any',
#                        level  = 'a')

# Dynamic sidebar options ----------------------------------------------------
output$sidebar_auth <- renderMenu({
  # 'b' for basic technician
  # 'a' for admin technician
  # 'd' for uataq data access
  # 's' for site access information
  menuitems <- list()
  if (!auth$logged){
    menuitems <- c(menuitems, list(
      menuItem('Sign in', tabName='signin', icon = icon('lock'), selected=T)))
  } else {
    if (grepl('b', auth$level)) {
      # Basic technician level, rw field form and r tank tracker
      menuitems <- c(menuitems, list(
        menuItem('Field Form', icon=icon('bookmark'), tabName='field_form'),
        menuItem('Tank Management', icon=icon('dashboard'), tabName = 'tank_tracker'),
        menuItem('Request Tank', icon=icon('envelope'), tabName='request_tank')))
    } else if (grepl('a', auth$level)) {
      # Admin technician level, rw field form and rw tank tracker
      menuitems <- c(menuitems, list(
        menuItem('Field Form', icon=icon('bookmark'), tabName='field_form'),
        br(),
        menuItem('Tank Management', icon=icon('dashboard'), tabName='tank_tracker', selected=T),
        menuItem('Request Tank', icon=icon('envelope'), tabName='request_tank'),
        menuItem('Download Tank Data', icon=icon('download'), tabName='tank_download')))
    }
    if (grepl('d', auth$level)) {
      menuitems <- c(menuitems, list(
        menuItem('Download UATAQ Measurements', icon=icon('download'),
                 tabName='data_download')))
    }
    # if (grepl('s', auth$level)) {
    #   menuitems <- c(menuitems, list(
    #     menuItem('Site Access', icon=icon('location-arrow'),
    #              tabName='site_access')))
    # }
    
    menuitems <- c(menuitems, list(
      br(), br(),
      sidebarUserPanel(span('Logged in as', strong(auth$name)),
                       subtitle = div(
                         actionLink('open_changepass_window', 'Change password', icon=icon('key')),
                         br(),
                         actionLink('signout', 'Sign out', icon=icon('sign-out'))))))
  }
  sidebarMenu(.list=menuitems)
})

# Validate login credentials -------------------------------------------------
observeEvent(input$login_button, {
  valid <- readr::read_csv(tail(dir('src/user_auth', full.names=T), 1))
  username <- input$username
  userpass <- input$userpass
  idx <- match(username, valid$user)
  
  if(!is.na(idx) && userpass==valid$key[idx]) {
    auth$logged <- T
    auth$name   <- username
    auth$pass   <- userpass
    auth$level  <- valid$level[idx]
    toggleModal(session, 'login_window', 'close')
    if (grepl('a', auth$level)) init_tab <- 'tank_tracker'
    else if (grepl('b', auth$level)) init_tab <- 'field_form'
    else if (grepl('d', auth$level)) init_tab <- 'data_download'
    else return()
    updateTabItems(session, 'sidebar_active', init_tab)
  } else {
    closeAlert(session, 'a1')
    createAlert(session, 'login_alert', 'a1', style='danger',
                content='Login error. Check credentials or email Ben Fasoli.')
  }
})

# Change password event ------------------------------------------------------
observeEvent(input$open_changepass_window, toggleModal(session, 'changepass_window', 'open'))
observeEvent(input$changepass_submit, {
  if(auth$logged && !is.null(input$changepass_submit) && input$changepass_submit > 0){
    oldpass  <- isolate(input$oldpass)
    newpass1 <- isolate(input$newpass1)
    newpass2 <- isolate(input$newpass2)
    
    if(newpass1 == newpass2 && oldpass==auth$pass){
      valid <- readr::read_csv(tail(dir('src/user_auth', full.names=T), 1))
      mask <- valid$user == auth$name
      newuser <- data_frame(user=auth$name,
                            key=newpass1,
                            level=auth$level)
      oldusers <- valid[!mask, ]
      newauth <- bind_rows(oldusers, newuser)
      fname <- format(Sys.time(), 'src/user_auth/%y%m%d_%H%M_auth.dat')
      readr::write_csv(newauth, fname)
      Sys.chmod(fname, mode='0600')
      
      closeAlert(session, 'a2')
      createAlert(session, 'changepass_alert', 'a2', style='success',
                  content='Success! Password successfully changed.')
    } else {
      closeAlert(session, 'a2')
      createAlert(session, 'changepass_alert', 'a2', style='danger',
                  content='Password change failed. Did you enter the correct password?')
    }
  }
})

# Log out event --------------------------------------------------------------
observeEvent(input$signout, session$reload())

