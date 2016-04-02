# ---------------------------------------------------------------------------------
# AUTH INITIALIZATION
AUTH <<- read.csv(tail(dir('user_auth', pattern='.*.{1}dat', full.names=T), 1), stringsAsFactors=F)
USER <- reactiveValues(logged = FALSE,
                       name   = NA,
                       pass   = NA,
                       level  = NA)

# ---------------------------------------------------------------------------------
# SIGN IN UI
output$ui_signin <- renderUI({
  input$signout
  if(!USER$logged){
    box(title='Sign in', status='danger', width=NULL,
        p('These are restricted technical utilities for analysts associated with the Air Quality',
          ' & Trace Gas Lab at the University of Utah. For access, email', 
          HTML('<a href=mailto:b.fasoli@utah.edu>Ben Fasoli</a>'), '.'),
        textInput('username', 'Username:', width=300),
        passwordInput('userpass', 'Password:', width=300),
        br(),
        actionButton('signin', 'Sign in', icon=icon('sign-in'))
    )
  } else{
    box(title='Success!', status='success', width=NULL,
        p('Signed in as :  ', strong(USER$name)),
        p('Your privilege level :  ', strong(USER$level)),
        hr(),
        p('You now have access to additional utilities in the sidebar.'),
        hr(),
        h3('Change password'),
        passwordInput('oldpass', 'Old password', width=300),
        passwordInput('newpass1', 'New password', width=300),
        passwordInput('newpass2', 'Repeat new password', width=300),
        actionButton('changepass', 'Apply')
    )
  }
})

# ---------------------------------------------------------------------------------
# SIGN IN EVENT AND ERROR MESSAGE
output$ui_signin_passfail <- renderUI({
  if(!is.null(input$signin) && input$signin > 0){
    username <- isolate(input$username)
    userpass <- isolate(input$userpass)
    idx <- match(username, AUTH$user)
    
    if(!is.na(idx) && userpass==AUTH$key[idx]) {
      USER$logged <- TRUE
      USER$name   <- username
      USER$pass   <- userpass
      USER$level  <- AUTH$level[idx]
      ''
    } else {
      box(title='Wrong username or password.', status='danger', width=NULL,
          p('Incorrect password attempt. Try again.')
      )
    }
  }
})

# ---------------------------------------------------------------------------------
# SIDEBAR LOGGED IN STATUS
output$ui_signin_status <- renderUI({
  if(USER$logged){
    tagList(
      br(), br(),
      sidebarUserPanel(span('Logged in as', strong(USER$name)), 
                       subtitle = actionLink('signout', 'Sign out', icon=icon('sign-out')))
    )
  }
})

# ---------------------------------------------------------------------------------
# PASSWORD CHANGE EVENT AND ERROR
output$ui_changepass <- renderUI({
  if(USER$logged && !is.null(input$changepass) && input$changepass > 0){
    username <- USER$name
    oldpass <- isolate(input$oldpass)
    newpass1 <- isolate(input$newpass1)
    newpass2 <- isolate(input$newpass2)
    d <- list(username, oldpass, newpass1, newpass2, USER)
    saveRDS(d, '~/test.rds')
    if(newpass1 == newpass2 && oldpass==USER$pass){
      USER <- read.csv(tail(dir('user_auth', pattern='.*.{1}dat', full.names=T), 1), stringsAsFactors=F)
      mask <- USER$user == username
      newuser <- data.frame(stringsAsFactors=F,
                            user=username,
                            key=newpass1,
                            level=USER$level[mask])
      oldusers <- USER[!mask, ]
      newauth <- rbind.fill(oldusers, newuser)
      fname <- format(Sys.time(), 'user_auth/%y%m%d_%H%M_auth.dat')
      write.csv(newauth, file=fname, 
                quote=F, row.names=F)
      Sys.chmod(fname, mode='0600')
      
      AUTH <<- read.csv(tail(dir('user_auth', pattern='.*.{1}dat', full.names=T), 1), stringsAsFactors=F)
      
      updateTextInput(session, 'username', value='')
      updateTextInput(session, 'userpass', value='')
      
      box(title='Success!', status='success', width=NULL,
          p('Password successfully changed.')
      )
    } else {
      box(title='Error', status='danger', width=NULL,
          p('Password change failed. Did you enter the correct password?')
      )
    }
  }
})

# ---------------------------------------------------------------------------------
# SIGN OUT EVENT
observeEvent(input$signout,{
  updateTextInput(session, 'username', value='')
  updateTextInput(session, 'userpass', value='')
  USER$logged <- FALSE
  USER$name   <- NA
  USER$pass   <- NA
  USER$level  <- NA
})

# ---------------------------------------------------------------------------------
# SIDEBAR PERMISSIONS
output$ui_auth_menu <- renderMenu({
  if(!USER$logged){
    sidebarMenu(
      menuItem('Sign in', tabName = 'signin', icon = icon('lock'))
    )
  } else if(USER$level == 'user'){
    sidebarMenu(
      menuItem('Field Form', icon=icon('bookmark'), tabName='fieldnotes'),
      menuItem('Tank Management', icon=icon('dashboard'), tabName = 'tanktracker'),
      menuItem('Request Tank', icon=icon('envelope'), tabName='request_tank')
    )
  } else if(USER$level == 'admin'){
    sidebarMenu(
      menuItem('Field Form', icon=icon('bookmark'), tabName='fieldnotes'),
      br(),
      menuItem('Tank Management', icon=icon('dashboard'), tabName='tanktracker'),
      menuItem('Request Tank', icon=icon('envelope'), tabName='request_tank'),
      menuItem('Download Tank Data', icon=icon('download'), tabName='download')
    )
  } else if(USER$level == 'master'){
    sidebarMenu(
      menuItem('Request Tank', icon=icon('envelope'), tabName='request_tank')
    )
  }
})
