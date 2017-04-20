function(input, output, session) {
  observeEvent(input$btn, {
    showNotification("This is a notification.")
    showModal(
      modalDialog(
        size = 'l',
        title = 'Welcome!',
        'Message...',
        footer = NULL
      )
    )
  })
}