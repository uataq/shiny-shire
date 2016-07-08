# ---------------------------------------------------------------------------------
# SEND REQUEST EVENT
output$ui_request_passfail <- renderUI({
  if(!is.null(input$request_save)  && input$request_save > 0){
    name <- isolate(input$request_name)
    date <- isolate(input$request_date)
    email <- isolate(input$request_email)
    target <- isolate(cbind(c(input$request_qty_1, input$request_qty_2, input$request_qty_3),
                            c(input$request_co2_1, input$request_co2_2, input$request_co2_3),
                            c(input$request_ch4_1, input$request_ch4_2, input$request_ch4_3)))
    
    text <- isolate(input$request_comment)
    msg <- paste(sep='\n',
                 paste('Name:\t\t', name),
                 paste('Date:\t\t', date),
                 paste('Email:\t\t', email),
                 paste('Comments:\t', text),
                 paste('\nRequest:'),
                 paste(input$request_qty_1, input$request_co2_1, input$request_ch4_1, sep='\t'),
                 paste(input$request_qty_2, input$request_co2_2, input$request_ch4_2, sep='\t'),
                 paste(input$request_qty_3, input$request_co2_3, input$request_ch4_3, sep='\t')
    )
    
    sendmail_options(smtpServer='ASPMX.L.GOOGLE.COM')
    from <- '<donotreply@air.utah.edu>'
    to <- '<tankfillrequest@gmail.com>'
    subject <- paste('Tank Request:', Sys.Date())
    sendmail(from, to, subject, msg)
    
    box(title='Success!', status='success', width=NULL,
        p('Tank request has been sent. You will be contacted via email regarding the status of the tank.')
    )
  }
})
