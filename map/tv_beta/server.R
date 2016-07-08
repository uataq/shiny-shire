# Ben Fasoli
source('global.R')

function(input, output, session) {
  
  # Get data -------------------------------------------------------------------
  df <- reactiveFileReader(30000, session, 
                           '/srv/shiny-server/map/shared/airmap.rds',
                           readRDS)
  
  df_wbb <- reactiveFileReader(30000, session, 
                               '/srv/shiny-server/map/shared/airmap.rds',
                               function(x) {
                                 readRDS(x)$fixed %>%
                                   filter(grepl('wbb', site))
                                 })
  
  df_wbb_ts <- dir('/projects/data/wbb/parsed',
                    full.names=T) %>%
    tail(1) %>%
    reactiveFileReader(intervalMillis=1800000, session=session, function(x) {
      df <- readr::read_csv(x)
      if (nrow(df) < 10) return()
      df %>%
        filter(ID_co2 == -10,
               Time_UTC > as.POSIXct(Sys.Date() - 5))
      })
  
  # Value boxes ----------------------------------------------------------------
  output$wbb_co2 <- renderValueBox({
    value <- df_wbb()$CO2d_ppm
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'black'
    } else {
      if (value < 430) { color <- 'green'
      } else if (value >= 430 && value < 460) { color <- 'yellow'
      } else if (value >= 460 && value < 520) { color <- 'orange'
      } else if (value >= 520) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(paste(value, 'ppm'), HTML('CO<sub>2</sub> concentration'), 
             icon('cloud'), color)
  })
  
  output$wbb_ch4 <- renderValueBox({
    value <- df_wbb()$CH4d_ppm
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'black'
    } else {
      if (value < 2.0) { color <- 'green'
      } else if (value >= 2.0 && value < 2.3) { color <- 'yellow'
      } else if (value >= 2.3 && value < 2.5) { color <- 'orange'
      } else if (value >= 2.5) { color <- 'red'
      }
      value <- round(value, 2)
    }
    print(value)
    print(color)
    valueBox(paste(value, 'ppm'), HTML('CH<sub>4</sub> concentration'), 
             icon('industry'), color)
  })
  
  output$wbb_o3 <- renderValueBox({
    value <- df_wbb()$O3_ppbv
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'red'
    } else {
      if (value < 20) { color <- 'green'
      } else if (value >= 20 && value < 40) { color <- 'yellow'
      } else if (value >= 40 && value < 70) { color <- 'orange'
      } else if (value >= 70) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(paste(value, 'ppb'), HTML('O<sub>3</sub> concentration'), 
             icon('sun-o'), color)
  })
  
  output$wbb_pm25 <- renderValueBox({
    value <- df_wbb()$PM25_ugm3
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'red'
    } else {
      if (value < 15) { color <- 'green'
      } else if (value >= 15 && value < 30) { color <- 'yellow'
      } else if (value >= 30 && value < 45) { color <- 'orange'
      } else if (value >= 45) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(paste(value, 'ug m3'), HTML('PM<sub>2.5</sub> concentration'), 
             icon('car'), color)
  })
  
  # Produce timeseries ---------------------------------------------------------
  # output$ts_co2 <- renderPlot({
  #     d <- df_wbb_ts()
  #     
  #     minmax <- c(400, 550)
  #     d$color <- d$CO2d_ppm
  #     d$color[d$color < minmax[1]] <- minmax[1]
  #     d$color[d$color > minmax[2]] <- minmax[2]
  #     
  #     ggplot(data=d, aes(x=Time_UTC, y=CO2d_ppm, color=color)) +
  #       geom_point(alpha=0.3) +
  #       scale_color_gradientn(colors=c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
  #                             guide=F) +
  #       # stat_smooth(method='lm', formula=y~poly(x, 10), se=F, color='black') +
  #       geom_line(aes(y=run_smooth(d$CO2d_ppm, n=360)), color='black', na.rm=T,
  #                 size=1.2, alpha=0.9) +
  #       scale_x_datetime(labels=NULL, breaks=NULL) +
  #       xlab(NULL) +
  #       ylab(expression(CO[2] ~ ' (ppm)')) +
  #       theme_classic() +
  #       theme(legend.position='bottom', legend.title=element_blank())
  # })
  # 
  # output$ts_ch4 <- renderPlot({
  #   d <- df_wbb_ts()
  #   
  #   minmax <- c(1.9, 2.5)
  #   d$color <- d$CH4d_ppm
  #   d$color[d$color < minmax[1]] <- minmax[1]
  #   d$color[d$color > minmax[2]] <- minmax[2]
  #   
  #   ggplot(data=d, aes(x=Time_UTC, y=CH4d_ppm, color=color)) +
  #     geom_point(alpha=0.3) +
  #     scale_color_gradientn(colors=c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
  #                           guide=F) +
  #     geom_line(aes(y=run_smooth(d$CH4d_ppm, n=360)), color='black', na.rm=T,
  #               size=1.2, alpha=0.9) +
  #     scale_x_datetime(date_breaks='1 day', date_labels='%a') +
  #     xlab('Time (UTC)') +
  #     ylab(expression(CH[4] ~ ' (ppm)')) +
  #     theme_classic() +
  #     theme(legend.position='bottom', legend.title=element_blank())
  # })

  # Map tabs use shiny modules to define the map UI
  for (tracer in opts$short)
    callModule(mod_map_srv, tracer, tab=input$tab)
}