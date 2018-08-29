# ---------------------------------------------------------------------------------
# NEWS ITEMS
# output$newsMenu <- renderMenu({
#   if(USER$logged){
#     dropdownMenu(type = 'notifications',
#                  notificationItem(
#                    text = 'Tanks rolled back to 9/28',
#                    icon('exclamation-triangle'),
#                    status='danger'
#                  )
#     )
#   }
# })

# ---------------------------------------------------------------------------------
# TANK REQUESTS
# output$tankMenu <- renderMenu({
#   if(USER$logged && USER$level=='admin'){
#     dropdownMenu(type = 'tasks',
#                  notificationItem(
#                    text = 'Tank request: 2x 400 ppm',
#                    icon('dashboard'),
#                    status='success'
#                  )
#     )
#   }
# })