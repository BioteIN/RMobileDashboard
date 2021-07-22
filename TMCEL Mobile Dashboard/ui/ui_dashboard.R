#####################################################
### ui_dashboard.R
#####################################################



header <- dashboardHeader(title = "Basic Dashboard")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
                 href = "https://www.tmcel.mz")
    )
)


# body

frow1 <- fluidRow(
    valueBoxOutput("value1", width = 4)
    ,valueBoxOutput("value2", width = 4)
    ,valueBoxOutput("value3", width = 4)
)


frow2 <- fluidRow( 
    box(width = 6,
        title = "Revenue per Account"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("plot_out_daily_topup", height = "300px")
    )
    ,box(width = 6,
         title = "Revenue per Product"
         ,status = "primary"
         ,solidHeader = TRUE 
         ,collapsible = TRUE 
         ,plotOutput("revenuebyRegion", height = "300px")
    ) 
)


# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)