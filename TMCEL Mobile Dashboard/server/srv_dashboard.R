##############################################################
### srv_dashboard.R
##############################################################

output$value1 <- renderValueBox({
    valueBox(value = prettyNum(topup_yearly_tbl$total_amount, big.mark = ","), 
             subtitle = "Valor Total"
             # formatC(sales.account$value, format="d", big.mark=',')
             # ,paste('Top Account:',sales.account$Account)
             ,icon = icon("stats",lib='glyphicon')
             ,color = "purple")
})



output$value2 <- renderValueBox({
    valueBox(value = scales::comma(200000, big.mark = ","), 
             subtitle = "Compra Recarga"
             # formatC(sales.account$value, format="d", big.mark=',')
             # ,paste('Top Account:',sales.account$Account)
             ,icon = icon("stats",lib='glyphicon')
             ,color = "yellow")  
})

output$value3 <- renderValueBox({
    valueBox(value = scales::comma(100000,big.mark = ",") , subtitle = "Total Expected Revenue"
             # formatC(sales.account$value, format="d", big.mark=',')
             # ,paste('Top Account:',sales.account$Account)
             ,icon = icon("stats",lib='glyphicon')
             ,color = "green")  
})


output$plot_out_daily_topup <- renderPlot({
    
    ggplot(data = topup_daily_tbl, aes(x = CREATED_AT, y = total_amount)) +
        geom_line()
})