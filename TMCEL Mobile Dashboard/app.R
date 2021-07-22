library(shiny)
library(shinyMobile)
library(apexcharter)
library(shinydashboard)
library(shinyWidgets)
library(bs4Dash)
library(tidyverse)
library(shinycssloaders)
library(highcharter)


source(file = "global.R", local = TRUE)
source(file = "ui/ui_dashboard.R", local = TRUE)

poll <- data.frame(
    answer = c("Yes", "No"),
    n = c(254, 238)
)

shinyApp(
    ui = f7Page(
        title = "Mobile Dashboard",
        f7TabLayout(
            panels = tagList(
                f7Panel(title = "Mobile Dashboard", side = "left", theme = "light", "TMCEL Mobile Dashboard", effect = "cover", id="myPanel"),
                f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
            ),
            navbar = f7Navbar(
                title = "Mobile Dashboard",
                hairline = TRUE,
                shadow = TRUE,
                leftPanel = TRUE,
                rightPanel = FALSE
            ),
            f7Tabs(
                animated = TRUE,
                #swipeable = TRUE,
                f7Tab(
                    tabName = "Recargas",
                    icon = f7Icon("folder"),
                    active = TRUE,
                    
                    
                    
                    tags$head(
                        tags$script(
                            'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
                var os = message.os;
                var skin = message.skin;
                if (os === "md") {
                  $("html").addClass("md");
                  $("html").removeClass("ios");
                  $(".tab-link-highlight").show();
                } else if (os === "ios") {
                  $("html").addClass("ios");
                  $("html").removeClass("md");
                  $(".tab-link-highlight").hide();
                }

                if (skin === "dark") {
                 $("html").addClass("theme-dark");
                } else {
                  $("html").removeClass("theme-dark");
                }

               });
              '
                        )
                    ),
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        
                        f7Card(
                            title = NULL,
                            tagList(
                                f7Flex(
                                    prettyRadioButtons(
                                        inputId = "theme",
                                        label = "Selecione o tema:",
                                        thick = TRUE,
                                        inline = TRUE,
                                        selected = "md",
                                        choices = c("ios", "md"),
                                        animation = "pulse",
                                        status = "info"
                                    ),
                                    
                                    prettyRadioButtons(
                                        inputId = "color",
                                        label = "Selecione a cor:",
                                        thick = TRUE,
                                        inline = TRUE,
                                        selected = "light",
                                        choices = c("light", "dark"),
                                        animation = "pulse",
                                        status = "info"
                                    )
                                )
                            ),
                            
                            footer = "Configurações"
                        ),
                        
                        
                    ),
                    
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        
                        f7Card(
                            tagList(
                                bs4ValueBoxOutput("vbox")
                            ),
                            
                            footer = NULL
                        ),
                        
                        
                    ),
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7Card(
                            tagList(
                                bs4ValueBoxOutput("vbox_2")
                            ),
                            
                            footer = NULL
                        ),),
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7Card(
                            tagList(
                                bs4ValueBoxOutput("vbox_1")
                            ),
                            
                            footer = NULL
                        ),),
                ),
                f7Tab(
                    tabName = "Gráfico",
                    icon = f7Icon("keyboard"),
                    active = FALSE,
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7Card(
                            title = "Card header",
                            dateRangeInput('dateRange',
                                           label = 'Pesquisa por Data', start = Sys.Date() - 254, end = Sys.Date()-248
                            ),
                            apexchartOutput("scatter")
                        )
                    )
                ),
                f7Tab(
                    tabName = "Tabela",
                    icon = f7Icon("layers_alt"),
                    active = FALSE,
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7Card(
                            title = "Tabela das Recargas Vendidas",
                            f7SmartSelect(
                                "variable",
                                "Resultados apresentados pelo tipo de recarga:",
                                c("Quantidade de Recargas" = "QYT",
                                  "Valor Total de Recargas (MZN)" = "AMOUNT"
                                  ),
                                openIn = "sheet",
                                multiple = TRUE
                            ),
                            tableOutput("data")
                        )
                    )
                )
            )
        )
    ),
    server = function(input, output, session) {
        
        source(file = "server/01_srv_recarregamento.R", local = TRUE)
        
        
        # river plot
         dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
         
         topup_filter_tbl <- reactive({
             
             result <- topup_dat %>% filter(between(CREATED_AT, input$dateRange[1], input$dateRange[2]))
             
             return(result)
             
         })
        
        output$pie <- renderApexchart({
            apex(
                data = poll,
                type = "pie",
                mapping = aes(x = answer, y = n)
            )
        })
        
        output$scatter <- renderApexchart({
            apex(
                data = topup_filter_tbl(),
                type = "scatter",
                mapping = aes(
                    x = CREATED_AT,
                    y = QYT,
                    fill = factor(CREATED_AT)
                )
            )
        })
        
        # datatable
        output$data <- renderTable({
            topup_dat[, c("SEGMENTATION_ID", input$variable), drop = FALSE]
        }, rownames = TRUE)
        
        # send the theme to javascript
        observe({
            session$sendCustomMessage(
                type = "ui-tweak",
                message = list(os = input$theme, skin = input$color)
            )
        })
        
        v2 <- topup_dat %>% group_by(CREATED_AT) %>% summarise(value = sum(QYT)) %>% filter(value==max(value))
        b <-  reactive({ topup_filter_tbl() %>% group_by(CREATED_AT) %>% summarise(value = sum(QYT))%>% filter(value==max(value))})
        v3 <- sum(topup_dat$AMOUNT)
        v4 <- topup_dat %>% group_by(format(CREATED_AT, "%m")) %>% summarise(value = sum(AMOUNT)) %>% filter(value==max(value))
        output$vbox <- renderbs4ValueBox({
            bs4ValueBox(
                value = prettyNum(v2$value, big.mark = ","),
                subtitle = "Quantidade de Recargas Vendidas",
                status = "primary",
                #icon = "shopping-cart",
            )
        })
        
        output$vbox_2 <- renderbs4ValueBox({
            bs4ValueBox(
                value = prettyNum(topup_yearly_tbl$total_amount, big.mark = ","),
                subtitle = "Valor Total de Recargas Vendidas (MZN)",
                status = "primary",
            )
        })
        
        output$vbox_1 <- renderbs4ValueBox({
            bs4ValueBox(
                value = prettyNum(v4$value, big.mark = ","),
                subtitle = "Valor de recargas Mensais (MZN)",
                status = "primary",
            )
        })
        
        
        
    }
)