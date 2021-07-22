######################################################
### global.R
######################################################

library(readxl)
library(tidyverse)
library(highcharter)

library(lubridate) # for date



# 1.1 Carregar os dados ---------------------------------------------------

topup_dat <- read.csv(file = "00_data/topup_daily.csv")

topup_dat %>% glimpse()

# converter o tipo de dados
topup_dat$CREATED_AT <- as.Date(topup_dat$CREATED_AT, "%m/%d/%Y")

topup_dat %>% glimpse()


# calcular o montante total de recarregamentos por dia
topup_daily_tbl <- 
    topup_dat %>% 
    group_by(CREATED_AT) %>% 
    summarise(total_amount = sum(AMOUNT))


# montante anual
topup_yearly_tbl <- 
topup_dat %>% 
    group_by(Year = lubridate::year(CREATED_AT)) %>% 
    summarise(total_amount = sum(AMOUNT))


