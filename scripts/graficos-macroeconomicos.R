library(rbcb)
library(tidyverse)
library(timetk)
library(PerformanceAnalytics)



# taxa selic 2010-2022 ----------------------------------------------------

selic <- rbcb::get_series(11)

selic <- selic %>% 
  filter(date > as.Date("2011-12-31") & date < as.Date("2023-01-01")) %>% 
  rename(SELIC = `11`)


anual_selic <- selic %>% 
  mutate(SELIC=SELIC/100) %>% 
  mutate(SELIC=(1 + SELIC)^252 - 1) %>% 
  tk_xts() %>% 
  to.yearly(OHLC=FALSE) %>% 
  tk_tbl(rename_index = "Data")


Grafico_selic_anual <- ggplot(anual_selic) +
  aes(x = Data, y = SELIC) +
  geom_line(colour = "gray") +
  labs(
    y = "SELIC % a.a",
    title = "Serie histórica da taxa SELIC 2010-2022") +
  expand_limits(y = c(0, 0.15)) +
  scale_y_continuous(labels = scales::percent)+
  theme_minimal() + 
  ggeasy::easy_center_title()


# investidores --------------------------------------------------------------------


investors <- rio::import("dados/investidores_custodia.xlsx")


investidores_custodia <-  ggplot(investors, aes(x = Ano, y = Investidores)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Número de Investidores por Ano com posição em custódia",
       x = "Ano",
       y = "Investidores")+
    ggeasy::easy_adjust_legend(to="center")+
    ggeasy::easy_center_title()
