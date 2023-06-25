library(rbcb)
library(tidyverse)
library(timetk)
library(PerformanceAnalytics)
library(gtrendsR)
library(rio)
library(quantmod)



# taxa selic 2010-2022 ----------------------------------------------------

selic <- rbcb::get_series(11)

selic <- selic %>% 
  filter(date > as.Date("2012-01-01") & date < as.Date("2023-03-09")) %>% 
  rename(SELIC = `11`)


anual_selic <- selic %>% 
  mutate(SELIC=SELIC/100) %>% 
  mutate(SELIC=(1 + SELIC)^252 - 1) %>% 
  tk_xts() %>% 
  to.yearly(OHLC=FALSE) %>% 
  tk_tbl(rename_index = "Data")


Grafico_selic_anual <- ggplot(anual_selic) +
  aes(x = Data, y = SELIC) +
  geom_line(colour = "gray", se=FALSE) +
  labs(
    y = "SELIC % a.a",
    title = "Serie anual da taxa SELIC 2013-2023") +
  expand_limits(y = c(0, 0.15)) +
  scale_y_continuous(labels = scales::percent)+
  theme_minimal() + 
  ggeasy::easy_center_title()

Grafico_selic_anual
# investidores --------------------------------------------------------------------


investors <- rio::import("dados/investidores_custodia.xlsx")


investidores_custodia <-  ggplot(investors, aes(x = Ano, y = Investidores)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Investidores com posição em custódia 2008-2023",
       x = "Ano",
       y = "Investidores")+
    theme_minimal()+
    ggeasy::easy_adjust_legend(to="center")+
    ggeasy::easy_center_title()


# Trends busca por FII no youtube -----------------------------------------

FII_trend <- gtrends("FII",geo = "BR", time = "2013-01-01 2022-12-31")$interest_over_time

FII_trend_plot <- FII_trend %>% 
  select(date,hits) %>% 
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(total_hits = sum(hits))


FII_trend_ggplot <-  ggplot(FII_trend_plot, aes(x = year, y = total_hits)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Popularidade dos FIIs no Youtube 2013-2023",
       x = "Data",
       y = "Popularidade")+
  theme_minimal()+
  ggeasy::easy_adjust_legend(to="center")+
  ggeasy::easy_center_title()


# Retornos do IFIX --------------------------------------------------------
  
  ifix_prices <- readxl::read_excel("dados/ifix_serie_2013_2019.xlsx",
                                    col_types = c("date", "numeric")) %>%
    tk_xts()
  
  # calcula os retornos diarios
  ifix_returns <- annualReturn(ifix_prices["2012-01-01/2023-03-09"])
  index(ifix_returns) <- as.POSIXct(format(index(ifix_returns), "%Y-%m-%d 00:00:00"))
  
  #chartSeries(ifix_returns)
  
  ifix_monthly_returns <- ggplot() +
    geom_line(data = ifix_returns ,
              aes(x = index(ifix_returns), y = ifix_returns),
              color = "gray") +
    labs(title = "Retornos anuais do IFIX - 2013-2023", x = "Data", y = "Retornos % a.a")+
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    ggeasy::easy_center_title()

# +
#   transition_reveal(index(ifix_returns))

# selic vs ifix -----------------------------------------------------------

# Converter ifix_returns para um data frame
ifix_data <- ifix_returns %>% 
  tk_tbl(rename_index = "Data") 
ifix_data$Data <- format(as.Date(ifix_data$Data), "%m-%Y")
  

# # Filtro de datas
# ifix_data <- ifix_data %>% filter(Data <= as.Date("2022-12")) 
 selic_data <- anual_selic 
 selic_data$Data <- format(as.Date(anual_selic$Data), "%m-%Y")
 
 comparativo <- full_join(ifix_data,selic_data,by=NULL) %>% slice(-12)
 colnames(comparativo) <- c("DATA","IFIX", "SELIC") 
 
# comparativo <- comparativo %>% 
#   pivot_longer(cols = 2:3,
#                values_to = "returns")
# 
# ggplot(data = comparativo, aes(x = DATA, y = returns, color = name)) +
#   geom_line(data = subset(comparativo, name == "IFIX"), linetype = "solid") +
#   geom_line(data = subset(comparativo, name == "SELIC"), linetype = "dashed") +
#   labs(x = "Data", y = "Retornos", color = "Nome") +
#   theme_minimal()
 
 
# Gráfico comparativo com linhas suavizadas e legendas
grafico_comparativo <- ggplot() +
  geom_smooth(data = comparativo, aes(x = DATA, y = IFIX, color = "IFIX"), linetype = "solid", size = 1, se=FALSE) +
  geom_smooth(data = comparativo, aes(x = DATA, y = SELIC, color = "SELIC"), linetype = "dashed", size = 1, se=FALSE) +
  labs(title = "Retornos anuais do IFIX vs Taxa SELIC 2013-2023", x = "Data", y = "Retornos / Taxa % a.a",
       color = "") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("IFIX" = "black", "SELIC" = "darkgray"),
                     labels = c("IFIX", "SELIC")) +
  theme_minimal() +
  ggeasy::easy_center_title() +
  theme(legend.position = "bottom")

grafico_comparativo


