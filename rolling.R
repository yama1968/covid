
##
library(zoo)

covid.7 <- covid %>%
  group_by(iso3) %>%
  arrange(date_rep) %>%
  mutate(deaths = rollmean(x = deaths, 7, align = "right", fill = NA),
         cases  = rollmean(x = cases, 7,  align = 'right', fill = NA))

countries <- c("FRA", "ITA", "ESP", "DEU", "GBR", "SWE")

covid.7 %>%
  filter(iso3 %in% countries) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, cases, data = ., color = iso3, geom = c("line"))

covid.7 %>%
  filter(iso3 %in% countries) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, deaths, data = ., color = iso3, geom = c("line"))

covid.7 %>%
  filter((iso3 %in% countries) | (iso3 == "USA")) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, deaths, data = ., color = iso3, geom = c("line"))
