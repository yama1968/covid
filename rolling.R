
##
library(zoo)

covid.7 <- covid %>%
  group_by(iso3) %>%
  arrange(date_rep) %>%
  mutate(deaths = rollmean(x = deaths, 7, align = "right", fill = 0),
         cases  = rollmean(x = cases, 7,  align = 'right', fill = 0))

countries <- c("FRA", "ITA", "ESP", "DEU", "GBR", "SWE", "BRA", "IND")

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

covid.7 %>%
  filter((iso3 %in% countries) | (iso3 == "USA")) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, cases, data = ., color = iso3, geom = c("line"))

covid.7 %>%
  filter((iso3 %in% c("FRA", "ITA", "ESP", "DEU", "GBR", "SWE"))) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, cases, data = ., color = iso3, geom = c("line"))

covid.7 %>%
  filter((iso3 %in% c("FRA", "ITA", "ESP", "DEU", "GBR", "SWE"))) %>%
  filter(date_rep > ymd("20200615") & cases > 0) %>%
  qplot(date_rep, cases, data = ., color = iso3, geom = c("line"))


cov_curve.7 <- cov_curve %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(deaths = rollmean(x = deaths, 7, align = "right", fill = 0),
         cases  = rollmean(x = cases, 7,  align = 'right', fill = 0))

(cov_curve.7 %>% 
    group_by(iso3) %>% 
    filter(row_number() == n()) %>% 
    ggplot(aes(deaths, cu_deaths, label=iso3))) + 
  geom_text() + 
  scale_x_log10() + 
  scale_y_log10()

(cov_curve.7 %>%
  filter(iso3 %in% countries) %>%
  arrange(date) %>%
  ggplot(mapping = aes(deaths, cu_deaths, 
                       group = factor(iso3), color = factor(iso3)))) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

(cov_curve.7 %>%
    filter(iso3 %in% countries) %>%
    arrange(date) %>%
    ggplot(mapping = aes(cases, cu_cases, 
                         group = factor(iso3), color = factor(iso3)))) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()
