

cov_curve %>% 
  filter(iso3 == 'FRA' ) %>% 
  filter(deaths < 1000) %>%
  qplot(date, deaths, data=., 
        geom = c("line", "smooth"))

cov_curve %>% 
  filter(iso3 == 'FRA' ) %>% 
  qplot(date, deaths, data=., 
        geom = c("line", "smooth"))

cov_curve %>% 
  filter(iso3 == 'FRA' ) %>% 
  qplot(date, cases, data=., 
        geom = c("line", "smooth"))

cov_curve %>% 
  filter(iso3 == 'FRA' ) %>% 
  filter(deaths < 1000) %>%
  mutate(after = (date > ymd("200321"))) %>%
  qplot(date, deaths, data=., 
        geom = c("line", "smooth"),
        color = after)

cov_curve %>% 
  filter(iso3 == 'FRA' ) %>% 
  mutate(after = (date > ymd("200328"))) %>%
  qplot(date, cases, data=., 
        geom = c("line"),
        color = after) +
  geom_smooth(method = lm)

cov_curve %>% 
  filter(iso3 == 'USA' ) %>% 
  qplot(date, deaths, data=., 
        geom = c("line", "smooth"))+
  geom_smooth(method = lm)

cov_curve %>% 
  filter(iso3 == 'USA' ) %>% 
  qplot(date, cases, data=., 
        geom = c("line", "smooth"))+
  geom_smooth(method = lm)
