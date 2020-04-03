

covid.smooth <- cov_curve %>%
  group_by(iso3) %>%
  arrange(desc(date)) %>%
  mutate(deaths = (deaths + lead(deaths, 1) * 0.5 + lead(deaths, 2) * 0.25) / 1.75,
         cases =  (cases  + lead(cases, 1)  * 0.5 + lead(cases, 2)  * 0.25) / 1.75)

last_death <- cov_curve %>% group_by(iso3) %>%
  arrange(date) %>% 
  mutate(cum.deaths = cumsum(deaths)) %>%
  filter(row_number() == n()) 

ante_last_death <- cov_curve %>% group_by(iso3) %>%
  arrange(date) %>% 
  mutate(cum.deaths = cumsum(deaths)) %>%
  filter(row_number() == n() - 1) 

max_deaths <- cov_curve %>% group_by(iso3) %>% 
  arrange(deaths) %>% 
  filter(row_number() == n()) %>% 
  select(date, iso3, deaths)



last_death %>%
  inner_join(max_deaths, by = "iso3", suffix = c("_last", "_max")) %>%
  arrange(desc(deaths_last))

country_deaths <- function(country_iso3 = "FRA", covid = covid)
  covid %>% filter(iso3 == country_iso3) %>%
    arrange(desc(date_rep))

fra <- country_deaths("FRA", covid)
fra

fra %>%
  arrange(date_rep) %>%
  filter(row_number() > 60) %>%
  qplot(date_rep, deaths, data = ., geom = "line")

fra %>%
  arrange(date_rep) %>%
  filter(row_number() > 60) %>%
  qplot(date_rep, cases, data = ., geom = "line")

ita <- country_deaths("ITA", covid)
ita

ita %>%
  arrange(date_rep) %>%
  filter(row_number() > 60) %>%
  qplot(date_rep, deaths, data = ., geom = "line")

covid %>%
  filter(iso3 %in% c("FRA", "ITA", "ESP", "DEU", "GBR")) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, cases, data = ., color = iso3, geom = c("line", "smooth"))


covid %>%
  filter(iso3 %in% c("FRA", "ITA", "ESP", "DEU", "GBR")) %>%
  filter(date_rep > ymd("20200215")) %>%
  qplot(date_rep, deaths, data = ., color = iso3, geom = c("line", "smooth"))


cov_curve %>%
  group_by(iso3) %>%
  filter(row_number() == n()) %>%
  arrange(desc(cu_deaths)) %>%
  View()

cov_curve %>%
  filter(iso3 %in% focus_cn) %>% ## focus on just a few countries, defined above
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_deaths, 
                       color = cname, label = end_label, 
                       group = cname)) + 
  geom_line(size = 0.8) + 
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1, 
                  segment.color = NA) + 
  guides(color = FALSE) + 
  scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                     breaks = c(32, 64, 128, 256, 512, 1024, 2048),
                     trans = "log2") + 
  labs(x = "Days Since 10th Confirmed Death", 
       y = "Cumulative Number of Deaths (log2 scale)", 
       title = "Cumulative Deaths from COVID-19, Selected Countries", 
       subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: ECDC") + 
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(2)),
        legend.text = element_text(size = rel(2))
  )


cov_curve %>%
  filter(iso3 %in% focus_cn) %>% ## focus on just a few countries, defined above
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases, 
                       color = cname, label = end_label, 
                       group = cname)) + 
  geom_line(size = 0.8) + 
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1, 
                  segment.color = NA) + 
  guides(color = FALSE) + 
  scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                     breaks = c(32, 64, 128, 256, 512, 1024, 2048),
                     trans = "log2") + 
  labs(x = "Days Since 10th Confirmed Death", 
       y = "Cumulative Number of Cases (log2 scale)", 
       title = "Cumulative Casess from COVID-19, Selected Countries", 
       subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: ECDC") + 
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(2)),
        legend.text = element_text(size = rel(2))
  )


cov_curve %>%
  filter(iso3 %in% focus_cn) %>% ## focus on just a few countries, defined above
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = deaths, 
                       color = cname, label = end_label, 
                       group = cname)) + 
  geom_line(size = 0.8) + 
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1, 
                  segment.color = NA) + 
  guides(color = FALSE) + 
  scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                     breaks = c(32, 64, 128, 256, 512, 1024, 2048)) + 
  labs(x = "Days Since 10th Confirmed Death", 
       y = "Number of Deaths", 
       title = "Deaths from COVID-19, Selected Countries", 
       subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: ECDC") + 
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(2)),
        legend.text = element_text(size = rel(2))
  )


cov_curve %>%
  filter(iso3 %in% focus_cn) %>% ## focus on just a few countries, defined above
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cases, 
                       color = cname, label = end_label, 
                       group = cname)) + 
  geom_line(size = 0.8) + 
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1, 
                  segment.color = NA) + 
  guides(color = FALSE) + 
  scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                     breaks = c(32, 64, 128, 256, 512, 1024, 2048)) + 
  labs(x = "Days Since 10th Confirmed Death", 
       y = "Number of Cases", 
       title = "Cases from COVID-19, Selected Countries", 
       subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: ECDC") + 
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(2)),
        legend.text = element_text(size = rel(2))
  )
