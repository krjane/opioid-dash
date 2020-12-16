library(tidyverse)
library(lubridate)
library(bbplot)

#Cumulative n's
sum_dat <- full_dataset %>% 
  filter(city %in% c("Plymouth","Charter Township Of Canton","Canton","Canton Twp"), type == "naloxone") %>% 
  mutate(year = year(date)) %>% 
  mutate(gg_date = as.POSIXct(paste0("2020-", format(date,format = "%m-%d")))) %>% 
  group_by(year) %>% 
  count(gg_date) %>% 
  mutate(sum = cumsum(n))


#End point n's
sum_point <- sum_dat %>% 
  group_by(year) %>% 
  top_n(1)

#>10 in a year
listed_years <- sum_point %>% 
  filter(sum > 10)

#sum_dat updated
sum_dat <- sum_dat %>% 
  filter(year %in% listed_years$year)

#create "current_year"
current_year <- max(sum_dat$year)

#plot
ggplot() + 
  geom_smooth(data = sum_dat %>% filter(year == current_year), 
              aes(x= gg_date, 
                  y = sum, 
                  group = factor(year)),
              linetype = 0,
              method="gam",
              fullrange=TRUE,
              level  = .99999999999999,
              fill = "#7ba7cc")+
  
  geom_smooth(data = sum_dat, 
              aes(x= gg_date, 
                  y = sum, 
                  color = factor(year)), method = "gam",
              size = 3)+
  
  geom_point(data = sum_point,
             aes(x = gg_date,
                 y = sum,
                 color = factor(year)),
             size = 7)+
  
  geom_label(data = sum_point %>%filter(year == "2020"), 
             aes(x= gg_date-10, y = sum * 1.3, label = paste("...",sum,"times.")),
             fill = NA, 
             label.size = NA,
             family = "Helvetica",
             size = 6)+
  
  scale_x_datetime(date_labels = "%b", 
                   breaks = seq(as.POSIXct("2020-01-15"), 
                                as.POSIXct("2020-12-15"), by = "1 month"))+
  scale_color_manual(values = c("#D3D3D3","#C0C0C0", "#4682B4"))+
  bbc_style()+ 
  scale_y_continuous(position = "right")+
  labs(title = paste0("Plymouth/Canton EMS has administered narcan..."),
       caption = "Source: Michigan EMS Information System (MI-EMSIS)")+
  theme(axis.ticks.x = element_line(colour = "#cbcbcb"), 
        axis.ticks.length =  unit(0.26, "cm"),
        axis.text = element_text(size = 18),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 26),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        plot.caption = element_text(size = 12))+
  guides(color=guide_legend(override.aes=list(fill=NA)))
