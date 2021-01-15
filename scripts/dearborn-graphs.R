library(tidyverse)
library(lubridate)
library(bbplot)

full_dataset <- read_csv("processed/data/full_dataset.csv") %>% 
  filter(str_detect(city,"^Dearborn")) %>% 
  mutate(month = month(date), year = year(date)) %>% 
  filter(year == 2020 & type == "naloxone" & month %in% 0:8)

full_dataset %>% 
  ggplot()+
  geom_bar(aes(x = month), fill = "#4682B4")+
  bbc_style()+
  labs(title = "Dearborn EMS Naloxone Administrations",
       caption = "Source: Michigan EMS Information System (MI-EMSIS)")+
  scale_x_continuous(breaks = 1:8,labels=c("Jan","Feb","Mar",
                                  "Apr","May","Jun",
                                  "Jul","Aug"))+
  theme(plot.title = element_text(size = 26),
        plot.caption = element_text(size = 12))
